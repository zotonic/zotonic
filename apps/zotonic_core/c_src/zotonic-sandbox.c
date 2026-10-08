/* Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0 */

/* A non-setuid launcher. All policy is installed before executing untrusted
 * decoders. Exit 125 means setup failed; never retry without confinement.
 *
 * Usage:
 *   zotonic-sandbox --check
 *       Install the sandbox in this process and exit 0 if that works. Intended
 *       for startup capability probing, not for running anything.
 *
 *   zotonic-sandbox [OPTION VALUE]... -- COMMAND [ARGS...]
 *       --read PATH        allow reading PATH (file or directory tree)
 *       --write PATH       allow reading and writing PATH (file or directory tree)
 *       --execute PATH     allow executing PATH (and its ELF loader), plus reading it
 *       --cpu SECONDS      RLIMIT_CPU
 *       --memory BYTES     RLIMIT_AS (Linux only; ignored on macOS)
 *       --file-size BYTES  RLIMIT_FSIZE
 *
 * Exit codes:
 *   125  setup failed (SANDBOX_EXIT_SETUP_FAILED)
 *   78   platform lacks a required feature, e.g. Landlock ABI < 3
 *        (SANDBOX_EXIT_UNSUPPORTED)
 *   else the exit status of COMMAND, or 128 + signal number if it was killed.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __linux__
#include <elf.h>
#include <limits.h>
#include <linux/landlock.h>
#include <seccomp.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#if SCMP_VER_MAJOR < 2 || (SCMP_VER_MAJOR == 2 && SCMP_VER_MINOR < 5)
#error "libseccomp 2.5 or newer is required"
#endif
#elif defined(__APPLE__)
#include <sandbox.h>
#endif

/*
 * Constants and types
 */

#define SANDBOX_EXIT_SETUP_FAILED 125
#define SANDBOX_EXIT_UNSUPPORTED   78

#define NOFILE_LIMIT 256

enum launch_mode { MODE_CHECK, MODE_RUN };

enum access_kind { ACCESS_READ, ACCESS_WRITE, ACCESS_EXECUTE };

enum option_kind {
    OPT_READ, OPT_WRITE, OPT_EXECUTE,
    OPT_CPU, OPT_MEMORY, OPT_FILE_SIZE
};

static const struct {
    const char *name;
    enum option_kind kind;
} option_table[] = {
    { "--read",      OPT_READ      },
    { "--write",     OPT_WRITE     },
    { "--execute",   OPT_EXECUTE   },
    { "--cpu",       OPT_CPU       },
    { "--memory",    OPT_MEMORY    },
    { "--file-size", OPT_FILE_SIZE },
};

/*
 * Error handling
 */

/* Uses _exit(): stderr is unbuffered, and this is also reached in the forked
 * child, where atexit handlers and inherited stdio buffers must not run. */
static void
fail(const char *what) {
    fprintf(stderr, "zotonic-sandbox: %s: %s\n", what, strerror(errno));
    _exit(SANDBOX_EXIT_SETUP_FAILED);
}

static void
usage_error(const char *what) {
    errno = EINVAL;
    fail(what);
}

static char *
resolve_path(const char *path) {
    char *resolved = realpath(path, NULL);

    if (!resolved) {
        fail(path);
    }
    return resolved;
}

/*
 * Resource limits
 */

static rlim_t
parse_limit(const char *value) {
    char *end;

    errno = 0;
    unsigned long long n = strtoull(value, &end, 10);
    bool valid = !errno && *value && !*end && value[0] != '-' && n != 0 &&
                 (unsigned long long)(rlim_t)n == n;
    if (!valid) {
        usage_error("invalid resource limit");
    }
    return (rlim_t)n;
}

static void
set_limit(int resource, rlim_t value) {
    struct rlimit r = { value, value };

    if (setrlimit(resource, &r)) {
        fail("setrlimit");
    }
}

static void
set_limit_from_arg(int resource, const char *value) {
    set_limit(resource, parse_limit(value));
}

/*
 * Descriptor hygiene (shared)
 */

static void
close_fds_from(int first) {
#if defined(__linux__) && defined(SYS_close_range)
    if (syscall(SYS_close_range, (unsigned)first, ~0U, 0) == 0) {
        return;
    }
#endif
    long maxfd = sysconf(_SC_OPEN_MAX);
    if (maxfd < 0) {
        fail("descriptor limit");
    }
    for (long fd = first; fd < maxfd; fd++) {
        close((int)fd);
    }
}

/*
 * Platform layer. Each platform implements the same small interface:
 *
 *   sb_init()                 prepare an empty policy
 *   sb_allow(kind, path)      add a grant; path is already resolved
 *   sb_apply_memory_limit(v)  apply --memory
 *   sb_prepare_fds()          close everything except stdio + policy state
 *   sb_enforce()              irrevocably confine the calling process
 *   sb_discard_in_parent()    release policy state the supervisor won't use
 */

#if defined(__linux__)

/* Landlock ABI 3 handles every FS right up to and including TRUNCATE
 * (bits 0-14). Handling only read rights would leave writes unrestricted. */
#define LANDLOCK_ABI3_FS_MASK ((LANDLOCK_ACCESS_FS_TRUNCATE << 1) - 1)
#define LANDLOCK_MIN_ABI 3

/* The ruleset lives at a fixed descriptor so that everything above it can be
 * closed before the child enforces the policy. */
#define RULESET_FD 3

#ifndef LANDLOCK_ACCESS_FS_REFER
#define LANDLOCK_ACCESS_FS_REFER (1ULL << 13)
#endif
#ifndef LANDLOCK_ACCESS_FS_TRUNCATE
#define LANDLOCK_ACCESS_FS_TRUNCATE (1ULL << 14)
#endif
#ifndef LANDLOCK_ACCESS_FS_IOCTL_DEV
#define LANDLOCK_ACCESS_FS_IOCTL_DEV (1ULL << 15)
#endif

static struct {
    int ruleset_fd;
    uint64_t handled;
} ll = { -1, 0 };

/*
 * Landlock syscall wrappers
 */

static int
ll_create_ruleset(const struct landlock_ruleset_attr *attr,
                  size_t size, uint32_t flags) {
    return (int)syscall(SYS_landlock_create_ruleset, attr, size, flags);
}

static int
ll_add_rule(const struct landlock_path_beneath_attr *rule) {
    return (int)syscall(SYS_landlock_add_rule, ll.ruleset_fd,
                        LANDLOCK_RULE_PATH_BENEATH, rule, 0);
}

static int
ll_restrict_self(void) {
    return (int)syscall(SYS_landlock_restrict_self, ll.ruleset_fd, 0);
}

/* Landlock rule helpers */

/* One resolution: open O_PATH once and take the type from the descriptor, so
 * there is no window between checking the type and installing the rule. */
static int
open_path_fd(const char *path, struct stat *st) {
    int fd = open(path, O_PATH | O_CLOEXEC);

    if (fd < 0) {
        fail(path);
    }
    if (fstat(fd, st)) {
        fail(path);
    }
    return fd;
}

static void
landlock_add(int fd, uint64_t rights) {
    struct landlock_path_beneath_attr rule = {
        .allowed_access = rights, .parent_fd = fd
    };

    if (ll_add_rule(&rule)) {
        fail("landlock_add_rule");
    }
}

/* Landlock rejects directory-only rights on regular files, so the rights
 * depend on the file type. Writing implies reading on every platform. */
static uint64_t
landlock_rights(enum access_kind kind, bool is_dir) {
    uint64_t rights = 0;

    switch (kind) {
    case ACCESS_READ:
        rights = LANDLOCK_ACCESS_FS_READ_FILE;
        if (is_dir) {
            rights |= LANDLOCK_ACCESS_FS_READ_DIR;
        }
        break;
    case ACCESS_WRITE:
        rights = LANDLOCK_ACCESS_FS_READ_FILE | LANDLOCK_ACCESS_FS_WRITE_FILE |
                 LANDLOCK_ACCESS_FS_TRUNCATE;
        if (is_dir) {
            rights |= LANDLOCK_ACCESS_FS_READ_DIR | LANDLOCK_ACCESS_FS_REMOVE_DIR |
                      LANDLOCK_ACCESS_FS_REMOVE_FILE | LANDLOCK_ACCESS_FS_MAKE_DIR |
                      LANDLOCK_ACCESS_FS_MAKE_REG | LANDLOCK_ACCESS_FS_REFER;
        }
        break;
    case ACCESS_EXECUTE:
        rights = LANDLOCK_ACCESS_FS_EXECUTE | LANDLOCK_ACCESS_FS_READ_FILE;
        break;
    }
    return rights;
}

/* ELF interpreter discovery
 *
 * Read PT_INTERP without executing the tool (unlike ldd). Shared libraries
 * only need read access; the kernel also requires EXECUTE on this exact
 * loader. Both ELF classes are parsed, but only the host byte order:
 * foreign-endian tools cannot run natively and must not silently receive
 * broader permissions.
 */

struct elf_layout {
    int elf_class;
    uint64_t phoff, phentsize, phnum;
};

struct elf_segment {
    uint32_t type;
    uint64_t offset, filesz;
};

static void
bad_elf(const char *what) {
    errno = ENOEXEC;
    fail(what);
}

static void
read_at(int fd, void *buf, size_t size, off_t offset) {
    if (pread(fd, buf, size, offset) != (ssize_t)size) {
        bad_elf("read ELF header");
    }
}

static int
host_elf_encoding(void) {
    const uint16_t probe = 1;

    return *(const unsigned char *)&probe ? ELFDATA2LSB : ELFDATA2MSB;
}

static struct elf_layout
read_elf_layout(int fd, int elf_class) {
    struct elf_layout l = { .elf_class = elf_class };

    if (elf_class == ELFCLASS64) {
        Elf64_Ehdr h;
        read_at(fd, &h, sizeof(h), 0);
        l.phoff = h.e_phoff; l.phentsize = h.e_phentsize; l.phnum = h.e_phnum;
        if (l.phentsize != sizeof(Elf64_Phdr)) {
            bad_elf("ELF phentsize");
        }
    } else if (elf_class == ELFCLASS32) {
        Elf32_Ehdr h;
        read_at(fd, &h, sizeof(h), 0);
        l.phoff = h.e_phoff; l.phentsize = h.e_phentsize; l.phnum = h.e_phnum;
        if (l.phentsize != sizeof(Elf32_Phdr)) {
            bad_elf("ELF phentsize");
        }
    } else {
        bad_elf("unsupported ELF class");
    }
    return l;
}

static struct elf_segment
read_elf_segment(int fd, const struct elf_layout *l,
                 uint64_t index) {
    off_t at = (off_t)(l->phoff + index * l->phentsize);
    struct elf_segment s;

    if (l->elf_class == ELFCLASS64) {
        Elf64_Phdr p;
        read_at(fd, &p, sizeof(p), at);
        s.type = p.p_type; s.offset = p.p_offset; s.filesz = p.p_filesz;
    } else {
        Elf32_Phdr p;
        read_at(fd, &p, sizeof(p), at);
        s.type = p.p_type; s.offset = p.p_offset; s.filesz = p.p_filesz;
    }
    return s;
}

/*
 * Returns true and fills `loader` if the file is an ELF with a PT_INTERP.
 * Non-ELF files return false: script interpreters must be granted explicitly
 * in the profile.
 */
static bool
elf_interpreter(int fd, char *loader, size_t capacity) {
    unsigned char ident[EI_NIDENT];
    ssize_t n = pread(fd, ident, sizeof(ident), 0);

    if (n < 0) {
        fail("read executable");
    }
    if (n < SELFMAG || memcmp(ident, ELFMAG, SELFMAG)) {
        return false;
    }
    if (n != (ssize_t)sizeof(ident) || ident[EI_DATA] != host_elf_encoding()) {
        bad_elf("unsupported ELF encoding");
    }

    struct elf_layout layout = read_elf_layout(fd, ident[EI_CLASS]);
    struct stat st;
    if (fstat(fd, &st)) {
        fail("stat ELF");
    }
    uint64_t file_size = (uint64_t)st.st_size;
    if (layout.phnum == PN_XNUM || layout.phoff > file_size ||
        layout.phnum * layout.phentsize > file_size - layout.phoff) {
        bad_elf("invalid ELF program headers");
    }

    for (uint64_t i = 0; i < layout.phnum; i++) {
        struct elf_segment seg = read_elf_segment(fd, &layout, i);
        if (seg.type != PT_INTERP) {
            continue;
        }

        if (seg.filesz < 2 || seg.filesz > capacity || seg.offset > file_size ||
            seg.filesz > file_size - seg.offset) {
            bad_elf("invalid ELF interpreter");
        }
        read_at(fd, loader, seg.filesz, (off_t)seg.offset);
        if (loader[0] != '/' || loader[seg.filesz - 1] != '\0' ||
            strlen(loader) != seg.filesz - 1) {
            bad_elf("invalid ELF interpreter path");
        }
        return true;
    }
    return false;
}

static void
allow_elf_interpreter(const char *executable) {
    int fd = open(executable, O_RDONLY | O_CLOEXEC);

    if (fd < 0) {
        fail(executable);
    }
    char loader[PATH_MAX];
    bool found = elf_interpreter(fd, loader, sizeof(loader));
    close(fd);
    if (!found) {
        return;
    }

    char *resolved = resolve_path(loader);
    struct stat st;
    int loader_fd = open_path_fd(resolved, &st);
    if (!S_ISREG(st.st_mode)) {
        bad_elf("ELF interpreter is not a regular file");
    }
    landlock_add(loader_fd, landlock_rights(ACCESS_EXECUTE, false));
    close(loader_fd);
    free(resolved);
}

/*
 * seccomp
 */

/* This is a denylist, not a general syscall allowlist. Names that do not
 * exist on the build architecture are skipped. */
static const char *const denied_syscalls[] = {
    /* Network and sockets, including io_uring as an alternate path. */
    "socket", "socketpair", "connect", "bind", "listen", "accept", "accept4",
    "sendto", "sendmsg", "sendmmsg", "recvfrom", "recvmsg", "recvmmsg",
    "io_uring_setup", "io_uring_enter", "io_uring_register",

    /* Touching other processes, signals, and escaping the process group. */
    "ptrace", "process_vm_readv", "process_vm_writev", "pidfd_getfd",
    "pidfd_send_signal", "kill", "tkill", "tgkill", "rt_sigqueueinfo",
    "rt_tgsigqueueinfo", "setsid", "setpgid", "process_madvise",
    "process_mrelease",

    /* Namespaces, mounts, privileged kernel interfaces, and module loading. */
    "unshare", "setns", "mount", "umount2", "pivot_root", "chroot", "bpf",
    "perf_event_open", "userfaultfd", "keyctl", "add_key", "request_key",
    "open_by_handle_at", "name_to_handle_at", "reboot",
    "kexec_load", "kexec_file_load", "init_module", "finit_module",
    "delete_module",

    /* File metadata changes: permissions, ownership, timestamps, xattrs. */
    "chmod", "fchmod", "fchmodat", "fchmodat2",
    "chown", "fchown", "lchown", "fchownat",
    "utime", "utimes", "futimesat", "utimensat",
    "setxattr", "lsetxattr", "fsetxattr",
    "removexattr", "lremovexattr", "fremovexattr",

    /* System V IPC. */
    "shmget", "shmat", "shmctl", "semget", "semop", "semtimedop", "semctl",
    "msgget", "msgsnd", "msgrcv", "msgctl",
};

static void
seccomp_check(int rc, const char *what) {
    if (rc) {
        errno = -rc; /* libseccomp returns a negative errno */
        fail(what);
    }
}

static void
seccomp_deny(scmp_filter_ctx ctx, const char *name, uint32_t action) {
    int nr = seccomp_syscall_resolve_name(name);

    if (nr == __NR_SCMP_ERROR) {
        return;
    }
    seccomp_check(seccomp_rule_add(ctx, action, nr, 0), "seccomp_rule_add");
}

static void
install_seccomp_denylist(void) {
    scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_ALLOW);

    if (!ctx) {
        fail("seccomp_init");
    }

    for (size_t i = 0; i < sizeof(denied_syscalls) / sizeof(denied_syscalls[0]); i++) {
        seccomp_deny(ctx, denied_syscalls[i], SCMP_ACT_ERRNO(EPERM));
    }

    /* libc uses prlimit64(0, ...) for getrlimit/setrlimit. Permit that
     * self-only form, including in descendants, but never target another
     * process: matching UIDs otherwise allow changing the server's limits. */
    seccomp_check(seccomp_rule_add(ctx, SCMP_ACT_ERRNO(EPERM), SCMP_SYS(prlimit64),
                                   1, SCMP_A0(SCMP_CMP_NE, 0)),
                  "seccomp prlimit64");

    /* clone3 has opaque arguments: force libc's legacy clone fallback. */
    seccomp_deny(ctx, "clone3", SCMP_ACT_ERRNO(ENOSYS));

    seccomp_check(seccomp_load(ctx), "seccomp_load");
    seccomp_release(ctx);
}

/*
 * Platform interface
 */

static void
sb_init(void) {
    int abi = ll_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION);

    bool too_old = abi >= 0 && abi < LANDLOCK_MIN_ABI;
    bool missing = abi < 0 && (errno == ENOSYS || errno == EOPNOTSUPP);
    if (too_old || missing) {
        fprintf(stderr, "zotonic-sandbox: Landlock ABI %d or newer is required\n",
                LANDLOCK_MIN_ABI);
        _exit(SANDBOX_EXIT_UNSUPPORTED);
    }
    if (abi < 0) {
        fail("Landlock ABI probe");
    }

    ll.handled = LANDLOCK_ABI3_FS_MASK;
    if (abi >= 5) {
        ll.handled |= LANDLOCK_ACCESS_FS_IOCTL_DEV;
    }

    struct landlock_ruleset_attr attr = { .handled_access_fs = ll.handled };
    ll.ruleset_fd = ll_create_ruleset(&attr, sizeof(attr), 0);
    if (ll.ruleset_fd < 0) {
        fail("landlock_create_ruleset");
    }
}

static void
sb_allow(enum access_kind kind, const char *path) {
    struct stat st;
    int fd = open_path_fd(path, &st);

    landlock_add(fd, landlock_rights(kind, S_ISDIR(st.st_mode)));
    close(fd);

    if (kind == ACCESS_EXECUTE && S_ISREG(st.st_mode)) {
        allow_elf_interpreter(path);
    }
}

static void
sb_apply_memory_limit(const char *value) {
    set_limit_from_arg(RLIMIT_AS, value);
}

static void
sb_prepare_fds(void) {
    if (ll.ruleset_fd != RULESET_FD) {
        if (dup3(ll.ruleset_fd, RULESET_FD, O_CLOEXEC) < 0) {
            fail("dup3 ruleset");
        }
        close(ll.ruleset_fd);
        ll.ruleset_fd = RULESET_FD;
    }
    close_fds_from(RULESET_FD + 1);
}

static void
sb_enforce(void) {
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        fail("no_new_privs");
    }
    if (ll_restrict_self()) {
        fail("landlock_restrict_self");
    }
    close(ll.ruleset_fd);
    ll.ruleset_fd = -1;
    install_seccomp_denylist();
}

static void
sb_discard_in_parent(void) {
    close(ll.ruleset_fd);
    ll.ruleset_fd = -1;
}

#elif defined(__APPLE__)

static struct {
    FILE *stream;
    char *text;
    size_t size;
} seatbelt;

static void
write_quoted_path(const char *path) {
    fputc('"', seatbelt.stream);
    for (const unsigned char *p = (const unsigned char *)path; *p; p++) {
        if (*p < 32 || *p == 127) {
            usage_error("control character in path");
        }
        if (*p == '"' || *p == '\\') {
            fputc('\\', seatbelt.stream);
        }
        fputc(*p, seatbelt.stream);
    }
    fputc('"', seatbelt.stream);
}

static const char *
seatbelt_operations(enum access_kind kind) {
    switch (kind) {
    case ACCESS_READ:    return "file-read*";
    case ACCESS_WRITE:   return "file-read* file-write*";
    case ACCESS_EXECUTE: return "file-read* process-exec";
    }
    abort();
}

static void
sb_init(void) {
    seatbelt.stream = open_memstream(&seatbelt.text, &seatbelt.size);
    if (!seatbelt.stream) {
        fail("open_memstream");
    }

    fputs("(version 1)\n(deny default)\n"
          "(allow file-read-metadata)\n"
          /* dyld needs to open the root directory during process startup.
           * A literal grant permits listing /, but not reading its descendants. */
          "(allow file-read-data (literal \"/\"))\n"
          "(allow process-fork)\n"
          "(allow signal (target self))\n"
          "(allow sysctl-read)\n", seatbelt.stream);
}

static void
sb_allow(enum access_kind kind, const char *path) {
    struct stat st;

    if (stat(path, &st)) {
        fail(path);
    }

    fprintf(seatbelt.stream, "(allow %s (%s ", seatbelt_operations(kind),
            S_ISDIR(st.st_mode) ? "subpath" : "literal");
    write_quoted_path(path);
    fputs("))\n", seatbelt.stream);
}

static void
sb_apply_memory_limit(const char *value) {
    /* macOS does not support a useful RLIMIT_AS for these tools. */
    (void)value;
}

static void
sb_prepare_fds(void) {
    close_fds_from(3);
}

static void
sb_enforce(void) {
    if (fclose(seatbelt.stream)) {
        fail("sandbox profile");
    }
    char *error = NULL;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    if (sandbox_init(seatbelt.text, 0, &error)) {
        fprintf(stderr, "zotonic-sandbox: Seatbelt: %s\n", error ? error : "failed");
        _exit(SANDBOX_EXIT_SETUP_FAILED);
    }
#pragma clang diagnostic pop
    free(seatbelt.text);
}

static void
sb_discard_in_parent(void) {
}

#else /* unsupported platform */

static void
sb_init(void) {
    errno = ENOTSUP;
    fail("unsupported sandbox platform");
}
static void
sb_allow(enum access_kind kind, const char *path) { (void)kind; (void)path; }
static void
sb_apply_memory_limit(const char *value) { (void)value; }
static void
sb_prepare_fds(void) {}
static void
sb_enforce(void) {}
static void
sb_discard_in_parent(void) {}

#endif

/*
 * Policy construction
 */

static void
grant_path(enum access_kind kind, const char *path) {
    char *resolved = resolve_path(path);

    sb_allow(kind, resolved);
    free(resolved);
}

/*
 * Argument parsing
 */

static enum launch_mode
detect_mode(int argc, char **argv) {
    if (argc == 2 && !strcmp(argv[1], "--check")) {
        return MODE_CHECK;
    }
    return MODE_RUN;
}

static bool
lookup_option(const char *arg, enum option_kind *kind) {
    for (size_t i = 0; i < sizeof(option_table) / sizeof(option_table[0]); i++) {
        if (!strcmp(arg, option_table[i].name)) {
            *kind = option_table[i].kind;
            return true;
        }
    }
    return false;
}

static void
apply_option(enum option_kind kind, const char *value) {
    switch (kind) {
    case OPT_READ:      grant_path(ACCESS_READ, value);               break;
    case OPT_WRITE:     grant_path(ACCESS_WRITE, value);              break;
    case OPT_EXECUTE:   grant_path(ACCESS_EXECUTE, value);            break;
    case OPT_CPU:       set_limit_from_arg(RLIMIT_CPU, value);        break;
    case OPT_MEMORY:    sb_apply_memory_limit(value);                 break;
    case OPT_FILE_SIZE: set_limit_from_arg(RLIMIT_FSIZE, value);      break;
    }
}

/* Applies every "OPTION VALUE" pair up to "--" and returns the command. */
static char **
parse_options(int argc, char **argv) {
    int i = 1;

    while (i < argc && strcmp(argv[i], "--")) {
        enum option_kind kind;
        if (i + 1 >= argc) {
            usage_error("missing option value");
        }
        if (!lookup_option(argv[i], &kind)) {
            usage_error("unknown option");
        }
        apply_option(kind, argv[i + 1]);
        i += 2;
    }
    if (i >= argc) {
        usage_error("missing '--' separator");
    }
    if (i + 1 >= argc) {
        usage_error("missing command");
    }
    return &argv[i + 1];
}

/*
 * Supervisor
 */

/*
 * The supervisor stays outside the policy and owns the job's process group.
 * It kills every delegate, including children ignoring SIGTERM, on success,
 * failure or cancellation. Linux seccomp prevents process-group escape;
 * macOS Seatbelt does not, so descendant cleanup is best effort there.
 */
static volatile sig_atomic_t child_pid;

static void
cancel_job(int sig) {
    if (child_pid > 0) {
        kill(-(pid_t)child_pid, SIGKILL);
    }
    _exit(128 + sig);
}

static void
block_cancel_signals(sigset_t *previous) {
    sigset_t blocked;

    sigemptyset(&blocked);
    sigaddset(&blocked, SIGTERM);
    sigaddset(&blocked, SIGINT);
    sigaddset(&blocked, SIGHUP);
    if (sigprocmask(SIG_BLOCK, &blocked, previous)) {
        fail("sigprocmask");
    }
}

static void
restore_signal_mask(const sigset_t *mask) {
    if (sigprocmask(SIG_SETMASK, mask, NULL)) {
        fail("sigprocmask");
    }
}

static void
install_cancel_handlers(void) {
    struct sigaction action;

    memset(&action, 0, sizeof(action));
    action.sa_handler = cancel_job;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGTERM, &action, NULL) || sigaction(SIGINT, &action, NULL) ||
        sigaction(SIGHUP, &action, NULL)) {
        fail("sigaction");
    }
}

/* Forks; the child joins its own process group, enforces the policy and execs.
 * The parent records the pid and also sets the group. */
static pid_t
spawn_confined(char **command, const sigset_t *previous_mask) {
    pid_t pid = fork();

    if (pid < 0) {
        fail("fork");
    }
    if (pid == 0) {
        if (setpgid(0, 0)) {
            fail("child process group");
        }
        restore_signal_mask(previous_mask);
        sb_enforce();
        execv(command[0], command);
        fail("execv");
    }
    child_pid = pid;
    /* The child also sets its group before exec; EACCES means it won the race. */
    if (setpgid(pid, pid) && errno != EACCES && errno != ESRCH) {
        kill(pid, SIGKILL);
        fail("parent process group");
    }
    return pid;
}

/* WNOWAIT holds the leader's PID until the group has been killed, so the PID
 * (and therefore the group id) cannot be recycled in between. */
static int
wait_then_kill_group(pid_t pid) {
    siginfo_t info;

    while (waitid(P_PID, (id_t)pid, &info, WEXITED | WNOWAIT) < 0) {
        if (errno != EINTR) {
            kill(-pid, SIGKILL);
            fail("waitid");
        }
    }
    kill(-pid, SIGKILL);

    int status;
    while (waitpid(pid, &status, 0) < 0) {
        if (errno != EINTR) {
            fail("waitpid");
        }
    }
    return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
}

/* Order matters: block signals, fork, install handlers, then unblock, so a
 * cancellation can never arrive before child_pid is known. */
static int
run_command(char **command) {
    sigset_t previous_mask;

    block_cancel_signals(&previous_mask);
    pid_t pid = spawn_confined(command, &previous_mask);
    install_cancel_handlers();
    restore_signal_mask(&previous_mask);
    sb_discard_in_parent();
    return wait_then_kill_group(pid);
}

/* Preserve only policy setup state and the standard streams. */
static void
prepare_process(void) {
    struct rlimit no_core = { 0, 0 };

    if (setrlimit(RLIMIT_CORE, &no_core)) {
        fail("disable core dumps");
    }
    sb_prepare_fds();
    set_limit(RLIMIT_NOFILE, NOFILE_LIMIT);
    umask(077);
}

int
main(int argc, char **argv) {
    sb_init();

    switch (detect_mode(argc, argv)) {
    case MODE_CHECK:
        sb_enforce();
        return 0;
    case MODE_RUN: {
            char **command = parse_options(argc, argv);
            prepare_process();
            return run_command(command);
        }
    }
    return SANDBOX_EXIT_SETUP_FAILED; /* unreachable */
}
