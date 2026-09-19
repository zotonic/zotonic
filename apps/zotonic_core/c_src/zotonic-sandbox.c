/* Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0 */
/* A non-setuid launcher. All policy is installed before executing untrusted
 * decoders. Exit 125 means setup failed; never retry without confinement. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <signal.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
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
#ifndef LANDLOCK_ACCESS_FS_REFER
#define LANDLOCK_ACCESS_FS_REFER (1ULL << 13)
#endif
#ifndef LANDLOCK_ACCESS_FS_TRUNCATE
#define LANDLOCK_ACCESS_FS_TRUNCATE (1ULL << 14)
#endif
#ifndef LANDLOCK_ACCESS_FS_IOCTL_DEV
#define LANDLOCK_ACCESS_FS_IOCTL_DEV (1ULL << 15)
#endif
static int ruleset;
static uint64_t handled;
#elif defined(__APPLE__)
#include <sandbox.h>
static FILE *policy;
static char *policy_text;
static size_t policy_size;
#endif

static void fail(const char *what)
{
    fprintf(stderr, "zotonic-sandbox: %s: %s\n", what, strerror(errno));
    exit(125);
}

static void limit(int resource, const char *value)
{
    char *end;
    errno = 0;
    unsigned long long n = strtoull(value, &end, 10);
    if (errno || !*value || *end || value[0] == '-' || n == 0 ||
        (unsigned long long)(rlim_t)n != n) {
        errno = EINVAL;
        fail("invalid resource limit");
    }
    struct rlimit r = { (rlim_t)n, (rlim_t)n };
    if (setrlimit(resource, &r)) fail("setrlimit");
}

static void init_policy(void)
{
#ifdef __linux__
    int abi = syscall(SYS_landlock_create_ruleset, NULL, 0,
                      LANDLOCK_CREATE_RULESET_VERSION);
    if (abi < 3) {
        errno = ENOTSUP;
        fail("Landlock ABI 3 or newer required");
    }
    /* Handle ALL filesystem rights through ABI 3, including truncation.
     * Handling only read rights would leave writes unrestricted. */
    handled = (LANDLOCK_ACCESS_FS_TRUNCATE << 1) - 1;
    if (abi >= 5) handled |= LANDLOCK_ACCESS_FS_IOCTL_DEV;
    struct landlock_ruleset_attr attr = { .handled_access_fs = handled };
    ruleset = syscall(SYS_landlock_create_ruleset, &attr, sizeof(attr), 0);
    if (ruleset < 0) fail("landlock_create_ruleset");
#elif defined(__APPLE__)
    policy = open_memstream(&policy_text, &policy_size);
    if (!policy) fail("open_memstream");
    fputs("(version 1)\n(deny default)\n"
          "(allow file-read-metadata)\n"
          "(allow file-read-data (literal \"/\"))\n"
          "(allow process-fork)\n"
          "(allow signal (target self))\n"
          "(allow sysctl-read)\n", policy);
#else
    errno = ENOTSUP;
    fail("unsupported sandbox platform");
#endif
}

#ifdef __APPLE__
static void quoted_path(const char *path)
{
    fputc('"', policy);
    for (const unsigned char *p = (const unsigned char *)path; *p; p++) {
        if (*p < 32 || *p == 127) {
            errno = EINVAL;
            fail("control character in path");
        }
        if (*p == '"' || *p == '\\') fputc('\\', policy);
        fputc(*p, policy);
    }
    fputc('"', policy);
}
#endif

static void path_rule(const char *access, const char *path);

#ifdef __linux__
/* Read PT_INTERP without executing the tool (unlike ldd). Shared libraries only
 * need read access; the kernel also requires EXECUTE on this exact loader.
 * Parse both ELF classes, but only the host byte order. Foreign-endian tools
 * cannot run natively and must not silently receive broader permissions. */
static void read_at(int fd, void *buf, size_t size, off_t offset)
{
    if (pread(fd, buf, size, offset) != (ssize_t)size) {
        errno = ENOEXEC;
        fail("read ELF header");
    }
}

static void interpreter_rule(const char *path)
{
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) fail(path);
    unsigned char ident[EI_NIDENT];
    ssize_t n = pread(fd, ident, sizeof(ident), 0);
    if (n < 0) fail("read executable");
    if (n < SELFMAG || memcmp(ident, ELFMAG, SELFMAG)) {
        /* Script interpreters must be explicitly granted in the profile. */
        close(fd);
        return;
    }
    const uint16_t endian = 1;
    int encoding = *(const unsigned char *)&endian ? ELFDATA2LSB : ELFDATA2MSB;
    if (n != (ssize_t)sizeof(ident) || ident[EI_DATA] != encoding) {
        errno = ENOEXEC;
        fail("unsupported ELF encoding");
    }
    uint64_t offset, stride, count;
    if (ident[EI_CLASS] == ELFCLASS64) {
        Elf64_Ehdr h;
        read_at(fd, &h, sizeof(h), 0);
        offset = h.e_phoff; stride = h.e_phentsize; count = h.e_phnum;
        if (stride != sizeof(Elf64_Phdr)) { errno = ENOEXEC; fail("ELF phentsize"); }
    } else if (ident[EI_CLASS] == ELFCLASS32) {
        Elf32_Ehdr h;
        read_at(fd, &h, sizeof(h), 0);
        offset = h.e_phoff; stride = h.e_phentsize; count = h.e_phnum;
        if (stride != sizeof(Elf32_Phdr)) { errno = ENOEXEC; fail("ELF phentsize"); }
    } else {
        errno = ENOEXEC;
        fail("unsupported ELF class");
    }
    struct stat st;
    if (fstat(fd, &st)) fail("stat ELF");
    if (count == PN_XNUM || offset > (uint64_t)st.st_size ||
        count * stride > (uint64_t)st.st_size - offset) {
        errno = ENOEXEC;
        fail("invalid ELF program headers");
    }
    for (uint64_t i = 0; i < count; i++) {
        uint32_t type;
        uint64_t start, size;
        if (ident[EI_CLASS] == ELFCLASS64) {
            Elf64_Phdr h;
            read_at(fd, &h, sizeof(h), offset + i * stride);
            type = h.p_type; start = h.p_offset; size = h.p_filesz;
        } else {
            Elf32_Phdr h;
            read_at(fd, &h, sizeof(h), offset + i * stride);
            type = h.p_type; start = h.p_offset; size = h.p_filesz;
        }
        if (type != PT_INTERP) continue;
        char loader[PATH_MAX];
        if (size < 2 || size > sizeof(loader) || start > (uint64_t)st.st_size ||
            size > (uint64_t)st.st_size - start) {
            errno = ENOEXEC;
            fail("invalid ELF interpreter");
        }
        read_at(fd, loader, size, start);
        if (loader[0] != '/' || loader[size - 1] != '\0' || strlen(loader) != size - 1) {
            errno = ENOEXEC;
            fail("invalid ELF interpreter path");
        }
        /* Internal flag avoids recursive parsing of the loader itself. */
        path_rule("--loader", loader);
        break;
    }
    close(fd);
}
#endif

static void path_rule(const char *access, const char *path)
{
    struct stat st;
    char *resolved = realpath(path, NULL);
    if (!resolved) fail(path);
    if (stat(resolved, &st)) fail(path);
#ifdef __linux__
    if (!strcmp(access, "--loader") && !S_ISREG(st.st_mode)) {
        errno = ENOEXEC;
        fail("ELF interpreter is not a regular file");
    }
    uint64_t rights = 0;
    if (!strcmp(access, "--read")) {
        rights = LANDLOCK_ACCESS_FS_READ_FILE;
        if (S_ISDIR(st.st_mode)) rights |= LANDLOCK_ACCESS_FS_READ_DIR;
    } else if (!strcmp(access, "--write")) {
        rights = LANDLOCK_ACCESS_FS_WRITE_FILE | LANDLOCK_ACCESS_FS_TRUNCATE;
        if (S_ISDIR(st.st_mode)) {
            rights |= LANDLOCK_ACCESS_FS_READ_FILE | LANDLOCK_ACCESS_FS_READ_DIR |
                      LANDLOCK_ACCESS_FS_REMOVE_DIR | LANDLOCK_ACCESS_FS_REMOVE_FILE |
                      LANDLOCK_ACCESS_FS_MAKE_DIR | LANDLOCK_ACCESS_FS_MAKE_REG |
                      LANDLOCK_ACCESS_FS_REFER;
        }
    } else {
        rights = LANDLOCK_ACCESS_FS_EXECUTE | LANDLOCK_ACCESS_FS_READ_FILE;
    }
    int fd = open(resolved, O_PATH | O_CLOEXEC);
    if (fd < 0) fail(path);
    struct landlock_path_beneath_attr rule = {
        .allowed_access = rights, .parent_fd = fd
    };
    if (syscall(SYS_landlock_add_rule, ruleset, LANDLOCK_RULE_PATH_BENEATH,
                &rule, 0)) fail("landlock_add_rule");
    close(fd);
    if (!strcmp(access, "--execute") && S_ISREG(st.st_mode))
        interpreter_rule(resolved);
#elif defined(__APPLE__)
    const char *operations = !strcmp(access, "--read") ? "file-read*" :
        !strcmp(access, "--write") ? "file-read* file-write*" :
        "file-read* process-exec";
    fprintf(policy, "(allow %s (%s ", operations,
            S_ISDIR(st.st_mode) ? "subpath" : "literal");
    quoted_path(resolved);
    fputs("))\n", policy);
#else
    (void)access;
#endif
    free(resolved);
}

static void enforce(void)
{
#ifdef __linux__
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) fail("no_new_privs");
    if (syscall(SYS_landlock_restrict_self, ruleset, 0)) fail("landlock_restrict_self");
    close(ruleset);
    /* Deny all socket creation/use, including Unix sockets and alternate
     * io_uring networking. libseccomp rejects foreign syscall ABIs as well.
     * Keep children in the erlexec process group so timeout cleanup reaches
     * delegates. This is a denylist, not a general syscall allowlist. */
    const char *deny[] = {
        "socket", "socketpair", "connect", "bind", "listen", "accept", "accept4",
        "sendto", "sendmsg", "sendmmsg", "recvfrom", "recvmsg", "recvmmsg",
        "io_uring_setup", "io_uring_enter", "io_uring_register",
        "ptrace", "process_vm_readv", "process_vm_writev", "pidfd_getfd",
        "pidfd_send_signal", "kill", "tkill", "tgkill", "rt_sigqueueinfo", "rt_tgsigqueueinfo",
        "setsid", "setpgid", "unshare", "setns", "mount", "umount2", "pivot_root",
        "chroot", "bpf", "perf_event_open", "userfaultfd", "keyctl", "add_key",
        "request_key", "chmod", "fchmod", "fchmodat", "fchmodat2",
        "chown", "fchown", "lchown", "fchownat", "utime", "utimes", "futimesat", "utimensat",
        "setxattr", "lsetxattr", "fsetxattr", "removexattr", "lremovexattr", "fremovexattr",
        "process_madvise", "process_mrelease",
        "shmget", "shmat", "shmctl", "semget", "semop", "semtimedop", "semctl",
        "msgget", "msgsnd", "msgrcv", "msgctl",
        "open_by_handle_at", "name_to_handle_at", "reboot",
        "kexec_load", "kexec_file_load", "init_module", "finit_module", "delete_module"
    };
    scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_ALLOW);
    if (!ctx) fail("seccomp_init");
    for (size_t i = 0; i < sizeof(deny) / sizeof(deny[0]); i++) {
        int nr = seccomp_syscall_resolve_name(deny[i]);
        if (nr != __NR_SCMP_ERROR &&
            seccomp_rule_add(ctx, SCMP_ACT_ERRNO(EPERM), nr, 0) != 0)
            fail("seccomp_rule_add");
    }
    /* libc uses prlimit64(0, ...) for getrlimit/setrlimit. Permit that
     * self-only form, including in descendants, but never target another
     * process: matching UIDs otherwise allow changing the server's limits. */
    if (seccomp_rule_add(ctx, SCMP_ACT_ERRNO(EPERM), SCMP_SYS(prlimit64), 1,
                         SCMP_A0(SCMP_CMP_NE, 0)) != 0)
        fail("seccomp prlimit64");
    /* clone3 has opaque arguments: force libc's legacy clone fallback. */
    int nr = seccomp_syscall_resolve_name("clone3");
    if (nr != __NR_SCMP_ERROR &&
        seccomp_rule_add(ctx, SCMP_ACT_ERRNO(ENOSYS), nr, 0) != 0)
        fail("seccomp clone3");
    if (seccomp_load(ctx)) fail("seccomp_load");
    seccomp_release(ctx);
#elif defined(__APPLE__)
    if (fclose(policy)) fail("sandbox profile");
    char *error = NULL;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
    if (sandbox_init(policy_text, 0, &error)) {
        fprintf(stderr, "zotonic-sandbox: Seatbelt: %s\n", error ? error : "failed");
        exit(125);
    }
#pragma clang diagnostic pop
    free(policy_text);
#endif
}

/* The supervisor stays outside the policy and owns the job's process group.
 * It kills every delegate, including children ignoring SIGTERM, on success,
 * failure or cancellation. Linux seccomp prevents process-group escape;
 * macOS Seatbelt does not, so descendant cleanup is best effort there. */
static volatile sig_atomic_t child_pid;

static void cancel_job(int sig)
{
    if (child_pid > 0) kill(-(pid_t)child_pid, SIGKILL);
    _exit(128 + sig);
}

static int run_command(char **command)
{
    sigset_t blocked, previous;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGTERM);
    sigaddset(&blocked, SIGINT);
    sigaddset(&blocked, SIGHUP);
    if (sigprocmask(SIG_BLOCK, &blocked, &previous)) fail("sigprocmask");
    pid_t pid = fork();
    if (pid < 0) fail("fork");
    if (pid == 0) {
        if (setpgid(0, 0)) fail("child process group");
        if (sigprocmask(SIG_SETMASK, &previous, NULL)) fail("sigprocmask");
        enforce();
        execv(command[0], command);
        fail("execv");
    }
    child_pid = pid;
    /* The child also sets its group before exec; EACCES means it won the race. */
    if (setpgid(pid, pid) && errno != EACCES && errno != ESRCH) {
        kill(pid, SIGKILL);
        fail("parent process group");
    }
    struct sigaction action;
    memset(&action, 0, sizeof(action));
    action.sa_handler = cancel_job;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGTERM, &action, NULL) || sigaction(SIGINT, &action, NULL) ||
        sigaction(SIGHUP, &action, NULL)) fail("sigaction");
    if (sigprocmask(SIG_SETMASK, &previous, NULL)) fail("sigprocmask");
#ifdef __linux__
    close(ruleset);
#endif
    /* WNOWAIT holds the leader's PID until the group has been killed. */
    siginfo_t info;
    while (waitid(P_PID, (id_t)pid, &info, WEXITED | WNOWAIT) < 0) {
        if (errno != EINTR) { kill(-pid, SIGKILL); fail("waitid"); }
    }
    kill(-pid, SIGKILL);
    int status;
    while (waitpid(pid, &status, 0) < 0) {
        if (errno != EINTR) fail("waitpid");
    }
    return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
}

int main(int argc, char **argv)
{
    if (argc == 2 && !strcmp(argv[1], "--check")) {
        init_policy();
        enforce();
        return 0;
    }
    init_policy();
    int i;
    for (i = 1; i < argc && strcmp(argv[i], "--"); i += 2) {
        if (i + 1 >= argc) { errno = EINVAL; fail("missing option value"); }
        if (!strcmp(argv[i], "--read") || !strcmp(argv[i], "--write") ||
            !strcmp(argv[i], "--execute")) path_rule(argv[i], argv[i+1]);
        else if (!strcmp(argv[i], "--cpu")) limit(RLIMIT_CPU, argv[i+1]);
        else if (!strcmp(argv[i], "--memory")) {
#ifdef __linux__
            limit(RLIMIT_AS, argv[i+1]);
#else
            /* macOS does not support a useful RLIMIT_AS for these tools. */
#endif
        }
        else if (!strcmp(argv[i], "--file-size")) limit(RLIMIT_FSIZE, argv[i+1]);
        else { errno = EINVAL; fail("unknown option"); }
    }
    if (i + 1 >= argc) { errno = EINVAL; fail("missing command"); }
    struct rlimit core = {0, 0};
    if (setrlimit(RLIMIT_CORE, &core)) fail("disable core dumps");
    /* Preserve only policy setup state and the standard streams. */
#ifdef __linux__
    if (ruleset != 3) {
        if (dup3(ruleset, 3, O_CLOEXEC) < 0) fail("dup3 ruleset");
        close(ruleset);
        ruleset = 3;
    }
#ifdef SYS_close_range
    if (syscall(SYS_close_range, 4U, ~0U, 0) < 0)
#endif
    {
        long maxfd = sysconf(_SC_OPEN_MAX);
        if (maxfd < 0) fail("descriptor limit");
        for (long fd = 4; fd < maxfd; fd++) close((int)fd);
    }
#elif defined(__APPLE__)
    long maxfd = sysconf(_SC_OPEN_MAX);
    if (maxfd < 0) fail("descriptor limit");
    for (long fd = 3; fd < maxfd; fd++) close((int)fd);
#endif
    limit(RLIMIT_NOFILE, "256");
    umask(077);
    return run_command(&argv[i+1]);
}
