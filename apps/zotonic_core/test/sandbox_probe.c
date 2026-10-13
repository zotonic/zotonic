/* Test operations must fail even after a decoder forks a delegate. */
#define _GNU_SOURCE
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __linux__
#include <pthread.h>
#include <sched.h>
#include <sys/syscall.h>
#endif

static int denied(void) { return errno == EPERM || errno == EACCES; }

static int network_denied(int family, int type)
{
    int fd = socket(family, type, 0);
    if (fd < 0) return denied(); /* Linux blocks socket creation itself. */
    struct sockaddr_storage storage;
    memset(&storage, 0, sizeof(storage));
    socklen_t len;
    if (family == AF_INET) {
        struct sockaddr_in *a = (struct sockaddr_in *)&storage;
        a->sin_family = AF_INET;
        a->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        a->sin_port = htons(9);
        len = sizeof(*a);
    } else if (family == AF_INET6) {
        struct sockaddr_in6 *a = (struct sockaddr_in6 *)&storage;
        a->sin6_family = AF_INET6;
        a->sin6_addr = in6addr_loopback;
        a->sin6_port = htons(9);
        len = sizeof(*a);
    } else {
        struct sockaddr_un *a = (struct sockaddr_un *)&storage;
        a->sun_family = AF_UNIX;
        snprintf(a->sun_path, sizeof(a->sun_path), "%s/probe.sock", getenv("TMPDIR"));
        len = sizeof(*a);
    }
    int r = family == AF_UNIX
        ? bind(fd, (struct sockaddr *)&storage, len)
        : connect(fd, (struct sockaddr *)&storage, len);
    int blocked = r < 0 && denied();
    close(fd);
    return blocked;
}

#ifdef __linux__
static int clone_child(void *arg)
{
    (void)arg;
    return 0;
}

static int user_namespace_denied(int flags)
{
    const size_t stack_size = 1024 * 1024;
    char *stack = malloc(stack_size);
    if (!stack) return 0;
    pid_t pid = clone(clone_child, stack + stack_size, flags | SIGCHLD, NULL);
    int blocked = pid < 0 && errno == EPERM;
    if (pid > 0) waitpid(pid, NULL, 0);
    free(stack);
    return blocked;
}

static void *thread_child(void *arg)
{
    return arg;
}

static int namespaces_and_mounts_confined(void)
{
    if (!user_namespace_denied(CLONE_NEWUSER)) return 0;
    if (!user_namespace_denied(CLONE_NEWUSER | CLONE_NEWNS)) return 0;

    /* Invalid arguments avoid mounting anything if a filter rule regresses.
     * Require EPERM, not EINVAL/EFAULT/EBADF/ENOSYS from syscall validation. */
    static const int mount_syscalls[] = {
        SYS_fsopen, SYS_fsconfig, SYS_fsmount, SYS_move_mount,
        SYS_open_tree, SYS_mount_setattr, SYS_fspick
    };
    for (size_t i = 0; i < sizeof(mount_syscalls) / sizeof(mount_syscalls[0]); i++) {
        if (syscall(mount_syscalls[i], -1L, 0L, -1L, 0L, -1L, 0L) != -1 ||
            errno != EPERM) return 0;
    }

    /* clone3 must still trigger libc's fallback, and ordinary threads work. */
    if (syscall(SYS_clone3, NULL, 0) != -1 || errno != ENOSYS) return 0;
    pthread_t thread;
    int token;
    void *result;
    if (pthread_create(&thread, NULL, thread_child, &token)) return 0;
    if (pthread_join(thread, &result)) return 0;
    return result == &token;
}

static int resource_limits_confined(void)
{
    struct rlimit own, other;
    /* These must still work after fork; the filter cannot pin the original
     * process ID as the only allowed target. */
    if (prlimit(0, RLIMIT_NOFILE, NULL, &own)) return 0;
    if (prlimit(0, RLIMIT_NOFILE, &own, NULL)) return 0;
    if (prlimit(getppid(), RLIMIT_NOFILE, NULL, &other) == 0 || !denied()) return 0;
    if (prlimit(getppid(), RLIMIT_NOFILE, &own, NULL) == 0 || !denied()) return 0;
    return 1;
}
#endif

int main(void)
{
    if (!network_denied(AF_INET, SOCK_STREAM)) return 10;
    if (!network_denied(AF_INET6, SOCK_STREAM)) return 11;
    if (!network_denied(AF_UNIX, SOCK_STREAM)) return 12;
    if (!network_denied(AF_INET, SOCK_DGRAM)) return 13;
#ifdef __linux__
    if (!resource_limits_confined()) return 25;
    if (!namespaces_and_mounts_confined()) return 27;
    /* Seatbelt does not mediate setsid/setpgid; only Linux promises this. */
    pid_t pid = fork();
    if (pid < 0) return 20;
    if (pid == 0) {
        if (!resource_limits_confined()) _exit(26);
        if (!namespaces_and_mounts_confined()) _exit(28);
        if (setsid() >= 0) _exit(21);
        if (setpgid(0, 0) == 0) _exit(22);
        _exit(0);
    }
    int status;
    if (waitpid(pid, &status, 0) < 0) return 23;
    return WIFEXITED(status) ? WEXITSTATUS(status) : 24;
#else
    return 0;
#endif
}
