/* Test operations must fail even after a decoder forks a delegate. */
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

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

int main(void)
{
    if (!network_denied(AF_INET, SOCK_STREAM)) return 10;
    if (!network_denied(AF_INET6, SOCK_STREAM)) return 11;
    if (!network_denied(AF_UNIX, SOCK_STREAM)) return 12;
    if (!network_denied(AF_INET, SOCK_DGRAM)) return 13;
#ifdef __linux__
    /* Seatbelt does not mediate setsid/setpgid; only Linux promises this. */
    pid_t pid = fork();
    if (pid < 0) return 20;
    if (pid == 0) {
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
