/* Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0 */

/* Linux-only test of the actual launcher filter, independent of Docker's
 * outer filter, capabilities, and Landlock. Keep production policy unchanged. */
#define main sandbox_launcher_main
#include "../c_src/zotonic-sandbox.c"
#undef main

static const char *const mount_calls[] = {
    "fsopen", "fsconfig", "fsmount", "move_mount", "open_tree",
    "mount_setattr", "fspick"
};

static int syscall_number(const char *name)
{
    int nr = seccomp_syscall_resolve_name(name);
    if (nr == __NR_SCMP_ERROR) {
        fprintf(stderr, "Unknown test syscall: %s\n", name);
        exit(1);
    }
    return nr;
}

static void baseline_rule(scmp_filter_ctx ctx, const char *name)
{
    seccomp_check(seccomp_rule_add(ctx, SCMP_ACT_ERRNO(EUCLEAN),
                                  syscall_number(name), 0), "baseline rule");
}

static void install_baseline(void)
{
    scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_ALLOW);
    if (!ctx) fail("baseline seccomp_init");
    baseline_rule(ctx, "clone");
    baseline_rule(ctx, "clone3");
    for (size_t i = 0; i < sizeof(mount_calls) / sizeof(mount_calls[0]); i++) {
        baseline_rule(ctx, mount_calls[i]);
    }
    seccomp_check(seccomp_load(ctx), "baseline seccomp_load");
    seccomp_release(ctx);
}

static int expect_errno(const char *name, long rc, int expected)
{
    int actual = errno;
    if (rc == -1 && actual == expected) return 0;
    fprintf(stderr, "%s: return %ld, errno %d; expected -1, errno %d\n",
            name, rc, actual, expected);
    return 1;
}

static int clone_child(void *arg)
{
    (void)arg;
    return 0;
}

static int check_policy(bool hardened)
{
    int errors = 0;
    int expected = hardened ? EPERM : EUCLEAN;
    const size_t stack_size = 1024 * 1024;
    char *stack = malloc(stack_size);
    if (!stack) fail("test stack");

    /* The libc wrapper handles the architecture-specific raw argument order.
     * The unconditional baseline prevents even ordinary clone from executing. */
    errors += expect_errno("clone NEWUSER",
        clone(clone_child, stack + stack_size, CLONE_NEWUSER | SIGCHLD, NULL), expected);
    errors += expect_errno("clone NEWUSER|NEWNS",
        clone(clone_child, stack + stack_size,
              CLONE_NEWUSER | CLONE_NEWNS | SIGCHLD, NULL), expected);
    errors += expect_errno("clone ordinary",
        clone(clone_child, stack + stack_size, SIGCHLD, NULL), EUCLEAN);
    free(stack);

    errors += expect_errno("clone3", syscall(syscall_number("clone3"), NULL, 0),
                           hardened ? ENOSYS : EUCLEAN);
    for (size_t i = 0; i < sizeof(mount_calls) / sizeof(mount_calls[0]); i++) {
        errors += expect_errno(mount_calls[i],
            syscall(syscall_number(mount_calls[i]), -1L, 0L, -1L, 0L, -1L, 0L), expected);
    }
    return errors;
}

int main(void)
{
    /* For equal-priority ERRNO actions, the newest matching filter supplies
     * the errno. First prove EUCLEAN wins over outer filters. Then a missing
     * production rule leaves EUCLEAN instead of being masked by outer EPERM
     * or kernel permission checks. No tested syscall reaches the kernel body.
     * See docs.kernel.org/userspace-api/seccomp_filter.html#return-values. */
    install_baseline();
    if (check_policy(false)) return 1;
    install_seccomp_denylist();
    if (check_policy(true)) return 1;
    puts("Sandbox seccomp attribution tests passed");
    return 0;
}
