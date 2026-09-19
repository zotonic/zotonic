# Sandboxing media processes

Media commands use `z_exec:run(Profile, Command, Options)`. Existing `run/1,2`
callers keep their existing behavior; backups and other server commands are not
assigned a media policy. Profiles are `file`, `imagemagick`, `imagemagick_pdf`,
`ffmpeg`, and `ffprobe`.

The profile-aware API runs a standalone native launcher before executing the
command. Configured command strings retain shell syntax, but the shell and all
its delegates run inside the sandbox. Profile permissions are explicit; neither
the command's working directory nor the input/output parent directories are
implicitly accessible. Use absolute paths for inputs, outputs and `cd`.

```erlang
z_exec:run(imagemagick, Command, #{
    read => [InputFile],
    write => [OutputFile],
    timeout => 120000
}).
```

`read` contains input files and any required assets. `write` contains regular
output files, never directories. Missing output files are exclusively created
before launch; existing symlinks and non-regular files are refused. This permits
writing the specified output without granting write access to neighboring files.
Tools must overwrite the output itself, rather than unlinking it or replacing it
by renaming another file. Existing outputs are not rolled back on failure; newly
created outputs are removed on failure. As with the existing media pipeline,
callers own validation and publication of successful output.

Each invocation gets a private scratch directory and a cleared environment, with
`PATH`, a fixed locale, and scratch-backed `HOME`, `TMPDIR`,
`MAGICK_TEMPORARY_PATH`, and `XDG_CACHE_HOME`. Additional inherited descriptors
are closed. Runtime libraries and application-specific configuration, font and
resource paths are readable. Executable permissions cover the shell, dynamic
loader and selected tools. The PDF profile adds Ghostscript. Paths are resolved
by the native launcher; missing explicit paths fail setup.

On Linux the helper reads each executable's ELF `PT_INTERP` entry and grants
execution to that exact loader. Library directories remain read-only, without
recursive execute grants. Custom scripts need an explicit execute grant for
their interpreter. These restrictions do not prevent an already compromised
process from running code within its permitted filesystem and network access.

Image previews are generated in a private directory alongside the destination
and atomically renamed into place after successful conversion. Concurrent
requests coordinate by destination path; partial outputs are never published.

## Linux

Build dependencies: a C compiler, Linux headers with Landlock support and
libseccomp 2.5+ development files (`libseccomp-dev` on Debian/Ubuntu). The rebar compile
hook builds `apps/zotonic_core/priv/bin/zotonic-sandbox`. The development Docker
image includes the dependency. Release builds must include the resulting `priv`
directory and the runtime libseccomp library. The helper is **not setuid**.
Hex packages exclude the compiled helper and include its C source and Makefile,
so dependency compilation builds it for the consumer's OS and architecture.

Requires enabled Landlock ABI 3 or newer (upstream Linux 6.2+), detected by syscall
at execution time. ABI 1/2 are rejected because they cannot restrict truncation.
The launcher handles all filesystem rights through ABI 3, plus device ioctl
rights when ABI 5 is available. Unspecified rights in newer ABIs are not claimed
as protections. Seccomp independently blocks sockets, networking through
io_uring, process inspection/signalling APIs and process-group/session changes.
It is a denylist, not a general syscall allowlist or a VM boundary.
Resource-limit operations through `prlimit64` are permitted only with PID zero
(the calling process); explicit PID targets are denied, including other
processes owned by the Zotonic user.

A supervisor keeps the decoder and delegates in a job process group. On normal
completion, failure, or cancellation, it kills that group with SIGKILL. The
Linux filter prevents delegates from moving into another group. If the Erlang
caller dies, a monitor asks the supervisor to cancel the job.

Wall-clock time, CPU time, address space, file size, open descriptors, stdout and
stderr are bounded. CPU/address-space/file-size limits apply per process/file;
they are not aggregate job limits. Use deployment-level cgroup memory/PID/CPU
limits and disk quotas for protection against aggregate resource exhaustion.
Inputs and outputs should be on a filesystem supported by Landlock. Validate
actual deployment mounts, especially host-shared filesystems in virtualized
container environments; never work around a denial by automatically rerunning
without the sandbox.

## macOS

The same helper uses Seatbelt's deprecated `sandbox_init` API with a generated
OS sandbox profile. This is unrelated to ImageMagick's `policy.xml`. File content
access and network access are restricted; file metadata and the root directory
are readable for dyld startup. System frameworks, the dyld cache and Homebrew
runtime resources are readable.

CPU, file-size, descriptor and output limits apply. There is no address-space
limit on macOS: `RLIMIT_AS` is not usable for this implementation. Seatbelt does
not prevent `setsid`/`setpgid`, so cleanup of descendants that deliberately detach
is **best effort**. This backend does not offer Linux's process-group guarantee.
Apple's API is deprecated; test each supported OS version. Already-sandboxed
parents may reject Seatbelt initialization; that error is propagated.

Windows and BSD backends are not implemented. Required mode logs an actionable
error and reports
`{sandbox_unsupported, Os}` there. Capsicum/jails and pledge/unveil need separate
implementations and platform tests.

On these platforms, administrators can explicitly opt out by adding
`{exec_sandbox, disabled}` to the Zotonic application configuration in
`zotonic.config`, or `exec_sandbox: disabled` under `zotonic:` in YAML.
This enables unrestricted media commands and removes sandbox protection.

## Configuration and failures

The Zotonic application setting `exec_sandbox` defaults to `required`. Missing
helpers, unsupported systems, unavailable kernel features, or policy setup
failures stop the operation. There is no automatic unrestricted retry.
`exec_sandbox = disabled` is an explicit administrator opt-out for the media
call sites. `z_exec:sandbox_status/0` probes actual policy enforcement, including
seccomp on Linux; it does not prove every installed tool's resource paths fit.

Additional trusted assets or custom executables can be supplied per profile:

```erlang
{exec_sandbox_profiles, #{
    imagemagick => #{read => ["/srv/media-assets/fonts"]},
    imagemagick_pdf => #{read => ["/srv/media-assets/fonts"]},
    ffmpeg => #{execute => ["/opt/media/bin/ffmpeg"]}
}}.
```

These grants are additive, and the profiles are independent. A custom binary's
libraries/configuration may need additional read grants. Avoid granting an entire
site directory, `/etc`, `/tmp`, or the media archive. `read`, `write`, `cd` and
resource limits are server-side API options, not user-supplied request options.

**The root-maintained ImageMagick policy is never changed.** If it disables PDF
or a delegate, the operation still fails. Administrators retain control over
accepted formats and ImageMagick resource limits. Sandbox permissions do not
override that policy. Error tuples preserve bounded stdout and stderr separately.

## Verification

Build with `./rebar3 compile`. For integration tests, install ImageMagick,
Ghostscript, FFmpeg and `file`, then run from the repository root as a non-root
user on a host that permits sandbox initialization:

```sh
cc -Wall -Wextra -Werror -o /tmp/zotonic-sandbox-probe \
    apps/zotonic_core/test/sandbox_probe.c
# Linux only: install a deliberately ungranted executable in a readable tree.
if [ "$(uname -s)" = Linux ]; then
    sudo install -m 755 /tmp/zotonic-sandbox-probe /usr/lib/zotonic-sandbox-denied-probe
fi
erlc -o /tmp apps/zotonic_core/test/z_exec_tests.erl
ZOTONIC_SANDBOX_TESTS=1 ZOTONIC_SANDBOX_PROBE=/tmp/zotonic-sandbox-probe \
    ZOTONIC_SANDBOX_DENIED_EXEC=/usr/lib/zotonic-sandbox-denied-probe \
    erl -noshell -pa _build/default/lib/*/ebin /tmp \
    -eval 'case eunit:test(z_exec_tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.'
# After testing on Linux:
if [ "$(uname -s)" = Linux ]; then
    sudo rm /usr/lib/zotonic-sandbox-denied-probe
fi
```

Opting in makes unavailable enforcement a test failure, not a skip. Tests cover
file/network denials, executable restrictions, environment clearing, quoted
filenames, output limits, cancellation, output cleanup, symlink rejection,
image conversion, FFmpeg conversion/probing and Ghostscript rendering. The PDF
delegate is tested directly so that tests need not modify or bypass the
administrator's ImageMagick policy. Linux additionally tests descendant
process-group escape denial.
