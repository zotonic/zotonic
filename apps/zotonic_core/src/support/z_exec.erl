%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023-2026 Marc Worrell

%% Copyright 2023-2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_exec).

-moduledoc("
Execute OS commands with timeouts and optional application sandboxing.

`run/1` and `run/2` execute unrestricted commands. Use `run/3` for media
processing: it runs the shell, decoder and delegates inside a native sandbox
with explicit filesystem permissions and no network access. Command strings
retain shell syntax; callers must still shell-escape untrusted arguments.

## Application profiles

Supported profiles are `imagemagick`, `imagemagick_pdf`, `ffmpeg`, `ffprobe`
and `file`. Each supplies resource limits and access to the selected tools,
runtime libraries and application configuration/assets. The PDF profile also
permits Ghostscript. ImageMagick's root-maintained `policy.xml` is read as
installed and is never changed or bypassed by this module.

For example, with an already escaped command and absolute file paths:

```erlang
z_exec:run(imagemagick, Command, #{
    read => [InputFile],
    write => [OutputFile],
    timeout => 120000
}).
```

`read` grants access to existing inputs and assets. `write` grants access to
regular output files, never directories. Missing outputs are pre-created;
symlinks and other non-regular outputs are refused. Tools must overwrite the
file itself, rather than replace it by rename. Newly created outputs are
removed on failure; existing outputs are not rolled back. Callers remain
responsible for validating and publishing successful output.

Use absolute paths for `read`, `write` and optional `cd`. Neither the working
directory nor input/output parent directories receive implicit permissions.
Each call has a private scratch directory and a cleared environment with
PATH, a fixed locale and scratch-backed HOME and temporary/cache paths.

Resource options override profile defaults: `timeout` is wall-clock time in
milliseconds, `cpu` is CPU time in seconds, and `memory`, `file_size` and
`max_size` are bytes. In sandbox mode, use a finite `max_size`; stdout and
stderr are bounded separately. CPU/memory limits apply per process and file
size limits per file, not to the entire job. Deployment-level resource
controls are needed to bound aggregate consumption.

## Configuration and failures

The Zotonic setting `exec_sandbox` defaults to `required`. Missing helpers,
unsupported platforms and failed sandbox setup return errors; commands are
never retried without protection. `disabled` explicitly opts out of the
sandbox, retaining only the ordinary run/2 timeout and stdout behavior.

Administrators can add trusted read/execute grants per profile:

```erlang
{exec_sandbox_profiles, #{
    imagemagick => #{read => [\"/srv/media-assets/fonts\"]},
    ffmpeg => #{execute => [\"/opt/media/bin/ffmpeg\"]}
}}.
```

Grants are additive and profiles are independent: configure PDF-specific
grants under `imagemagick_pdf` as well. Custom binaries may also need read
grants for libraries/configuration. Options and configuration are trusted
server-side values; never accept arbitrary grants from a request.

Success returns `{ok, Stdout}`. Command failures return
`{error, {sandbox_command, Reason, #{stdout => Out, stderr => Err}}}`.
Timeout and excessive output return `{error, timeout}` and
`{error, output_limit}`. Setup failures return their own error reasons.
`sandbox_status/0` probes enforcement, but does not validate every tool's
resource paths. Cancellation or caller death asks the native supervisor to
kill the command's process group.

## Platforms

Linux requires enabled Landlock ABI 3+ and libseccomp 2.5+. The native helper
combines filesystem restrictions with a seccomp denylist, including network
and process-group escape restrictions. It runs without root or setuid.

macOS uses the deprecated Seatbelt sandbox_init API. File metadata remains
readable for runtime startup. Address-space limits are not applied, and
cleanup of descendants that deliberately detach is best effort. Test each
supported macOS version, including any restrictions imposed by the parent.

BSD backends are not implemented; required mode returns an unsupported
platform error. Build, deployment and integration-test details are in
`doc/technotes/media-sandboxing.md`.
").

-export([
    run/1,
    run/2,
    run/3,
    sandbox_status/0
]).

-include_lib("kernel/include/file.hrl").

-type profile() :: imagemagick | imagemagick_pdf | ffmpeg | ffprobe | file.

-type os_command() :: iodata().
-type os_command_opts() :: #{
        max_size => non_neg_integer() | infinity,
        timeout => non_neg_integer()
    }.

% Default timeout for commands - 15 minutes.
-define(TIMEOUT, 900000).

% Default maximum stdout size for commands - 1GB
-define(MAX_SIZE, 1073741824).

% Timeout to kill a command after stop has been signaled
-define(KILL_TIMEOUT_SECS, 10).

%% @doc Executes the given command in the default shell for the operating system. Run with
%% a default timeout of 15 minutes and a default maximum returned size of 1GB.
-spec run(Command) -> {ok, Data} | {error, Reason} when
      Command :: os_command(),
      Data :: binary(),
      Reason :: term().
run(Command) ->
    run(Command, #{}).


%% @doc Executes the given command in the default shell for the operating system.
%% The option max_size will terminate the program after more data than the given number
%% of bytes is received. Use 'infinity' to not restrict the output size.
%% The option timeout will terminate the program if more time than the given milliseconds
%% have passed, in that case {error, timeout} is returned.
-spec run(Command, Options) -> {ok, Data} | {error, Reason} when
      Command :: os_command(),
      Options :: os_command_opts(),
      Data :: binary(),
      Reason :: term().
run(Command, Options) ->
    run_exec(Command, Options, []).


%% @doc Run an application command with a filesystem and no-network profile.
%% Commands retain shell syntax for administrator-configured command lines; the
%% shell itself runs INSIDE the sandbox. read names existing inputs and assets;
%% write names regular output files (not directories). Outputs are pre-created
%% so no permission to write their parent directory is needed. cd is only a
%% working directory, never an implicit read grant. System policy.xml is read
%% as installed; this module never generates or changes ImageMagick policies.
%%
%% exec_sandbox defaults to required. disabled is an explicit administrator
%% opt-out. There is no automatic fallback, including after an execution error.
-spec run(Profile, Command, Options) -> {ok, Data} | {error, Reason} when
    Profile :: profile(),
    Command :: iodata(),
    Options :: map(),
    Data :: binary(),
    Reason :: term().
run(Profile, Command, Options) ->
    case profile(Profile) of
        {error, _} = Error -> Error;
        Defaults ->
            case z_config:get(exec_sandbox, required) of
                disabled ->
                    run(unicode:characters_to_binary(Command), maps:merge(Defaults, Options));
                required ->
                    sandbox_run(Profile, Command, maps:merge(Defaults, Options));
                _ ->
                    {error, invalid_sandbox_mode}
            end
    end.


%% @doc Probe enforcement, not just the presence of the helper or kernel version.
-spec sandbox_status() -> {ok, binary()} | {error, term()}.
sandbox_status() ->
    case helper() of
        {ok, Helper} -> run_exec([Helper, "--check"], #{timeout => 5000}, []);
        {error, _} = Error -> Error
    end.


%% Application profiles

profile(imagemagick) ->
    #{
        timeout => 120000,
        max_size => 1048576,
        memory => 4294967296,
        file_size => 1073741824,
        cpu => 120
    };
profile(imagemagick_pdf) ->
    profile(imagemagick);
profile(ffmpeg) ->
    #{
        timeout => 3600000,
        max_size => 1048576,
        memory => 4294967296,
        file_size => 17179869184,
        cpu => 3600
    };
profile(ffprobe) ->
    #{
        timeout => 60000,
        max_size => 16777216,
        memory => 2147483648,
        file_size => 1048576,
        cpu => 60
    };
profile(file) ->
    #{
        timeout => 10000,
        max_size => 65536,
        memory => 536870912,
        file_size => 1048576,
        cpu => 10
    };
profile(_) ->
    {error, unknown_sandbox_profile}.

profile_read(imagemagick_pdf) ->
    profile_read(imagemagick) ++ [
        "/usr/share/ghostscript",
        "/usr/local/share/ghostscript",
        "/etc/ghostscript",
        "/etc/papersize",
        "/opt/homebrew/share/ghostscript",
        "/usr/share/color/icc/ghostscript"
    ];
profile_read(imagemagick) ->
    [
        "/etc/ImageMagick-6",
        "/etc/ImageMagick-7",
        "/usr/share/ImageMagick-6",
        "/usr/share/ImageMagick-7",
        "/usr/local/etc/ImageMagick-6",
        "/usr/local/etc/ImageMagick-7",
        "/opt/homebrew/etc/ImageMagick-6",
        "/opt/homebrew/etc/ImageMagick-7",
        "/etc/fonts",
        "/usr/share/fonts",
        "/usr/local/share/fonts",
        "/var/cache/fontconfig",
        "/opt/homebrew/etc/fonts"
    ];
profile_read(file) ->
    [
        "/usr/share/misc",
        "/usr/share/file",
        "/usr/local/share/misc",
        "/opt/homebrew/share/misc"
    ];
profile_read(ffmpeg) ->
    [
        "/usr/share/ffmpeg",
        "/usr/local/share/ffmpeg"
    ];
profile_read(ffprobe) ->
    profile_read(ffmpeg).

%% Runtime directories deliberately exclude /etc as a whole, application data,
%% home directories, /tmp, and /run. Extra paths are an administrator decision,
%% keyed by application profile in the Zotonic configuration.
runtime_read(Profile) ->
    Base = [
        "/lib",
        "/lib64",
        "/usr/lib",
        "/usr/lib64",
        "/usr/local/lib",
        "/etc/ld.so.cache",
        "/etc/localtime",
        "/usr/share/locale",
        "/dev/null",
        "/dev/urandom",
        "/dev/random"
    ],
    Darwin = case os:type() of
        {unix, darwin} ->
            [
                "/System/Library",
                "/System/Volumes/Preboot/Cryptexes/OS",
                "/usr/share/zoneinfo",
                "/opt/homebrew/lib",
                "/opt/homebrew/Cellar"
            ];
        _ ->
            []
    end,
    existing(Base ++ Darwin ++ profile_read(Profile)) ++ extra_paths(Profile, read).

runtime_execute(Profile) ->
    Binaries = case Profile of
        imagemagick -> ["magick", "convert", "identify"];
        imagemagick_pdf -> ["magick", "convert", "identify", "gs"];
        ffmpeg -> ["ffmpeg"];
        ffprobe -> ["ffprobe"];
        file -> ["file"]
    end,
    Shell = case os:type() of
        {unix, darwin} -> ["/bin/bash"]; % /bin/sh executes its selected variant.
        _ -> []
    end,
    %% The ELF interpreter needs execute permission too.
    existing(["/bin/sh", "/lib", "/lib64", "/usr/lib", "/usr/lib64"] ++ Shell)
        ++ [P || B <- Binaries, P <- [os:find_executable(B)], P =/= false]
        ++ extra_paths(Profile, execute).

existing(Paths) ->
    [P || P <- Paths, {ok, _} <- [file:read_file_info(P)]].

extra_paths(Profile, Access) ->
    Profiles = z_config:get(exec_sandbox_profiles, #{}),
    maps:get(Access, maps:get(Profile, Profiles, #{}), []).

command_path() ->
    case os:getenv("PATH") of
        false -> "/usr/local/bin:/usr/bin:/bin";
        Path -> Path
    end.


%% Sandbox preparation

helper() ->
    case os:type() of
        {unix, linux} -> helper_path();
        {unix, darwin} -> helper_path();
        Os -> {error, {sandbox_unsupported, Os}}
    end.

helper_path() ->
    Path = filename:absname(filename:join([code:priv_dir(zotonic_core), "bin", "zotonic-sandbox"])),
    case filelib:is_regular(Path) of
        true -> {ok, Path};
        false -> {error, sandbox_helper_missing}
    end.

sandbox_run(Profile, Command, Options) ->
    case helper() of
        {error, _} = Error -> Error;
        {ok, Helper} ->
            Scratch = z_convert:to_list(z_tempfile:new()) ++ "-sandbox",
            case file:make_dir(Scratch) of
                ok ->
                    try
                        ok = file:change_mode(Scratch, 8#700),
                        sandbox_run_1(Profile, Command, Options, Helper, Scratch)
                    after
                        file:del_dir_r(Scratch)
                    end;
                {error, Reason} -> {error, {sandbox_scratch, Reason}}
            end
    end.

sandbox_run_1(Profile, Command, Options, Helper, Scratch) ->
    Outputs = maps:get(write, Options, []),
    case prepare_outputs(Outputs, []) of
        {error, _} = Error -> Error;
        {ok, Created} ->
            Result = try
                Read = runtime_read(Profile) ++ Outputs ++ maps:get(read, Options, []),
                Execute = runtime_execute(Profile),
                Args = [
                        Helper,
                        "--cpu", integer_to_list(maps:get(cpu, Options)),
                        "--memory", integer_to_list(maps:get(memory, Options)),
                        "--file-size", integer_to_list(maps:get(file_size, Options))
                    ]
                    ++ path_args("--read", Read)
                    ++ path_args("--execute", Execute)
                    ++ path_args("--write", [Scratch, "/dev/null" | Outputs])
                    ++ ["--", "/bin/sh", "-c", unicode:characters_to_binary(Command)],
                Env = [
                    clear,
                    {"PATH", command_path()},
                    {"LANG", "C"},
                    {"HOME", Scratch},
                    {"TMPDIR", Scratch},
                    {"MAGICK_TEMPORARY_PATH", Scratch},
                    {"XDG_CACHE_HOME", Scratch}
                ],
                run_exec(Args, Options#{sandbox => true}, [
                    {env, Env},
                    {cd, maps:get(cd, Options, Scratch)},
                    {group, 0},
                    kill_group,
                    stderr
                ])
            catch
                error:Reason -> {error, {sandbox_setup, Reason}}
            end,
            case Result of
                {ok, _} -> Result;
                {error, _} ->
                    lists:foreach(fun file:delete/1, Created),
                    Result
            end
    end.

prepare_outputs([], Created) -> {ok, Created};
prepare_outputs([Path | Rest], Created) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = regular}} -> prepare_outputs(Rest, Created);
        {error, enoent} ->
            case file:open(Path, [write, exclusive, raw, binary]) of
                {ok, Fd} ->
                    ok = file:close(Fd),
                    prepare_outputs(Rest, [Path | Created]);
                {error, Reason} -> output_error(Path, Reason, Created)
            end;
        {ok, _} -> output_error(Path, not_regular, Created);
        {error, Reason} -> output_error(Path, Reason, Created)
    end.

output_error(Path, Reason, Created) ->
    lists:foreach(fun file:delete/1, Created),
    {error, {sandbox_output, Path, Reason}}.

path_args(Flag, Paths) ->
    lists:append([[Flag, filename:absname(P)] || P <- lists:usort(Paths)]).


%% Command execution and monitoring

run_exec(Command, Options, ExtraOptions) ->
    Timeout = maps:get(timeout, Options, ?TIMEOUT),
    MaxSize = maps:get(max_size, Options, ?MAX_SIZE),
    ExecOptions = [
        stdout,
        monitor,
        {kill_timeout, ?KILL_TIMEOUT_SECS}
    ],
    case exec:run(Command, ExecOptions ++ ExtraOptions) of
        {ok, Pid, OsPid} ->
            Guard = guard_job(maps:get(sandbox, Options, false), Pid, OsPid),
            {ok, Timer} = timer:send_after(Timeout, {timeout, OsPid}),
            Result = case maps:get(sandbox, Options, false) of
                true -> receive_sandbox(OsPid, MaxSize, <<>>, <<>>);
                false -> receive_data(OsPid, MaxSize, <<>>)
            end,
            % Cancel timeout timer and clear optional late timeout message
            timer:cancel(Timer),
            case Guard of
                undefined -> ok;
                _ -> Guard ! done
            end,
            receive
                {timeout, OsPid} ->
                    ok
                after 0 ->
                    ok
            end,
            Result;
        {error, _} = Error ->
            Error
    end.

receive_data(OsPid, MaxSize, Acc) when MaxSize =:= infinity orelse size(Acc) =< MaxSize ->
    receive
        {'DOWN', OsPid, process, _, normal} ->
            {ok, Acc};
        {'DOWN', OsPid, process, _, Reason} ->
            {error, Reason};
        {timeout, OsPid} ->
            exec:stop(OsPid),
            receive
                {'DOWN', OsPid, process, _, _Reason} ->
                    {error, timeout}
            end;
        {stdout, OsPid, Data} ->
            receive_data(OsPid, MaxSize, <<Acc/binary, Data/binary>>)
    end;
receive_data(OsPid, _MaxSize, Acc) ->
    exec:stop(OsPid),
    receive
        {'DOWN', OsPid, process, _, _Reason} ->
            {ok, Acc}
    end.


%% Keep stderr separate from machine-readable stdout and bound both streams.
receive_sandbox(OsPid, MaxSize, Out, Err) ->
    receive
        {'DOWN', OsPid, process, _, normal} ->
            {ok, Out};
        {'DOWN', OsPid, process, _, Reason} ->
            {error, {sandbox_command, Reason, #{stdout => Out, stderr => Err}}};
        {timeout, OsPid} ->
            stop_sandbox(OsPid, timeout);
        {stdout, OsPid, Data} when byte_size(Out) + byte_size(Data) =< MaxSize ->
            receive_sandbox(OsPid, MaxSize, <<Out/binary, Data/binary>>, Err);
        {stderr, OsPid, Data} when byte_size(Err) + byte_size(Data) =< MaxSize ->
            receive_sandbox(OsPid, MaxSize, Out, <<Err/binary, Data/binary>>);
        {Stream, OsPid, _} when Stream =:= stdout; Stream =:= stderr ->
            stop_sandbox(OsPid, output_limit)
    end.

stop_sandbox(OsPid, Reason) ->
    %% The supervisor kills the entire job with SIGKILL when signalled.
    %% Unlike stop/1, kill/2 preserves erlexec's exit notification if the
    %% supervisor has just exited (a race observed on macOS).
    exec:kill(OsPid, sigterm),
    flush_sandbox(OsPid),
    {error, Reason}.

flush_sandbox(OsPid) ->
    receive
        {'DOWN', OsPid, process, _, _} -> ok;
        {stdout, OsPid, _} -> flush_sandbox(OsPid);
        {stderr, OsPid, _} -> flush_sandbox(OsPid)
    end.


%% erlexec's monitor option reports exits, but does not monitor the caller.
%% Cancel the native supervisor if the Erlang request process disappears.
guard_job(false, _Pid, _OsPid) -> undefined;
guard_job(true, Pid, OsPid) ->
    Owner = self(),
    spawn(fun() ->
        OwnerRef = erlang:monitor(process, Owner),
        JobRef = erlang:monitor(process, Pid),
        receive
            done -> ok;
            {'DOWN', JobRef, process, Pid, _} -> ok;
            {'DOWN', OwnerRef, process, Owner, _} ->
                catch exec:kill(OsPid, sigterm)
        end
    end).
