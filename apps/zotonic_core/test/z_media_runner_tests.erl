%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test media job validation, file transfer, callback ownership and sandbox execution.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_runner_tests).

-include_lib("eunit/include/eunit.hrl").
-export([log/2]).

%% Logger callback used to verify the unsupported-platform NOTICE, without mocking OS globals.
log(#{level := notice, msg := {report, #{reason := sandbox_unsupported}}}, #{config := #{pid := Pid}}) ->
    Pid ! sandbox_notice,
    ok;
log(_, _) -> ok.

unsupported_sandbox_test_() ->
    case os:type() of
        {unix, _} -> {timeout, 30, fun unsupported_sandbox/0};
        _ -> []
    end.

unsupported_sandbox() ->
    {ok, _} = application:ensure_all_started(erlexec),
    #{level := OldLevel} = logger:get_primary_config(),
    OldMode = application:get_env(zotonic, exec_sandbox),
    ok = logger:set_primary_config(level, notice),
    ok = logger:add_handler(zmr_sandbox_test, ?MODULE, #{level => notice, config => #{pid => self()}}),
    ok = meck:new(z_exec, [passthrough]),
    try
        %% Simulate FreeBSD on any Unix host; notices below describe the mock OS.
        ok = meck:expect(z_exec, sandbox_status, fun() -> {error, {sandbox_unsupported, {unix, freebsd}}} end),
        application:set_env(zotonic, exec_sandbox, required),
        ?assertEqual({ok, <<"local">>}, z_exec:run_local(file, "printf local", #{})),
        with_files(fun(Input, _) ->
            Cmd = ["test -f ", z_filelib:os_filename(filename:basename(Input)), " && printf runner"],
            ?assertEqual({ok, <<"runner">>}, z_exec:run_sandbox(file, Cmd, #{cd => filename:dirname(Input)}))
        end),
        receive sandbox_notice -> ok after 1000 -> error(missing_notice) end,
        ?assertEqual({error, output_limit}, z_exec:run_sandbox(file, "printf too-large", #{max_size => 2})),
        ?assertEqual({error, timeout}, z_exec:run_sandbox(file, "while :; do :; done", #{timeout => 100})),
        %% The unsupported backend has no native supervisor to escalate TERM.
        ?assertEqual({error, timeout}, z_exec:run_sandbox(file,
            "trap '' TERM; while :; do :; done", #{timeout => 100})),
        ?assertEqual({error, output_limit}, z_exec:run_sandbox(file,
            "trap '' TERM; while :; do printf noise; done", #{max_size => 2})),
        %% Broken installations and enforcement failures must never become unrestricted runs.
        lists:foreach(fun(Reason) ->
            ok = meck:expect(z_exec, sandbox_status, fun() -> {error, Reason} end),
            ?assertEqual({error, Reason}, z_exec:run_sandbox(file, "printf must-not-run", #{}))
        end, [sandbox_helper_missing, {exit_status, 32000}])
    after
        meck:unload(z_exec),
        logger:remove_handler(zmr_sandbox_test),
        logger:set_primary_config(level, OldLevel),
        case OldMode of
            undefined -> application:unset_env(zotonic, exec_sandbox);
            {ok, Mode} -> application:set_env(zotonic, exec_sandbox, Mode)
        end
    end.

url_boundary_test() ->
    ?assert(z_media_runner_protocol:https_url(<<"https://runner.example/jobs">>)),
    lists:foreach(
        fun(U) -> ?assertNot(z_media_runner_protocol:https_url(U)) end,
        [
            <<"http://runner.example">>,
            <<"https://user:secret@runner.example">>,
            <<"https://runner.example/#fragment">>,
            <<"file:///etc/passwd">>,
            undefined
        ]
    ).

hostname_endpoint_test() ->
    ?assertEqual({ok, <<"https://media.example.com/media-runner/jobs">>},
        z_media_runner_protocol:endpoint(<<"media.example.com">>)),
    ?assertEqual({ok, <<"https://localhost:18443/media-runner/jobs">>},
        z_media_runner_protocol:endpoint("localhost:18443")),
    ?assertEqual({ok, <<"https://[::1]:18443/media-runner/jobs">>},
        z_media_runner_protocol:endpoint(<<"[::1]:18443">>)),
    lists:foreach(fun(Host) ->
        ?assertEqual({error, media_runner_configuration}, z_media_runner_protocol:endpoint(Host))
    end, [<<>>, <<"https://media.example.com">>, <<"media.example.com/path">>,
        <<"media.example.com/">>, <<"user@media.example.com">>, <<"media.example.com?x=1">>,
        <<"media.example.com#fragment">>, <<"localhost:0">>, <<"localhost:65536">>]).

runner_protocol_test() ->
    ?assertEqual({ok, <<"http://localhost:8080/media-runner/jobs">>},
        z_media_runner_protocol:endpoint("localhost:8080", "http")),
    ?assertEqual({ok, <<"https://media.example.com/media-runner/jobs">>},
        z_media_runner_protocol:endpoint(<<"media.example.com">>, <<"https">>)),
    ?assertEqual({ok, <<"http://[::1]/media-runner/jobs">>},
        z_media_runner_protocol:endpoint(<<"[::1]">>, <<"http">>)),
    ?assertEqual({error, media_runner_configuration},
        z_media_runner_protocol:endpoint(<<"media.example.com">>, <<"ftp:">>)),
    ?assert(z_media_runner_protocol:http_url(<<"http://localhost:8080/media-runner/jobs">>)),
    ?assertNot(z_media_runner_protocol:https_url(<<"http://localhost:8080/media-runner/jobs">>)),
    lists:foreach(fun(Url) ->
        ?assertNot(z_media_runner_protocol:http_url(Url))
    end, [<<"ftp://runner.example/jobs">>, <<"http://user@runner.example/jobs">>,
        <<"http://runner.example/jobs#fragment">>]).

runner_protocol_config_test() ->
    Keys = [media_runners, media_runner_hostname, media_runner_protocol, media_runner_oauth2_key],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    try
        application:unset_env(zotonic, media_runners),
        application:unset_env(zotonic, media_runner_protocol),
        application:set_env(zotonic, media_runner_hostname, <<"localhost:8080">>),
        application:set_env(zotonic, media_runner_oauth2_key, <<"token">>),
        ?assertMatch({ok, [#{url := <<"https://localhost:8080/media-runner/jobs">>}]},
            z_media_runner_pool:runners()),
        application:set_env(zotonic, media_runner_protocol, <<"http">>),
        ?assertMatch({ok, [#{url := <<"http://localhost:8080/media-runner/jobs">>}]},
            z_media_runner_pool:runners()),
        application:set_env(zotonic, media_runners, [
            #{hostname => <<"a.example">>, oauth2_key => <<"a">>},
            #{hostname => <<"b.example:8080">>, protocol => <<"http">>, oauth2_key => <<"b">>},
            #{<<"hostname">> => <<"c.example">>, <<"protocol">> => "http", <<"oauth2_key">> => <<"c">>}
        ]),
        ?assertMatch({ok, [#{url := <<"https://a.example/media-runner/jobs">>},
            #{url := <<"http://b.example:8080/media-runner/jobs">>},
            #{url := <<"http://c.example/media-runner/jobs">>}]}, z_media_runner_pool:runners())
    after
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

rewrite_overlap_test() ->
    ?assertEqual(
        <<"B[0] A B">>,
        z_media_runner_protocol:rewrite(
            <<"/site/image[0] /site /site/image">>,
            [{<<"/site">>, <<"A">>}, {<<"/site/image">>, <<"B">>}]
        )
    ),
    ?assertEqual(
        <<"B A">>,
        z_media_runner_protocol:rewrite(<<"A B">>, [{<<"A">>, <<"B">>}, {<<"B">>, <<"A">>}])
    ).

profile_limits_test() ->
    %% Check both default selection and enforcement on the remote execution path.
    Profiles = [
        {ffmpeg, 14400000, 43200000, 16777216, 17179869184},
        {ffmpeg_preview, 120000, 600000, 1048576, 1073741824},
        {imagemagick, 120000, 600000, 1048576, 1073741824},
        {imagemagick_pdf, 120000, 600000, 1048576, 1073741824},
        {ffprobe, 60000, 600000, 1048576, 1048576},
        {file, 10000, 60000, 65536, 1048576}
    ],
    ok = meck:new(z_exec, [passthrough]),
    try
        lists:foreach(fun({Profile, Default, Maximum, Console, FileSize}) ->
            ?assertMatch(#{timeout := Default, max_size := Console, file_size := FileSize},
                z_exec:profile(Profile)),
            {ok, Job} = z_media_runner_protocol:pack(Profile, <<"printf ok">>, #{}),
            ?assertEqual(Default, maps:get(<<"timeout">>, Job)),
            ?assertEqual(ok, z_media_runner_protocol:validate(Job#{<<"timeout">> => Maximum})),
            ?assertEqual({error, invalid_job},
                z_media_runner_protocol:validate(Job#{<<"timeout">> => Maximum + 1})),
            ok = meck:expect(z_exec, run_sandbox, fun(P, _, Options) ->
                ?assertEqual(Profile, P),
                ?assertMatch(#{timeout := Default, max_size := Console, file_size := FileSize}, Options),
                {ok, <<"ok">>}
            end),
            ?assertMatch(#{<<"status">> := <<"ok">>}, z_media_runner_protocol:execute(Job))
        end, Profiles)
    after
        meck:unload(z_exec)
    end.

validation_test() ->
    Base = job(),
    ?assertEqual(ok, z_media_runner_protocol:validate(Base)),
    lists:foreach(
        fun(J) -> ?assertEqual({error, invalid_job}, z_media_runner_protocol:validate(J)) end,
        [
            Base#{<<"profile">> => <<"shell">>},
            Base#{<<"timeout">> => 3600001},
            Base#{
                <<"files">> => [
                    #{<<"id">> => 1, <<"write">> => true, <<"extension">> => <<"/../../escape">>}
                ]
            },
            Base#{
                <<"files">> => [
                    #{<<"id">> => 1, <<"write">> => true}, #{<<"id">> => 1, <<"write">> => false}
                ]
            },
            Base#{<<"command">> => []}
        ]
    ).

result_paths_test() ->
    Options = #{write => ["/tmp/must-not-be-written"]},
    ?assertEqual(
        {error, media_runner_invalid_result},
        z_media_runner_protocol:unpack(
            #{
                <<"status">> => <<"ok">>,
                <<"stdout">> => <<>>,
                <<"files">> =>
                    [#{<<"id">> => 999, <<"data">> => base64:encode(<<"bad">>)}]
            },
            Options
        )
    ),
    ?assertEqual(
        {error, {media_runner_processing, <<"command_failed">>}},
        z_media_runner_protocol:unpack(
            #{<<"status">> => <<"error">>, <<"error">> => <<"command_failed">>}, Options
        )
    ).

callback_rendezvous_test() ->
    %% The full Zotonic suite already has a supervised callback registry.
    {Pid, IsOwned} = case z_media_runner:start_link() of
        {ok, Started} -> {Started, true};
        {error, {already_started, Started}} -> {Started, false}
    end,
    Secret = <<"secret">>,
    Id = <<"test-job-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    try
        ok = gen_server:call(Pid, {register, Id, Secret, self()}),
        ?assertNot(z_media_runner:authorized(Id, <<"wrong">>)),
        ?assertEqual({error, gone}, z_media_runner:callback(Id, <<"wrong">>, #{})),
        ?assert(z_media_runner:authorized(Id, Secret)),
        ok = z_media_runner:callback(Id, Secret, #{<<"status">> => <<"ok">>}),
        receive
            {media_runner_result, Id, _} -> ok
        after 100 -> error(no_result)
        end,
        ok = z_media_runner:callback(Id, Secret, #{}),
        receive
            {media_runner_result, Id, _} -> error(duplicate)
        after 10 -> ok
        end,
        ok = gen_server:call(Pid, {remove, Id}),
        ?assertNot(z_media_runner:authorized(Id, Secret)),
        Owner = spawn(fun() ->
            receive
                stop -> ok
            end
        end),
        ok = gen_server:call(Pid, {register, Id, Secret, Owner}),
        Monitor = monitor(process, Owner),
        Owner ! stop,
        receive
            {'DOWN', Monitor, _, _, _} -> ok
        end,
        %% Synchronize after the registry's monitor has fired.
        wait_removed(Id, Secret, 100)
    after
        ok = gen_server:call(Pid, {remove, Id}),
        case IsOwned of
            true -> gen_server:stop(Pid);
            false -> ok
        end
    end.

wait_removed(_, _, 0) ->
    error(owner_not_removed);
wait_removed(Id, Secret, N) ->
    case z_media_runner:authorized(Id, Secret) of
        false ->
            ok;
        true ->
            timer:sleep(1),
            wait_removed(Id, Secret, N - 1)
    end.

pack_roundtrip_test() ->
    with_files(fun(Input, Output) ->
        Options = #{read => [Input], write => [Output], cd => filename:dirname(Input)},
        Command = [
            "cd ",
            z_filelib:os_filename(filename:dirname(Input)),
            "; convert ",
            z_filelib:os_filename(Input ++ "[0]"),
            " ",
            z_filelib:os_filename(Output)
        ],
        {ok, Packed} = z_media_runner_protocol:pack(imagemagick, Command, Options),
        ?assertEqual(nomatch, binary:match(maps:get(<<"command">>, Packed), list_to_binary(Input))),
        ?assertNotEqual(nomatch, binary:match(maps:get(<<"command">>, Packed), <<"[0]">>)),
        Files = maps:get(<<"files">>, Packed),
        ?assert(lists:all(fun(F) -> not maps:is_key(<<"data">>, F) end, Files)),
        [Source] = [F || #{<<"sha256">> := _} = F <- Files],
        {ok, SourceBytes} = file:read_file(Input),
        ?assertEqual(byte_size(SourceBytes), maps:get(<<"size">>, Source)),
        ?assertEqual(binary:encode_hex(crypto:hash(sha256, SourceBytes), lowercase),
            maps:get(<<"sha256">>, Source)),
        [Out] = [F || F <- Files, maps:get(<<"write">>, F)],
        ?assertEqual(<<".png">>, maps:get(<<"extension">>, Out)),
        {ok, _} = z_media_runner_protocol:unpack(
            #{
                <<"status">> => <<"ok">>,
                <<"stdout">> => base64:encode(<<"__ZMR_FILE_1__ PNG">>),
                <<"files">> => [
                    #{
                        <<"id">> => maps:get(<<"id">>, Out),
                        <<"size">> => 6,
                        <<"sha256">> => binary:encode_hex(crypto:hash(sha256, <<"output">>), lowercase)
                    }
                ]
            },
            Options,
            fun(_, Temp, _) ->
                ok = file:write_file(Temp, <<"output">>),
                z_media_runner_protocol:hash_file(Temp)
            end
        ),
        ?assertEqual({ok, <<"output">>}, file:read_file(Output))
    end).

%% Long preview basenames must not make the temporary download exceed NAME_MAX.
long_output_filename_test() ->
    with_files(fun(_, Output) ->
        Dir = filename:dirname(Output),
        LongOutput = filename:join(Dir, lists:duplicate(251, $a) ++ ".jpg"),
        Data = <<"output">>,
        Hash = binary:encode_hex(crypto:hash(sha256, Data), lowercase),
        Result = #{<<"status">> => <<"ok">>, <<"stdout">> => <<>>, <<"files">> => [
            #{<<"id">> => 1, <<"size">> => byte_size(Data), <<"sha256">> => Hash}]},
        lists:foreach(fun(Path) ->
            ok = file:write_file(Path, <<"original">>),
            Received = z_media_runner_protocol:unpack(Result,
                #{write => [Path]}, fun(_, Temp, _) ->
                    ?assertEqual(unicode:characters_to_binary(Dir), filename:dirname(Temp)),
                    ?assert(byte_size(filename:basename(Temp)) < 255),
                    ?assertEqual({ok, <<"original">>}, file:read_file(Path)),
                    {ok, Fd} = file:open(Temp, [write, exclusive, raw, binary]),
                    try file:write(Fd, Data) after file:close(Fd) end,
                    z_media_runner_protocol:hash_file(Temp)
                end),
            ?assertEqual({ok, <<>>}, Received),
            ?assertEqual({ok, Data}, file:read_file(Path)),
            ?assertEqual([], filelib:wildcard(filename:join(Dir, ".download-*")))
        end, [LongOutput, unicode:characters_to_binary(LongOutput)])
    end).

%% Invalid or interrupted downloads must never replace existing caller files.
download_failure_test() ->
    with_files(fun(_, Output) ->
        ok = file:write_file(Output, <<"original">>),
        Hash = binary:encode_hex(crypto:hash(sha256, <<"expected">>), lowercase),
        Result = #{<<"status">> => <<"ok">>, <<"stdout">> => <<>>, <<"files">> => [
            #{<<"id">> => 1, <<"size">> => 8, <<"sha256">> => Hash,
                <<"url">> => <<"https://untrusted.example/results/", Hash/binary>>}]},
        Options = #{write => [Output], media_runner_endpoint => <<"https://runner.example/jobs">>,
            media_runner_token => <<"secret">>},
        ?assertEqual({error, media_runner_invalid_result}, z_media_runner_protocol:unpack(Result, Options)),
        lists:foreach(fun(Reason) ->
            ?assertEqual({error, {media_runner_unavailable, {download, Reason}}},
                z_media_runner_protocol:unpack(Result, Options, fun(_, _, _) -> {error, Reason} end)),
            ?assertEqual({ok, <<"original">>}, file:read_file(Output))
        end, [timeout, socket_closed_remotely, {shutdown, server_closed},
            {tcp_error, socket, econnreset}, {ssl_error, socket, closed}]),
        lists:foreach(fun(Data) ->
            ?assertEqual({error, media_runner_invalid_result},
                z_media_runner_protocol:unpack(Result, Options, fun(_, Temp, _) ->
                    ok = file:write_file(Temp, Data),
                    z_media_runner_protocol:hash_file(Temp)
                end)),
            ?assertEqual({ok, <<"original">>}, file:read_file(Output)),
            ?assertEqual([], filelib:wildcard(filename:join(filename:dirname(Output), ".download-*")))
        end, [<<"truncated">>, <<"tampered">>])
    end).

%% Opt-in because a real OS sandbox cannot be nested in every test environment.
sandbox_roundtrip_test_() ->
    case os:getenv("ZOTONIC_SANDBOX_TESTS") of
        "1" ->
            {timeout, 60, fun() ->
                {ok, _} = application:ensure_all_started(erlexec),
                %% Check the real platform before simulating unsupported probe exits.
                ?assertMatch({ok, _}, z_exec:sandbox_status()),
                probe_exit_boundary(),
                with_files(fun(_, Output) ->
                    ok = file:write_file(Output, <<>>),
                    Command = ["printf x >> ", z_filelib:os_filename(Output), "; exit 78"],
                    ?assertMatch({error, {sandbox_command, {exit_status, 19968}, _}},
                        z_exec:run_sandbox(file, Command, #{write => [Output]})),
                    %% Exactly one execution, even though its exit code matches the probe.
                    ?assertEqual({ok, <<"x">>}, file:read_file(Output))
                end),
                %% argv conversion must not encode UTF-8 command text twice.
                Utf8 = <<"café"/utf8>>,
                ?assertEqual(
                    {ok, Utf8},
                    z_exec:run_sandbox(file, ["printf %s ", Utf8], #{})
                ),
                with_files(fun(Input, Output) ->
                    Options = #{read => [Input], write => [Output]},
                    %% Ubuntu packages ImageMagick 6; other hosts may provide 7.
                    {ConvertCommand, IdentifyCommand} = case os:find_executable("magick") of
                        false -> {"convert ", "identify "};
                        _ -> {"magick ", "magick identify "}
                    end,
                    Cmd = [
                        ConvertCommand, z_filelib:os_filename(Input), " ", z_filelib:os_filename(Output)
                    ],
                    {ok, Packed} = z_media_runner_protocol:pack(imagemagick, Cmd, Options),
                    Saved = Output ++ ".saved",
                    Result = z_media_runner_protocol:execute(Packed, fun(_) -> {ok, {file, Input}} end,
                        fun(Path, F) -> {ok, _} = file:copy(Path, Saved), F end),
                    ?assertMatch(#{<<"status">> := <<"ok">>}, Result),
                    ?assertMatch({ok, _}, z_media_runner_protocol:unpack(Result, Options,
                        fun(_, Temp, _) ->
                            {ok, _} = file:copy(Saved, Temp),
                            ok = file:delete(Saved),
                            z_media_runner_protocol:hash_file(Temp)
                        end)),
                    {ok, Png} = file:read_file(Output),
                    ?assertMatch(<<137, "PNG", _/binary>>, Png),
                    %% ImageMagick's output path is restored for z_media_identify's parser.
                    {ok, Identify} = z_media_runner_protocol:pack(
                        imagemagick,
                        [IdentifyCommand, z_filelib:os_filename(Input ++ "[0]")],
                        #{read => [Input]}
                    ),
                    {ok, Stdout} = z_media_runner_protocol:unpack(
                        z_media_runner_protocol:execute(Identify, fun(_) -> {ok, {file, Input}} end), #{read => [Input]}
                    ),
                    ?assertNotEqual(nomatch, binary:match(Stdout, list_to_binary(Input)))
                end),
                %% The runner must enforce a sandbox even on a host with local opt-out.
                application:set_env(zotonic, exec_sandbox, disabled),
                try
                    Result = z_media_runner_protocol:execute((job())#{
                        <<"command">> => <<"printf bad > /tmp/mediarunner-escape-test">>
                    }),
                    ?assertMatch(#{<<"status">> := <<"error">>}, Result)
                after
                    application:unset_env(zotonic, exec_sandbox)
                end
            end};
        _ ->
            []
    end.

%% Exercise erlexec's real exit-status encoding, replacing only the OS capability probe.
probe_exit_boundary() ->
    ok = meck:new(exec, [passthrough]),
    try
        lists:foreach(fun({Exit, Expected}) ->
            ok = meck:expect(exec, run, fun(Command, Options) ->
                case Command of
                    [_, "--check"] ->
                        meck:passthrough([["/bin/sh", "-c", "exit " ++ integer_to_list(Exit)], Options]);
                    _ -> meck:passthrough([Command, Options])
                end
            end),
            ?assertEqual(Expected, z_exec:sandbox_status()),
            case Exit of
                78 -> ?assertEqual({ok, <<"unsupported-kernel">>},
                    z_exec:run_sandbox(file, "printf unsupported-kernel", #{}));
                125 -> ?assertEqual(Expected, z_exec:run_sandbox(file, "printf must-not-run", #{}))
            end
        end, [{78, {error, {sandbox_unsupported, os:type()}}}, {125, {error, {exit_status, 32000}}}])
    after
        meck:unload(exec)
    end.

local_file_identification_test_() ->
    case {os:type(), os:find_executable("file")} of
        {{unix, _}, Cmd} when Cmd =/= false -> fun local_file_identification/0;
        _ -> []
    end.

local_file_identification() ->
    {ok, _} = application:ensure_all_started(erlexec),
    %% The MIME dispatcher is generated at application startup, not compilation.
    {ok, _} = application:ensure_all_started(mimetypes),
    with_files(fun(Input, _Output) ->
        ok = file:write_file(Input, <<"Plain text.\n">>),
        Keys = [media_runner_hostname, exec_sandbox],
        Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
        ok = meck:new(z_exec, [passthrough, no_link]),
        try
            application:set_env(zotonic, media_runner_hostname, <<"unavailable.invalid">>),
            application:set_env(zotonic, exec_sandbox, invalid),
            ok = meck:expect(z_exec, run, fun(Command, #{timeout := 10000, max_size := 65536} = Options) ->
                meck:passthrough([Command, Options])
            end),
            %% Any accidental profiled call fails the test instead of routing remotely.
            ok = meck:expect(z_exec, run, fun(_, _, _, _) -> error(unexpected_media_job) end),
            ?assertMatch({ok, #{<<"mime">> := <<"text/plain">>}},
                z_media_identify:identify_file_direct(z_convert:to_binary(Input), <<"input.txt">>)),
            ?assertEqual(1, meck:num_calls(z_exec, run, ['_', '_'])),
            ?assertEqual(0, meck:num_calls(z_exec, run, ['_', '_', '_', '_']))
        after
            meck:unload(z_exec),
            lists:foreach(fun
                ({K, undefined}) -> application:unset_env(zotonic, K);
                ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
            end, Old)
        end
    end).

large_input_test() ->
    with_files(fun(Input, _Output) ->
        %% Larger than the old 64 MiB JSON limit; the envelope stays tiny.
        {ok, Fd} = file:open(Input, [write, raw, binary]),
        Size = 70 * 1024 * 1024,
        {ok, _} = file:position(Fd, Size - 1),
        ok = file:write(Fd, <<0>>),
        ok = file:close(Fd),
        {ok, Job} = z_media_runner_protocol:pack(file, "file", #{read => [Input]}),
        [F] = maps:get(<<"files">>, Job),
        ?assertEqual(Size, maps:get(<<"size">>, F)),
        ?assertNot(maps:is_key(<<"data">>, F)),
        ?assert(byte_size(z_json:encode(Job)) < 1024),
        ?assertEqual({error, invalid_job}, z_media_runner_protocol:validate(
            Job#{<<"files">> => [F#{<<"data">> => <<"unexpected">>}]}))
    end).

job() ->
    #{
        <<"version">> => 3,
        <<"profile">> => <<"file">>,
        <<"command">> => <<"printf ok">>,
        <<"files">> => [],
        <<"timeout">> => 1000
    }.
with_files(Fun) ->
    Dir = "/tmp/mediarunner-test-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Input = filename:join(Dir, "input ' space.ppm"),
    Output = filename:join(Dir, "output.png"),
    ok = file:write_file(Input, <<"P3\n1 1\n255\n255 0 0\n">>),
    try
        Fun(Input, Output)
    after
        file:del_dir_r(Dir)
    end.

%% The environment is server configuration, never a value supplied in a job.
https_environment_test() ->
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    Old = application:get_env(zotonic, environment),
    try
        lists:foreach(fun({Environment, Expected}) ->
            application:set_env(zotonic, environment, Environment),
            Options = z_media_runner_protocol:http_options(30000),
            Ssl = proplists:get_value(ssl, Options),
            ?assertEqual(Expected, proplists:get_value(verify, Ssl)),
            ?assertEqual(false, proplists:get_value(autoredirect, Options)),
            case Expected of
                verify_peer -> ?assert(proplists:is_defined(customize_hostname_check, Ssl));
                verify_none -> ok
            end
        end, [{development, verify_none}, {production, verify_peer},
            {test, verify_peer}, {acceptance, verify_peer}, {undefined, verify_peer}])
    after
        case Old of
            undefined -> application:unset_env(zotonic, environment);
            {ok, Value} -> application:set_env(zotonic, environment, Value)
        end
    end.

%% Binary filenames remain UTF-8 even when the VM's native filename encoding is
%% Latin-1 (for example OTP 26 under LANG=C). Unicode lists need a UTF-8 VM.
unicode_paths_test() ->
    with_files(fun(Input, Output) ->
        Unicode = filename:join(filename:dirname(Input), [16#4e2d, 16#e9] ++ ".ppm"),
        UnicodeBin = unicode:characters_to_binary(Unicode),
        {ok, _} = file:copy(Input, UnicodeBin),
        try
            Command = unicode:characters_to_binary(["convert ", z_filelib:os_filename(Unicode),
                " ", z_filelib:os_filename(Output)]),
            {ok, Job} = z_media_runner_protocol:pack(imagemagick, Command,
                #{read => [UnicodeBin], write => [unicode:characters_to_binary(Output)]}),
            ?assertEqual(nomatch, binary:match(maps:get(<<"command">>, Job), UnicodeBin)),
            case file:native_name_encoding() of
                utf8 ->
                    ?assertEqual({ok, Job}, z_media_runner_protocol:pack(imagemagick, Command,
                        #{read => [Unicode], write => [Output]}));
                latin1 ->
                    ok
            end
        after
            %% A Latin-1 directory listing cannot round-trip this name as a list.
            file:delete(UnicodeBin)
        end
    end).

%% Preview jobs retain a distinct wire profile so they bypass the render queue.
ffmpeg_preview_profile_test() ->
    {ok, Job} = z_media_runner_protocol:pack(ffmpeg_preview, <<"ffmpeg -version">>, #{}),
    ?assertEqual(<<"ffmpeg_preview">>, maps:get(<<"profile">>, Job)),
    ?assertEqual(ffmpeg_preview, z_media_runner_protocol:profile(<<"ffmpeg_preview">>)),
    ?assertEqual(ok, z_media_runner_protocol:validate(Job)).

pool_affinity_test() ->
    A = #{url => <<"https://a.example/media-runner">>, token => <<"a">>},
    B = #{url => <<"https://b.example/media-runner">>, token => <<"b">>},
    AId = z_media_runner_pool:identity(A),
    BId = z_media_runner_pool:identity(B),
    File = #{<<"sha256">> => <<"input-hash">>, <<"size">> => 1000},
    Job = #{<<"files">> => [File]},
    Hints = z_media_runner_pool:remember(AId, [File], #{}),
    ?assertEqual([A, B], z_media_runner_pool:rank([B, A], Job, Hints, #{})),
    Both = z_media_runner_pool:remember(BId, [File], Hints),
    ?assertEqual([B, A], z_media_runner_pool:rank([A, B], Job, Both, #{AId => 1})),
    Forgotten = z_media_runner_pool:forget(AId, [<<"input-hash">>], Both),
    ?assertEqual([B, A], z_media_runner_pool:rank([A, B], Job, Forgotten, #{})),
    %% Cold files rank identically regardless of configuration order.
    ?assertEqual(z_media_runner_pool:rank([A, B], Job, #{}, #{}),
        z_media_runner_pool:rank([B, A], Job, #{}, #{})),
    ?assertNotEqual(AId, z_media_runner_pool:identity(A#{token => <<"another-consumer">>})),
    Expired = #{{AId, <<"input-hash">>} => erlang:monotonic_time(second) - 1},
    ?assertEqual(#{}, z_media_runner_pool:remember(BId, [], Expired)),
    Many = [#{<<"sha256">> => integer_to_binary(N)} || N <- lists:seq(1, 10010)],
    ?assertEqual(10000, map_size(z_media_runner_pool:remember(AId, Many, #{}))).

burst_capacity_test_() -> {timeout, 30, fun burst_capacity/0}.

%% Bursts reuse one job ID and upload lease; persistent saturation remains bounded.
burst_capacity() ->
    Keys = [media_runners, media_runner_local_fallback, media_runner_wait_timeout],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    {Pid, Owned} = case z_media_runner:start_link() of
        {ok, Started} -> {Started, true};
        {error, {already_started, Started}} -> {Started, false}
    end,
    Modules = [z_context, z_dispatcher, z_media_runner_protocol],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Calls = ets:new(burst_calls, [public]),
    try
        application:set_env(zotonic, media_runners,
            [#{hostname => <<"burst.example">>, oauth2_key => <<"burst">>}]),
        application:set_env(zotonic, media_runner_local_fallback, false),
        application:set_env(zotonic, media_runner_wait_timeout, 10),
        mock_callback_context(burst_context, burst_site,
            <<"https://client.example/media-runner-callback">>),
        Result = #{<<"status">> => <<"ok">>, <<"stdout">> => base64:encode(<<"done">>), <<"files">> => []},
        meck:expect(z_media_runner_protocol, request, fun(Url, Token, Payload, Timeout) ->
            ?assert(Timeout > 0 andalso Timeout =< 10000),
            z_media_runner_protocol:request(Url, Token, Payload)
        end),
        meck:expect(z_media_runner_protocol, request, fun(Url, _, Request) ->
            case lists:last(binary:split(Url, <<"/">>, [global])) of
                <<"submit">> ->
                    Id = maps:get(<<"id">>, Request),
                    case ets:insert_new(Calls, {id, Id}) of
                        true -> ok;
                        false -> ?assertEqual(Id, ets:lookup_element(Calls, id, 2))
                    end,
                    case ets:update_counter(Calls, submit, 1, {submit, 0}) of
                        1 -> {ok, #{<<"outcome">> => <<"full">>}};
                        2 -> {error, {http_status, 429}};
                        3 ->
                            [File] = maps:get(<<"files">>, Request),
                            {ok, #{<<"outcome">> => <<"missing">>,
                                <<"missing">> => [maps:get(<<"sha256">>, File)]}};
                        4 ->
                            ok = z_media_runner:callback(Id, maps:get(<<"callback_token">>, Request), Result),
                            {ok, #{<<"outcome">> => <<"accepted">>}}
                    end;
                <<"reserve">> ->
                    case ets:update_counter(Calls, reserve, 1, {reserve, 0}) of
                        1 -> {ok, #{<<"outcome">> => <<"full">>}};
                        2 -> {ok, #{<<"outcome">> => <<"busy">>}};
                        3 -> {ok, #{<<"outcome">> => <<"upload">>, <<"upload_token">> => <<"lease">>}}
                    end;
                <<"received">> -> {ok, #{<<"outcome">> => <<"received">>}}
            end
        end),
        meck:expect(z_media_runner_protocol, upload, fun(_, _, <<"lease">>, _, _) ->
            1 = ets:update_counter(Calls, upload, 1, {upload, 0}),
            {ok, 204}
        end),
        with_files(fun(Input, _) ->
            ?assertEqual({ok, <<"done">>}, z_media_runner:run(file, <<"printf done">>,
                #{read => [Input], context => burst_context}))
        end),
        ?assertEqual(4, ets:lookup_element(Calls, submit, 2)),
        ?assertEqual(3, ets:lookup_element(Calls, reserve, 2)),
        ?assertEqual(1, ets:lookup_element(Calls, upload, 2)),
        %% Capacity can take longer than the former five-second retry window.
        ReadyAt = erlang:monotonic_time(millisecond) + 6000,
        meck:expect(z_media_runner_protocol, request, fun(Url, _, Request) ->
            case lists:last(binary:split(Url, <<"/">>, [global])) of
                <<"submit">> ->
                    case erlang:monotonic_time(millisecond) >= ReadyAt of
                        false -> {ok, #{<<"outcome">> => <<"full">>}};
                        true ->
                            ok = z_media_runner:callback(maps:get(<<"id">>, Request),
                                maps:get(<<"callback_token">>, Request), Result),
                            {ok, #{<<"outcome">> => <<"accepted">>}}
                    end;
                <<"received">> -> {ok, #{<<"outcome">> => <<"received">>}}
            end
        end),
        ?assertEqual({ok, <<"done">>},
            z_media_runner:run(file, <<"printf done">>, #{context => burst_context})),
        %% A stalled submit/reserve must receive only the remaining job budget.
        application:set_env(zotonic, media_runner_wait_timeout, 2),
        lists:foreach(fun(Operation) ->
            application:set_env(zotonic, media_runners,
                [#{hostname => <<Operation/binary, "-timeout.example">>, oauth2_key => <<"burst">>}]),
            meck:expect(z_media_runner_protocol, request, fun(Url, _, Payload, Timeout) ->
                ?assert(Timeout > 0 andalso Timeout =< 2000),
                case lists:last(binary:split(Url, <<"/">>, [global])) of
                    <<"submit">> when Operation =:= <<"reserve">> ->
                        [File] = maps:get(<<"files">>, Payload),
                        {ok, #{<<"outcome">> => <<"missing">>,
                            <<"missing">> => [maps:get(<<"sha256">>, File)]}};
                    Operation ->
                        timer:sleep(Timeout),
                        {error, timeout}
                end
            end),
            with_files(fun(Input, _) ->
                T0 = erlang:monotonic_time(millisecond),
                ?assertEqual({error, {media_runner_unavailable, timeout}},
                    z_media_runner:run(file, <<"printf done">>,
                        #{read => [Input], context => burst_context})),
                ?assert(erlang:monotonic_time(millisecond) - T0 < 2500)
            end)
        end, [<<"submit">>, <<"reserve">>]),
        application:set_env(zotonic, media_runners,
            [#{hostname => <<"burst.example">>, oauth2_key => <<"burst">>}]),
        meck:expect(z_media_runner_protocol, request, fun(Url, Token, Payload, Timeout) ->
            ?assert(Timeout > 0 andalso Timeout =< 2000),
            z_media_runner_protocol:request(Url, Token, Payload)
        end),
        application:set_env(zotonic, media_runner_wait_timeout, 2),
        meck:expect(z_media_runner_protocol, request, fun(_, _, _) ->
            ets:update_counter(Calls, full, 1, {full, 0}),
            {ok, #{<<"outcome">> => <<"full">>}}
        end),
        Start = erlang:monotonic_time(millisecond),
        ?assertEqual({error, {media_runner_unavailable, 429}},
            z_media_runner:run(file, <<"printf done">>, #{context => burst_context})),
        ?assert(ets:lookup_element(Calls, full, 2) > 1),
        ?assert(erlang:monotonic_time(millisecond) - Start < 2500)
    after
        ets:delete(Calls),
        lists:foreach(fun meck:unload/1, Modules),
        case Owned of true -> gen_server:stop(Pid); false -> ok end,
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

pool_failover_test_() -> {timeout, 40, fun pool_failover/0}.

pool_failover() ->
    Keys = [media_runners, media_runner_local_fallback, media_runner_wait_timeout],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    {Pid, Owned} = case z_media_runner:start_link() of
        {ok, Started} -> {Started, true};
        {error, {already_started, Started}} -> {Started, false}
    end,
    Modules = [z_context, z_dispatcher, z_media_runner_protocol],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Calls = ets:new(pool_calls, [public]),
    try
        application:set_env(zotonic, media_runners, [
            #{hostname => <<"pool-a.example">>, oauth2_key => <<"token-a">>},
            #{<<"hostname">> => <<"pool-b.example">>, <<"oauth2_key">> => <<"token-b">>}
        ]),
        application:set_env(zotonic, media_runner_local_fallback, false),
        application:set_env(zotonic, media_runner_wait_timeout, 30),
        {ok, [A, B]} = z_media_runner_pool:runners(),
        mock_callback_context(pool_context, pool_site,
            <<"https://client.example/media-runner-callback">>),
        Result = #{<<"status">> => <<"ok">>, <<"stdout">> => base64:encode(<<"done">>), <<"files">> => []},
        meck:expect(z_media_runner_protocol, request, fun(Url, Token, Request) ->
            case lists:last(binary:split(Url, <<"/">>, [global])) of
                <<"submit">> ->
                    Id = maps:get(<<"id">>, Request),
                    Secret = maps:get(<<"callback_token">>, Request),
                    case Token of
                        <<"token-a">> ->
                            ?assertMatch({0, _}, binary:match(Url, <<"https://pool-a.example/">>)),
                            ets:insert(Calls, {rejected, Id, Secret}),
                            {ok, #{<<"outcome">> => <<"unavailable">>}};
                        <<"token-b">> ->
                            ?assertMatch({0, _}, binary:match(Url, <<"https://pool-b.example/">>)),
                            %% A rejected attempt cannot deliver a late callback.
                            [{rejected, OldId, OldSecret}] = ets:lookup(Calls, rejected),
                            ?assertEqual({error, gone}, z_media_runner:callback(OldId, OldSecret, Result)),
                            ets:insert(Calls, {accepted, Id}),
                            %% Deliberately omit the callback; status must recover it.
                            {ok, #{<<"outcome">> => <<"accepted">>}}
                    end;
                <<"received">> ->
                    ?assertEqual(<<"token-b">>, Token),
                    ets:insert(Calls, {received, true}),
                    {ok, #{<<"outcome">> => <<"received">>}}
            end
        end),
        meck:expect(z_media_runner_protocol, request, fun
            (Url, <<"token-b">>, #{<<"id">> := Id}, 5000) ->
            ?assertEqual(<<"status">>, lists:last(binary:split(Url, <<"/">>, [global]))),
            ?assertEqual([{accepted, Id}], ets:lookup(Calls, accepted)),
            {ok, #{<<"outcome">> => <<"completed">>, <<"result">> => Result}};
            (Url, Token, Payload, _Timeout) ->
                z_media_runner_protocol:request(Url, Token, Payload)
        end),
        with_files(fun(Input, _) ->
            Options = #{read => [Input], context => pool_context},
            {ok, Job} = z_media_runner_protocol:pack(file, <<"printf done">>, Options),
            ok = gen_server:call(Pid, {remember, z_media_runner_pool:identity(A), maps:get(<<"files">>, Job)}),
            ?assertEqual({ok, <<"done">>}, z_media_runner:run(file, <<"printf done">>, Options)),
            ?assertEqual([{received, true}], ets:lookup(Calls, received)),
            ?assertEqual([B], gen_server:call(Pid, {rank, [B], Job}))
        end),
        application:set_env(zotonic, media_runners, [#{hostname => <<"bad/path">>, oauth2_key => <<"a">>}]),
        ?assertEqual({error, media_runner_configuration}, z_media_runner_pool:runners()),
        application:set_env(zotonic, media_runners, []),
        ?assertEqual({ok, []}, z_media_runner_pool:runners()),
        ?assertNot(z_media_runner:enabled())
    after
        ets:delete(Calls),
        lists:foreach(fun meck:unload/1, Modules),
        case Owned of true -> gen_server:stop(Pid); false -> ok end,
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

pool_atomic_selection_test() ->
    A = #{url => <<"https://atomic-a.example/jobs">>, token => <<"a">>},
    B = #{url => <<"https://atomic-b.example/jobs">>, token => <<"b">>},
    Job = #{<<"profile">> => <<"file">>, <<"files">> => [#{<<"sha256">> => <<"same-input">>, <<"size">> => 10}]},
    {ok, Empty} = z_media_runner:init([]),
    {reply, {ok, First}, State1} = z_media_runner:handle_call(
        {select, [A,B], Job, <<"atomic-1">>, <<"secret">>, self()}, undefined, Empty),
    %% Intervening load must not split the same input's upload across runners.
    BusyJobs = (maps:get(jobs, State1))#{other => #{runner => z_media_runner_pool:identity(First),
        profile => <<"file">>, files => []}},
    {reply, {ok, First}, State2} = z_media_runner:handle_call(
        {select, [A,B], Job, <<"atomic-2">>, <<"secret">>, self()}, undefined, State1#{jobs => BusyJobs}),
    maps:foreach(fun
        (_, #{monitor := Ref}) -> demonitor(Ref, [flush]);
        (_, _) -> ok
    end, maps:get(jobs, State2)).

pool_pinned_download_retry_test_() -> {timeout, 15, fun pool_pinned_download_retry/0}.

pool_pinned_download_retry() ->
    ensure_sidejobs(),
    Keys = [media_runners, media_runner_local_fallback],
    Old = [{K, application:get_env(zotonic, K)} || K <- Keys],
    {Pid, Owned} = case z_media_runner:start_link() of
        {ok, Started} -> {Started, true};
        {error, {already_started, Started}} -> {Started, false}
    end,
    Modules = [z_context, z_dispatcher, z_media_runner_protocol, z_media_runner_http],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    Calls = ets:new(pinned_retry, [public]),
    ets:insert(Calls, {downloads, 0}),
    try
        application:set_env(zotonic, media_runners, [
            #{hostname => <<"pinned-", T/binary, ".example">>, oauth2_key => T}
            || T <- [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>]]),
        application:set_env(zotonic, media_runner_local_fallback, false),
        mock_callback_context(pinned_context, pinned_site,
            <<"https://client.example/callback">>),
        Version = fun(Major) ->
            Tool = case Major of 6 -> <<"convert">>; 7 -> <<"magick">> end,
            {ok, #{<<"imagemagick">> => #{<<"available">> => true, <<"tool">> => Tool,
                <<"major">> => Major, <<"version">> => <<(integer_to_binary(Major))/binary, ".0.0">>}}}
        end,
        meck:expect(z_media_runner_protocol, request, fun(_, T, _, 5000) ->
            Version(case T of <<"a">> -> 6; <<"b">> -> 6; _ -> 7 end)
        end),
        z_media_imagemagick:clear_cache(),
        #{cmd := Cmd, major := 7} = Installation = z_media_imagemagick:selected(),
        %% Majority changes after arguments were generated: keep using IM 7.
        meck:expect(z_media_runner_protocol, request, fun
            (_, T, _, 5000) ->
                Version(case T of <<"d">> -> 7; <<"e">> -> 7; _ -> 6 end);
            (Url, Token, Payload, _Timeout) ->
                z_media_runner_protocol:request(Url, Token, Payload)
        end),
        z_media_imagemagick:clear_cache(),
        ?assertMatch(#{major := 6}, z_media_imagemagick:selected()),
        Data = <<"result">>,
        Hash = binary:encode_hex(crypto:hash(sha256, Data), lowercase),
        meck:expect(z_media_runner_protocol, request, fun(Url, T, Request) ->
            ?assert(lists:member(T, [<<"d">>, <<"e">>])),
            case lists:last(binary:split(Url, <<"/">>, [global])) of
                <<"submit">> ->
                    ?assertMatch({0, _}, binary:match(maps:get(<<"command">>, Request), <<"magick ">>)),
                    [Out] = [F || #{<<"write">> := true} = F <- maps:get(<<"files">>, Request)],
                    {ok, Base} = z_media_runner_protocol:endpoint(<<"pinned-", T/binary, ".example">>),
                    Result = #{<<"status">> => <<"ok">>, <<"stdout">> => <<>>, <<"files">> => [
                        #{<<"id">> => maps:get(<<"id">>, Out), <<"size">> => 6, <<"sha256">> => Hash,
                            <<"url">> => <<Base/binary, "/results/", Hash/binary>>}]},
                    ok = z_media_runner:callback(maps:get(<<"id">>, Request), maps:get(<<"callback_token">>, Request), Result),
                    {ok, #{<<"outcome">> => <<"accepted">>}};
                <<"received">> -> {ok, #{<<"outcome">> => <<"received">>}}
            end
        end),
        with_files(fun(Input, Output) ->
            ok = file:write_file(Output, <<"original">>),
            meck:expect(z_media_runner_http, download, fun(_, _, Temp, 6, _) ->
                ?assertEqual({ok, <<"original">>}, file:read_file(Output)),
                case ets:update_counter(Calls, downloads, 1) of
                    1 -> {error, timeout};
                    2 -> ok = file:write_file(Temp, Data), {ok, 6, Hash}
                end
            end),
            Options = #{read => [Input], write => [Output], context => pinned_context,
                media_runner_imagemagick => maps:with([major, tool], Installation)},
            ?assertEqual({ok, <<>>}, z_media_runner:run(imagemagick, Cmd ++ " -version", Options)),
            ?assertEqual({ok, Data}, file:read_file(Output)),
            ?assertEqual(2, ets:lookup_element(Calls, downloads, 2)),
            ?assertEqual([], filelib:wildcard(filename:join(filename:dirname(Output), ".download-*")))
        end)
    after
        ets:delete(Calls),
        lists:foreach(fun meck:unload/1, Modules),
        z_media_imagemagick:clear_cache(),
        case Owned of true -> gen_server:stop(Pid); false -> ok end,
        lists:foreach(fun
            ({K, undefined}) -> application:unset_env(zotonic, K);
            ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
        end, Old)
    end.

%% Mock only the fixture: CI also runs site installation and cron processes.
%% Meck's passthrough option does not cover unmatched arguments in a mock fun.
mock_callback_context(TestContext, TestSite, CallbackUrl) ->
    meck:expect(z_context, site, fun
        (Context) when Context =:= TestContext -> TestSite;
        (Context) -> meck:passthrough([Context])
    end),
    meck:expect(z_context, new, fun
        (Site) when Site =:= TestSite -> TestContext;
        (Site) -> meck:passthrough([Site])
    end),
    meck:expect(z_dispatcher, url_for, fun
        (media_runner_callback, _, Context) when Context =:= TestContext -> CallbackUrl;
        (Name, Args, Context) -> meck:passthrough([Name, Args, Context])
    end),
    %% Exercise the calls made by background processes while these mocks are active.
    Context = z_context:new(zotonic_site_testsandbox),
    ?assertEqual(zotonic_site_testsandbox, z_context:site(Context)).

%% Standalone EUnit does not start zotonic_core's sidejob resource.
ensure_sidejobs() ->
    {ok, _} = application:ensure_all_started(sidejob),
    case whereis(zotonic_sidejobs) of
        undefined -> {ok, _} = z_sidejob:init(), ok;
        _ -> ok
    end.
