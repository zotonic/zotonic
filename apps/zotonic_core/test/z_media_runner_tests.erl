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
        [Out] = [F || F <- Files, maps:get(<<"write">>, F)],
        ?assertEqual(<<".png">>, maps:get(<<"extension">>, Out)),
        {ok, _} = z_media_runner_protocol:unpack(
            #{
                <<"status">> => <<"ok">>,
                <<"stdout">> => base64:encode(<<"__ZMR_FILE_1__ PNG">>),
                <<"files">> => [
                    #{
                        <<"id">> => maps:get(<<"id">>, Out),
                        <<"data">> => base64:encode(<<"output">>)
                    }
                ]
            },
            Options
        ),
        ?assertEqual({ok, <<"output">>}, file:read_file(Output))
    end).

%% Opt-in because a real OS sandbox cannot be nested in every test environment.
sandbox_roundtrip_test_() ->
    case os:getenv("ZOTONIC_SANDBOX_TESTS") of
        "1" ->
            {timeout, 60, fun() ->
                {ok, _} = application:ensure_all_started(erlexec),
                %% argv conversion must not encode UTF-8 command text twice.
                Utf8 = <<"café"/utf8>>,
                ?assertEqual(
                    {ok, Utf8},
                    z_exec:run_sandbox(file, ["printf %s ", Utf8], #{})
                ),
                with_files(fun(Input, Output) ->
                    Options = #{read => [Input], write => [Output]},
                    Cmd = [
                        "magick ", z_filelib:os_filename(Input), " ", z_filelib:os_filename(Output)
                    ],
                    {ok, Packed} = z_media_runner_protocol:pack(imagemagick, Cmd, Options),
                    Result = z_media_runner_protocol:execute(Packed),
                    ?assertMatch(#{<<"status">> := <<"ok">>}, Result),
                    ?assertMatch({ok, _}, z_media_runner_protocol:unpack(Result, Options)),
                    {ok, Png} = file:read_file(Output),
                    ?assertMatch(<<137, "PNG", _/binary>>, Png),
                    %% ImageMagick's output path is restored for z_media_identify's parser.
                    {ok, Identify} = z_media_runner_protocol:pack(
                        imagemagick,
                        ["magick identify ", z_filelib:os_filename(Input ++ "[0]")],
                        #{read => [Input]}
                    ),
                    {ok, Stdout} = z_media_runner_protocol:unpack(
                        z_media_runner_protocol:execute(Identify), #{read => [Input]}
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

job() ->
    #{
        <<"version">> => 1,
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
