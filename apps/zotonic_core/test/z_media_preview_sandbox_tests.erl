%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
-module(z_media_preview_sandbox_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

%% Exercise the public conversion API with a paused decoder, keeping the real
%% filesystem and gproc coordination. No ImageMagick or sandbox is needed.
publication_test_() ->
    {timeout, 30, fun publication_checks/0}.

publication_checks() ->
    {ok, _} = application:ensure_all_started(gproc),
    Modules = [z_exec, z_media_identify, jobs],
    lists:foreach(fun(M) -> ok = meck:new(M, [passthrough, no_link]) end, Modules),
    Dir = z_convert:to_list(z_tempfile:new()) ++ "-preview-test",
    ok = file:make_dir(Dir),
    Input = filename:join(Dir, "input.jpg"),
    Output = filename:join(Dir, "output.jpg"),
    Props = #{ <<"mime">> => <<"image/jpeg">>, <<"width">> => 16, <<"height">> => 16 },
    Parent = self(),
    %% Prevent executable discovery from depending on ImageMagick installation.
    CmdKey = {z_media_preview, imagemagick_find_executable},
    OldCmd = persistent_term:get(CmdKey, undefined),
    persistent_term:put(CmdKey, #{cmd => "convert", legacy => true}),
    try
        ok = meck:expect(z_media_identify, identify, fun(_, _, _, _) -> {ok, Props} end),
        ok = meck:expect(jobs, run, fun(media_preview_jobs, F) ->
            Parent ! {entered, self()}, F()
        end),
        ok = meck:expect(z_exec, run, fun(imagemagick, _, #{write := [Temp]}) ->
            ok = file:write_file(Temp, <<"partial">>),
            Parent ! {decoding, self(), Temp},
            receive
                finish ->
                    ok = file:write_file(Temp, <<"complete">>),
                    {ok, <<>>};
                fail -> {error, decoder_failed};
                empty ->
                    ok = file:write_file(Temp, <<>>),
                    {ok, <<>>}
            after 5000 -> error(decoder_wait_timeout)
            end
        end),
        First = start_convert(Input, Output),
        Temp = decoding(First),
        ?assertNot(filelib:is_file(Output)),
        Second = start_convert(Input, Output),
        receive {entered, Second} -> ok after 5000 -> error(waiter_not_started) end,
        %% Neither request can return while the decoder is paused.
        receive {finished, _, _} -> error(premature_result) after 50 -> ok end,
        ?assertNot(filelib:is_file(Output)),
        ?assertEqual({ok, <<"partial">>}, file:read_file(Temp)),
        First ! finish,
        ?assertEqual(ok, finished(First)),
        ?assertEqual(ok, finished(Second)),
        ?assertEqual({ok, <<"complete">>}, file:read_file(Output)),
        ?assertNot(filelib:is_file(Temp)),
        ?assertEqual(1, meck:num_calls(z_exec, run, '_')),
        %% Existing published files are not deleted or regenerated.
        Third = start_convert(Input, Output),
        ?assertEqual(ok, finished(Third)),
        ?assertEqual(1, meck:num_calls(z_exec, run, '_')),
        ok = file:delete(Output),
        Failed = start_convert(Input, Output),
        FailedTemp = decoding(Failed),
        Retry = start_convert(Input, Output),
        receive {entered, Retry} -> ok after 5000 -> error(waiter_not_started) end,
        Failed ! fail,
        ?assertMatch({error, _}, finished(Failed)),
        _ = decoding(Retry),
        ?assertNot(filelib:is_file(Output)),
        ?assertNot(filelib:is_file(FailedTemp)),
        Retry ! finish,
        ?assertEqual(ok, finished(Retry)),
        ok = file:delete(Output),
        Empty = start_convert(Input, Output),
        EmptyTemp = decoding(Empty),
        Empty ! empty,
        ?assertEqual({error, convert_error}, finished(Empty)),
        ?assertNot(filelib:is_file(Output)),
        ?assertNot(filelib:is_file(EmptyTemp))
    after
        lists:foreach(fun meck:unload/1, Modules),
        case OldCmd of
            undefined -> persistent_term:erase(CmdKey);
            _ -> persistent_term:put(CmdKey, OldCmd)
        end,
        file:del_dir_r(Dir)
    end.

start_convert(Input, Output) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = z_media_preview:convert(Input, Output, [], #context{site = zotonic_core}),
        Parent ! {finished, self(), Result}
    end).

decoding(Pid) ->
    receive {decoding, Pid, Temp} -> Temp after 5000 -> error(decoder_not_started) end.

finished(Pid) ->
    receive {finished, Pid, Result} -> Result after 5000 -> error(conversion_not_finished) end.
