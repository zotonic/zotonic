%% @copyright 2026 Marc Worrell
%% @doc Registry and Webmachine callback tests without a running site or database.
-module(z_media_runner_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("webzmachine/include/wm_reqdata.hrl").

registry_test_() ->
    {foreach, fun start/0, fun stop/1, [
        fun delivery/0, fun owner_cleanup/0, fun expiry/0, fun bounded_registry/0,
        fun ownership/0, fun status_redaction/0, fun callback_controller/0,
        fun callback_validation/0, fun callback_body_limit/0, fun callback_race/0,
        fun streamed_callback_body/0, fun registry_restart/0, fun callback_decision_flow/0
    ]}.

start() ->
    {ok, Pid} = z_media_runner:start_link(),
    Pid.
stop(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.

delivery() ->
    {ok, Id, Secret} = z_media_runner:register(),
    ?assert(z_media_runner:authorized(Id, Secret)),
    ?assertNot(z_media_runner:authorized(Id, <<"wrong">>)),
    ?assertEqual({error, gone}, z_media_runner:callback(Id, <<"wrong">>, #{})),
    Result = #{<<"status">> => <<"ok">>},
    ?assertEqual(ok, z_media_runner:callback(Id, Secret, Result)),
    receive {media_runner_result, Id, Result} -> ok after 100 -> error(no_result) end,
    ?assertEqual(ok, z_media_runner:callback(Id, Secret, Result)),
    receive {media_runner_result, Id, _} -> error(duplicate) after 0 -> ok end,
    ok = z_media_runner:unregister(Id),
    ?assertNot(z_media_runner:authorized(Id, Secret)),
    ?assertEqual({error, gone}, z_media_runner:callback(Id, Secret, Result)),
    ?assertEqual(#{}, sys:get_state(z_media_runner)).

owner_cleanup() ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Parent ! {job, z_media_runner:register()},
        receive stop -> ok end
    end),
    receive {job, {ok, Id, Secret}} ->
        Pid ! stop,
        receive {'DOWN', Ref, process, Pid, _} -> ok end,
        ?assertNot(z_media_runner:authorized(Id, Secret)),
        await_empty(100)
    end.

await_empty(0) -> ?assertEqual(#{}, sys:get_state(z_media_runner));
await_empty(N) ->
    case sys:get_state(z_media_runner) of
        Jobs when map_size(Jobs) =:= 0 -> ok;
        _ -> receive after 1 -> await_empty(N - 1) end
    end.

expiry() ->
    {ok, Id, Secret} = z_media_runner:register(1),
    await_empty(100),
    ?assertNot(z_media_runner:authorized(Id, Secret)),
    ?assertEqual({error, gone}, z_media_runner:callback(Id, Secret, #{})).

bounded_registry() ->
    Jobs = [begin {ok, Id, _} = z_media_runner:register(), Id end || _ <- lists:seq(1, 1000)],
    ?assertEqual({error, media_runner_busy}, z_media_runner:register()),
    [First | _] = Jobs,
    ok = z_media_runner:unregister(First),
    ?assertMatch({ok, _, _}, z_media_runner:register()).

ownership() ->
    {ok, Id, Secret} = z_media_runner:register(),
    Parent = self(),
    spawn(fun() -> Parent ! {removed, z_media_runner:unregister(Id)} end),
    receive {removed, Reply} -> ?assertEqual({error, not_owner}, Reply) end,
    ?assert(z_media_runner:authorized(Id, Secret)).

status_redaction() ->
    {ok, _Id, Secret} = z_media_runner:register(),
    Status = iolist_to_binary(io_lib:format("~p", [sys:get_status(z_media_runner)])),
    ?assertEqual(nomatch, binary:match(Status, Secret)),
    ?assertEqual([{data, [{"State", redacted}]}],
        z_media_runner:format_status(terminate, [[], #{secret => Secret}])).

callback_controller() ->
    {ok, Id, Secret} = z_media_runner:register(),
    RD = request(Id, Secret, <<"{}">>),
    {true, RD1, undefined} = controller_media_runner_callback:service_available(RD, undefined),
    ?assertEqual("no-store", wrq:get_resp_header("cache-control", RD1)),
    {true, RD2, State} = controller_media_runner_callback:is_authorized(RD1, undefined),
    ?assertMatch({{halt, 204}, _, undefined}, controller_media_runner_callback:process_post(RD2, State)),
    receive {media_runner_result, Id, Result} -> ?assertEqual(#{}, Result) after 100 -> error(no_result) end,
    ?assertMatch({{halt, 204}, _, _}, controller_media_runner_callback:process_post(RD2, State)),
    receive {media_runner_result, Id, _} -> error(duplicate) after 0 -> ok end.

callback_decision_flow() ->
    {ok, Id, Secret} = z_media_runner:register(),
    RD = request(Id, Secret, <<"{}">>),
    Controller = #wm_controller{mod=controller_media_runner_callback,
        mod_state=undefined, trace=false},
    {_, _, FinalRD} = webmachine_decision_core:handle_request(Controller, RD),
    ?assertEqual(204, wrq:response_code(FinalRD)),
    receive {media_runner_result, Id, #{}} -> ok after 100 -> error(no_result) end.

callback_validation() ->
    {ok, Id, Secret} = z_media_runner:register(),
    %% No socket/body: any attempt to parse the POST body during auth would fail.
    ?assertMatch({{halt, 401}, _, _}, controller_media_runner_callback:is_authorized(
        wrq:create(undefined, 'POST', https, {1,1}, "/media-runner/callback", mochiweb_headers:empty()), undefined)),
    ?assertMatch({{halt, 410}, _, _}, controller_media_runner_callback:is_authorized(
        request(Id, <<"wrong">>, not_fetched_yet), undefined)),
    {true, _, _} = controller_media_runner_callback:is_authorized(
        request(Id, Secret, not_fetched_yet), undefined),
    lists:foreach(fun(Body) ->
        RD = request(Id, Secret, Body),
        {true, _, State} = controller_media_runner_callback:is_authorized(RD, undefined),
        ?assertMatch({{halt, 400}, _, _}, controller_media_runner_callback:process_post(RD, State))
    end, [<<"{">>, <<"[]">>, <<"null">>]),
    RD = wrq:create(undefined, 'POST', https, {1,1}, "/media-runner/callback",
        mochiweb_headers:make([{"content-type", "text/plain"}])),
    ?assertMatch({{halt, 415}, _, _}, controller_media_runner_callback:process_post(RD, {Id, Secret})).

callback_body_limit() ->
    {ok, Id, Secret} = z_media_runner:register(),
    Saved = application:get_env(zotonic, media_runner_max_callback_bytes),
    try
        application:set_env(zotonic, media_runner_max_callback_bytes, 2),
        ?assertMatch({{halt, 413}, _, _}, controller_media_runner_callback:process_post(
            request(Id, Secret, <<"{  }">>), {Id, Secret})),
        ?assertMatch({{halt, 204}, _, _}, controller_media_runner_callback:process_post(
            request(Id, Secret, <<"{}">>), {Id, Secret}))
    after restore(media_runner_max_callback_bytes, Saved)
    end.

callback_race() ->
    {ok, Id, Secret} = z_media_runner:register(),
    RD = request(Id, Secret, <<"{}">>),
    {true, _, State} = controller_media_runner_callback:is_authorized(RD, undefined),
    ok = z_media_runner:unregister(Id),
    ?assertMatch({{halt, 410}, _, _}, controller_media_runner_callback:process_post(RD, State)).

%% Exercise Webmachine's actual socket reader, including chunked bodies without
%% a Content-Length header. No HTTP server, site, database or remote runner needed.
streamed_callback_body() ->
    Saved = application:get_env(zotonic, media_runner_max_callback_bytes),
    try
        application:set_env(zotonic, media_runner_max_callback_bytes, 2),
        socket_callback([{"content-length", "2"}], <<"{}">>, 204),
        socket_callback([{"content-length", "4"}], <<"{  }">>, 413),
        socket_callback([{"transfer-encoding", "chunked"}], <<"2\r\n{}\r\n0\r\n\r\n">>, 204),
        socket_callback([{"transfer-encoding", "chunked"}], <<"4\r\n{  }\r\n0\r\n\r\n">>, 413)
    after restore(media_runner_max_callback_bytes, Saved)
    end.

socket_callback(Headers, Body, Status) ->
    {ok, Id, Secret} = z_media_runner:register(),
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127,0,0,1}}]),
    try
        {ok, {_, Port}} = inet:sockname(Listen),
        {ok, Client} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}], 1000),
        try
            {ok, Socket} = gen_tcp:accept(Listen, 1000),
            try
                ok = gen_tcp:send(Client, Body),
                RD = wrq:create(Socket, 'POST', http, {1,1}, "/media-runner/callback",
                    mochiweb_headers:make([{"content-type", "application/json"} | Headers])),
                ?assertMatch({{halt, Status}, _, _},
                    controller_media_runner_callback:process_post(RD, {Id, Secret}))
            after gen_tcp:close(Socket)
            end
        after gen_tcp:close(Client)
        end
    after
        gen_tcp:close(Listen),
        z_media_runner:unregister(Id),
        erase(mochiweb_request_recv),
        erase(mochiweb_request_body_length)
    end.

registry_restart() ->
    {ok, Id, Secret} = z_media_runner:register(),
    OldPid = whereis(z_media_runner),
    ok = gen_server:stop(OldPid),
    {ok, NewPid} = z_media_runner:start_link(),
    try
        ?assertNot(z_media_runner:authorized(Id, Secret)),
        ?assertEqual({error, gone}, z_media_runner:callback(Id, Secret, #{}))
    after gen_server:stop(NewPid)
    end.

request(Id, Secret, Body) ->
    RD = wrq:create(undefined, 'POST', https, {1,1},
        "/media-runner/callback?id=" ++ binary_to_list(Id),
        mochiweb_headers:make([{ "authorization", "Bearer " ++ binary_to_list(Secret)},
            {"content-type", "application/json"}, {"accept", "application/json"}])),
    wrq:set_req_body(Body, RD).

restore(Key, undefined) -> application:unset_env(zotonic, Key);
restore(Key, {ok, Value}) -> application:set_env(zotonic, Key, Value).

configuration_limits_test() ->
    Saved = application:get_env(zotonic, media_runner_wait_timeout),
    try
        application:set_env(zotonic, media_runner_wait_timeout, infinity),
        ?assertEqual({error, media_runner_configuration}, z_media_runner:register()),
        ?assertEqual({error, media_runner_configuration}, z_media_runner:register(0))
    after restore(media_runner_wait_timeout, Saved)
    end,
    ?assert(z_media_runner_protocol:https_url(<<"https://example.com/media-runner/callback">>)),
    ?assertNot(z_media_runner_protocol:https_url(<<"http://example.com/media-runner/callback">>)),
    ?assertNot(z_media_runner_protocol:https_url(<<"https://user:pass@example.com/callback">>)),
    ?assertEqual({error, media_runner_configuration}, z_media_runner:callback_url(undefined)).
