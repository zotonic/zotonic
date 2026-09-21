-module(z_clamav_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% Exercise transport selection with real sockets, without a running clamd.
socket_selection_test() ->
    Path = "/tmp/z_clamav_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".sock",
    Keys = [clamav_socket, clamav_ip, clamav_port],
    Saved = [{Key, application:get_env(zotonic, Key)} || Key <- Keys],
    {ok, Tcp} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, {_, Port}} = inet:sockname(Tcp),
    try
        application:set_env(zotonic, clamav_socket, list_to_binary(Path)),
        application:set_env(zotonic, clamav_ip, "127.0.0.1"),
        application:set_env(zotonic, clamav_port, Port),
        %% A missing socket uses TCP.
        check_ping(Tcp),
        {ok, Local} = listen_local(Path),
        try
            %% A socket appearing between calls immediately takes precedence.
            check_ping(Local),
            AvailabilityRef = reply(Local, <<"PONG\n">>),
            ?assertEqual({ok, {true, []}}, availability()),
            ?assertEqual(<<"PING\n">>, request(AvailabilityRef)),
            Ref = reply(Local, <<"stream: OK", 0>>),
            ?assertEqual(ok, z_clamav:scan(<<"test">>)),
            ?assertEqual(<<"zINSTREAM", 0, 4:32, "test", 0:32>>, request(Ref)),
            application:set_env(zotonic, clamav_socket, false),
            check_ping(Tcp),
            application:set_env(zotonic, clamav_socket, Path),
            check_ping(Local)
        after
            gen_tcp:close(Local)
        end,
        %% A stale socket also falls back to TCP.
        check_ping(Tcp),
        file:delete(Path),
        check_ping(Tcp),
        gen_tcp:close(Tcp),
        ?assertEqual({ok, {false, []}}, availability())
    after
        gen_tcp:close(Tcp),
        file:delete(Path),
        lists:foreach(fun restore_env/1, Saved)
    end.

availability() ->
    m_clamav:m_get([<<"is_available">>], undefined, #context{acl = admin}).

listen_local(Path) ->
    gen_tcp:listen(0, [binary, {active, false}, {ifaddr, {local, Path}}]).

check_ping(Listener) ->
    Ref = reply(Listener, <<"PONG\n">>),
    ?assertEqual(pong, z_clamav:ping()),
    ?assertEqual(<<"PING\n">>, request(Ref)).

reply(Listener, Reply) ->
    Parent = self(),
    Ref = make_ref(),
    spawn_link(fun() ->
        {ok, Socket} = gen_tcp:accept(Listener, 1000),
        try
            Request = receive_request(Socket, <<>>),
            ok = gen_tcp:send(Socket, Reply),
            Parent ! {Ref, Request}
        after
            gen_tcp:close(Socket)
        end
    end),
    Ref.

receive_request(_Socket, <<"PING\n">> = Data) -> Data;
receive_request(_Socket, <<"zINSTREAM", 0, 4:32, "test", 0:32>> = Data) -> Data;
receive_request(Socket, Acc) ->
    {ok, Data} = gen_tcp:recv(Socket, 0, 1000),
    receive_request(Socket, <<Acc/binary, Data/binary>>).

request(Ref) ->
    receive
        {Ref, Data} -> Data
    after 1000 ->
        error(request_timeout)
    end.

restore_env({Key, undefined}) -> application:unset_env(zotonic, Key);
restore_env({Key, {ok, Value}}) -> application:set_env(zotonic, Key, Value).
