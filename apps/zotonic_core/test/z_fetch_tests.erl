%% @copyright 2026 Marc Worrell

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

-module(z_fetch_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

%% These tests mock shared core modules. Run in a separate VM so that loading
%% and unloading mocks cannot crash the site used by the integration suite.
-export([
    start_dependencies/0,
    destination_options_check/0,
    anonymous_options_check/0,
    caller_callback_check/0,
    fetch_entry_points_check/0,
    cross_origin_options_check/0,
    contextless_fetch_check/0
]).

isolated_test_() ->
    {setup,
        fun() ->
            {ok, Peer, _} = peer:start_link(#{
                connection => standard_io,
                args => ["-pa" | code:get_path()]
            }),
            ok = peer:call(Peer, ?MODULE, start_dependencies, []),
            Peer
        end,
        fun peer:stop/1,
        fun(Peer) ->
            [{atom_to_list(Test), {timeout, 30, fun() ->
                peer:call(Peer, ?MODULE, Test, [], 30000)
            end}} || Test <- [
        destination_options_check,
        anonymous_options_check,
        caller_callback_check,
        fetch_entry_points_check,
        cross_origin_options_check,
        contextless_fetch_check
            ]]
        end}.

start_dependencies() ->
    {ok, _} = application:ensure_all_started(mimetypes),
    ok.


destination_options_check() ->
    with_fetch(fun(Context) ->
        meck:expect(z_notifier, first, fun(#url_fetch_options{host = Host, options = Options}, FetchContext) ->
            ?assertEqual(42, FetchContext#context.user_id),
            ?assertEqual(true, proplists:get_value(insecure, Options)),
            ?assertEqual(en, proplists:get_value(language, Options)),
            [{authorization, <<"Bearer ", Host/binary>>} | Options]
        end),
        meck:expect(z_url_fetch, fetch, fun(Url, Options) ->
            ?assertEqual(undefined, proplists:get_value(authorization, Options)),
            First = resolve_options(get, Url, Options),
            Second = resolve_options(get, <<"https://two.test:8443/end">>, Options),
            ?assertEqual(<<"Bearer one.test">>, proplists:get_value(authorization, First)),
            ?assertEqual(<<"Bearer two.test:8443">>, proplists:get_value(authorization, Second)),
            success(Url)
        end),
        ?assertMatch({ok, _}, z_fetch:fetch(<<"https://one.test/start">>, [], Context))
    end).

anonymous_options_check() ->
    with_fetch(fun(Context) ->
        meck:expect(z_notifier, first, fun(#url_fetch_options{options = Options}, _) ->
            ?assertEqual(none, proplists:get_value(authorization, Options)),
            undefined
        end),
        meck:expect(z_url_fetch, fetch, fun(Url, Options) ->
            ?assertEqual(none, proplists:get_value(authorization, Options)),
            lists:foreach(fun(Destination) ->
                Resolved = resolve_options(get, Destination, Options),
                ?assertEqual(none, proplists:get_value(authorization, Resolved))
            end, [Url, <<"https://two.test/end">>]),
            success(Url)
        end),
        ?assertMatch({ok, _}, z_fetch:fetch(<<"https://one.test/start">>, [{authorization, none}], Context))
    end).

caller_callback_check() ->
    with_fetch(fun(Context) ->
        CallerOptionsFun = fun(get, Url, Options) -> [{accept, Url} | Options] end,
        meck:expect(z_url_fetch, fetch, fun(Url, Options) ->
            Resolved = resolve_options(get, Url, Options),
            ?assertEqual(Url, proplists:get_value(accept, Resolved)),
            ?assertEqual(true, proplists:get_value(insecure, Resolved)),
            success(Url)
        end),
        ?assertMatch({ok, _}, z_fetch:fetch(<<"https://one.test/start">>, [{options_fun, CallerOptionsFun}], Context))
    end).

fetch_entry_points_check() ->
    with_fetch(fun(Context) ->
        Reply = fun(Method, Url, Options) ->
            Resolved = resolve_options(Method, Url, Options),
            ?assertEqual(true, proplists:get_value(insecure, Resolved)),
            {ok, {binary_to_list(Url), [], 2, <<"{}">>}}
        end,
        meck:expect(z_url_fetch, fetch, fun(Url, Options) -> Reply(get, Url, Options) end),
        meck:expect(z_url_fetch, fetch, fun(Method, Url, _, Options) -> Reply(Method, Url, Options) end),
        meck:expect(z_url_fetch, fetch_partial, fun(Url, Options) -> Reply(get, Url, Options) end),
        Url = <<"https://one.test/start">>,
        ?assertMatch({ok, _}, z_fetch:fetch(Url, [], Context)),
        ?assertMatch({ok, _}, z_fetch:fetch(post, Url, <<"payload">>, [], Context)),
        ?assertMatch({ok, _}, z_fetch:fetch_partial(Url, [], Context)),
        ?assertEqual({ok, #{}}, z_fetch:fetch_json(Url, [], Context)),
        ?assertEqual({ok, #{}}, z_fetch:fetch_json(get, Url, <<>>, [], Context)),
        %% Metadata also passes through the stdlib's fetch_partial entry point.
        ?assertMatch({ok, _}, z_fetch:metadata(Url, [], Context))
    end).

%% Exercise the installed stdlib's redirect callback with real HTTP requests.
cross_origin_options_check() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    Modules = [z_notifier, m_site, z_context],
    lists:foreach(fun(Module) -> meck:new(Module, [no_link]) end, Modules),
    try
        meck:expect(m_site, environment, fun(_) -> production end),
        meck:expect(z_context, language, fun(_) -> en end),
        lists:foreach(fun(Override) ->
            meck:expect(z_notifier, first,
                fun(#url_fetch_options{host = Host, options = Options}, _) ->
                    case binary:split(Host, <<":">>) of
                        [<<"localhost">>, _] ->
                            [{authorization, <<"Bearer first">>} | Options];
                        [<<"127.0.0.1">>, _] ->
                            ?assertEqual(none, proplists:get_value(authorization, Options)),
                            case Override of
                                true -> [{authorization, <<"Bearer second">>} | Options];
                                false -> Options
                            end
                    end
                end),
            {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {packet, http_bin}]),
            {ok, {_, Port}} = inet:sockname(Listen),
            {Server, Monitor} = spawn_monitor(fun() ->
                {ok, First} = gen_tcp:accept(Listen, 2000),
                ?assertEqual(<<"Bearer first">>, request_authorization(First, undefined)),
                ok = gen_tcp:send(First, ["HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:",
                    integer_to_list(Port), "/end\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"]),
                gen_tcp:close(First),
                {ok, Second} = gen_tcp:accept(Listen, 2000),
                Expected = case Override of true -> <<"Bearer second">>; false -> undefined end,
                ?assertEqual(Expected, request_authorization(Second, undefined)),
                ok = gen_tcp:send(Second, "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"),
                gen_tcp:close(Second)
            end),
            try
                Url = "http://localhost:" ++ integer_to_list(Port) ++ "/start",
                ?assertMatch({ok, {_, _, 2, <<"ok">>}}, z_fetch:fetch(Url,
                    [{authorization_redirect, none}, {use_range, false}], #context{})),
                receive
                    {'DOWN', Monitor, process, Server, Reason} -> ?assertEqual(normal, Reason)
                after 3000 -> error(server_timeout)
                end
            after gen_tcp:close(Listen) end
        end, [false, true])
    after meck:unload(Modules) end.

request_authorization(Socket, Authorization) ->
    case gen_tcp:recv(Socket, 0, 2000) of
        {ok, http_eoh} -> Authorization;
        {ok, {http_header, _, 'Authorization', _, Value}} -> request_authorization(Socket, Value);
        {ok, _} -> request_authorization(Socket, Authorization)
    end.

contextless_fetch_check() ->
    meck:new(z_url_fetch, [no_link]),
    try
        meck:expect(z_url_fetch, fetch, fun(Url, Options) ->
            ?assertEqual([{authorization, none}, {autoredirect, false}], Options),
            success(Url)
        end),
        ?assertMatch({ok, _}, z_fetch:fetch(<<"https://one.test/start">>,
            [{authorization, none}, {autoredirect, false}], undefined))
    after meck:unload(z_url_fetch) end.

resolve_options(Method, Url, Options) ->
    OptionsFun = proplists:get_value(options_fun, Options),
    OptionsFun(Method, Url, proplists:delete(options_fun, Options)).

with_fetch(Fun) ->
    Modules = [z_url_fetch, z_notifier, m_site, z_context],
    lists:foreach(fun(Module) -> meck:new(Module, [no_link]) end, Modules),
    try
        meck:expect(m_site, environment, fun(_) -> development end),
        meck:expect(z_context, language, fun(_) -> en end),
        meck:expect(z_notifier, first, fun(_, _) -> undefined end),
        Fun(#context{user_id = 42})
    after meck:unload(Modules) end.

success(Url) -> {ok, {binary_to_list(Url), [], 2, <<"ok">>}}.
