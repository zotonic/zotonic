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

destination_options_test() ->
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

anonymous_options_test() ->
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

caller_callback_test() ->
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

fetch_entry_points_test() ->
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

contextless_fetch_test() ->
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
