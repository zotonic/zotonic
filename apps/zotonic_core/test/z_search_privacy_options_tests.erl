%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Verify the Erlang-only property privacy search option and model boundary.
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

-module(z_search_privacy_options_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe_search_query/2]).

trusted_options_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(search_privacy_options_fixture),
    z_notifier:observe(search_query, {?MODULE, observe_search_query}, 100, C),
    Args = #{<<"page">> => 1, <<"pagelen">> => 20},
    Name = <<"privacy_options_fixture">>,
    try
        #search_result{options = Trusted} = z_search:search(Name, Args, 1, 20,
            #{no_privacy_check => true}, C),
        ?assertEqual(true, maps:get(no_privacy_check, Trusted)),
        lists:foreach(fun(Option) ->
            ?assertEqual(#{}, z_search:map_to_options(Option)),
            Input = Args#{<<"options">> => Option, <<"no_privacy_check">> => true},
            #search_result{options = Embedded} = z_search:search(Name, Input, 1, 20, C),
            ?assertNot(maps:is_key(no_privacy_check, Embedded)),
            lists:foreach(fun(Path) ->
                {ok, {#search_result{options = External}, []}} = m_search:m_get(
                    Path, #{payload => Input}, C),
                ?assertNot(maps:is_key(no_privacy_check, External))
            end, [[Name], [<<"paged">>, Name], [<<"count">>, Name]])
        end, [#{no_privacy_check => true},
              #{<<"no_privacy_check">> => true}]),
        lists:foreach(fun(Value) ->
            #search_result{options = O} = z_search:search(Name, Args, 1, 20,
                #{no_privacy_check => Value}, C),
            ?assertNot(maps:is_key(no_privacy_check, O))
        end, [false, <<"true">>, 1])
    after
        z_notifier:detach(search_query, C)
    end.

nested_guards_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(search_privacy_compiler_fixture),
    z_notifier:observe(acl_query_prop, {mod_acl_user_groups, observe_acl_query_prop}, 100, C),
    Term = #search_sql_term{where = <<"rsc.id > $1">>, args = [7],
        property_sources = [{<<"rsc">>, {jsonb, <<"rsc">>, <<"props_json">>, <<"email">>}}]},
    Query = #search_sql_terms{terms = [#search_sql_nested{terms = [Term]}]},
    try
        Normal = z_search_terms:combine(Query, #{}, C),
        Trusted = z_search_terms:combine(Query, #{no_privacy_check => true}, C),
        External = z_search_terms:combine(Query, #{<<"no_privacy_check">> => true}, C),
        ?assertNotEqual(nomatch, binary:match(Normal#search_sql.where, <<".privacy">>)),
        ?assertEqual(Normal, External),
        ?assertEqual(nomatch, binary:match(Trusted#search_sql.where, <<".privacy">>)),
        ?assertNotEqual(nomatch, binary:match(Trusted#search_sql.where, <<"rsc.id > $1">>)),
        ?assertEqual([7], Trusted#search_sql.args),
        ?assertEqual(Normal#search_sql.tables, Trusted#search_sql.tables),
        ?assertEqual(Normal#search_sql.extra, Trusted#search_sql.extra),
        ?assertEqual(Normal, z_search_terms:combine(Query, #{}, C))
    after
        z_notifier:detach(acl_query_prop, C)
    end.

observe_search_query(#search_query{name = <<"privacy_options_fixture">>}, _) ->
    #search_result{result = [], total = 0};
observe_search_query(_, _) -> undefined.
