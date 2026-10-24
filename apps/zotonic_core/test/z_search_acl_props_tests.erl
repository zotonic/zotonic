%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test coarse query privacy policies and SQL guard construction.
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

-module(z_search_acl_props_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

policy_and_sql_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(query_property_policy_test),
    Member = Context#context{user_id = 123},
    Admin = z_acl:sudo(Context),
    ?assertEqual(allow, z_search_acl_props:policy(<<"email">>, Context)),
    ?assertEqual({<<>>, []}, z_search_acl_props:sql(<<"rsc">>, [<<"email">>], [], Context)),
    ?assertEqual(allow, z_search_acl_props:policy(<<"email">>, Admin)),
    ok = z_notifier:observe(acl_query_prop,
        {mod_acl_user_groups, observe_acl_query_prop}, 100, Context),
    ok = meck:new(m_category, [passthrough, no_link]),
    ok = meck:expect(m_category, contains, fun
        (person, C) when C =:= Context; C =:= Member -> [10, 11];
        (Cat, C) -> meck:passthrough([Cat, C])
    end),
    try
        ?assertMatch({privacy, 0},
            z_search_acl_props:policy(<<"email">>, Context)),
        ?assertMatch({privacy, 10},
            z_search_acl_props:policy(<<"email">>, Member)),
        ?assertEqual(deny, z_search_acl_props:policy(<<"billing_email">>, Member)),
        ?assertEqual(allow, z_search_acl_props:policy(<<"title">>, Context)),
        lists:foreach(fun(Source) ->
            ?assertEqual({<<>>, []}, z_search_acl_props:sources_sql(<<"rsc">>, [Source], [], Context))
        end, [{column, <<"search_facet">>, <<"f_email">>},
              {column, <<"pivot_custom">>, <<"email">>},
              {column, <<"rsc">>, <<"pivot_tsv">>},
              {column, <<"rsc">>, <<"pivot_rtsv">>}]),
        lists:foreach(fun(Column) ->
            {Guard, _} = z_search_acl_props:sources_sql(<<"rsc">>,
                [{column, <<"rsc">>, Column}], [], Context),
            ?assertNotEqual(<<>>, Guard),
            ?assertNotEqual(<<"false">>, Guard)
        end, [<<"pivot_location_lat">>, <<"pivot_date_end_month_day">>, <<"pivot_street">>, <<"pivot_city">>, <<"pivot_postcode">>]),
        ?assertEqual({<<>>, [existing]},
            z_search_acl_props:sql(<<"rsc">>, [<<"email">>], [existing], Admin)),
        ?assertEqual({<<"false">>, []},
            z_search_acl_props:sql(<<"rsc">>, [<<"billing_email">>], [], Member)),
        {Sql, Args} = z_search_acl_props:sql(<<"rsc">>,
            [<<"email">>, <<"email">>, <<"phone">>, <<"title">>], [existing], Member),
        ?assertEqual([existing, 10], Args),
        ?assertNotEqual(nomatch, binary:match(Sql, <<"BETWEEN 0 AND $2::integer">>)),
        ?assertEqual(nomatch, binary:match(Sql, <<"category_id">>)),
        Existing = [first, 10, last, 10],
        {Reused, Existing} = z_search_acl_props:sql(<<"other">>, [<<"email">>], Existing, Member),
        ?assertEqual(<<"(\"other\".privacy BETWEEN 0 AND $2::integer)">>, Reused),
        {Repeated, Args} = z_search_acl_props:sql(<<"other">>, [<<"email">>], Args, Member),
        ?assertEqual(Reused, Repeated),
        % Matching must be exact: a float cannot stand in for an integer level.
        {Distinct, DistinctArgs} = z_search_acl_props:sql(<<"rsc">>, [<<"email">>], [10.0], Member),
        ?assertEqual([10.0, 10], DistinctArgs),
        ?assertNotEqual(nomatch, binary:match(Distinct, <<"BETWEEN 0 AND $2::integer">>)),
        {Quoted, _} = z_search_acl_props:sql(<<"odd\"alias">>, [<<"email">>], [], Context),
        ?assertNotEqual(nomatch, binary:match(Quoted, <<"\"odd\"\"alias\".privacy">>))
    after
        meck:unload(m_category),
        z_notifier:detach(acl_query_prop, Context)
    end.
