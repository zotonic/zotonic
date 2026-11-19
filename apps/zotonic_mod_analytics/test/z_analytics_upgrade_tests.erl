%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Regression tests for budget-aware traffic recording upgrades.
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

-module(z_analytics_upgrade_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

upgrade_test_() ->
    z_test_support:isolated({setup, fun setup/0, fun cleanup/1, fun upgrade/0}).

setup() ->
    meck:new(z_db, [passthrough, no_link]),
    meck:new(m_config, [passthrough, no_link]),
    meck:expect(z_db, transaction, fun(F, C) -> F(C) end),
    meck:expect(z_db, qmap, fun(_, _) -> {ok, get(rows)} end),
    meck:expect(z_db, q, fun("select id from analytics_budget where id=1 for update", _) ->
        put(locked, true), []
    end),
    meck:expect(z_db, q, fun("update analytics_collector set definition=$2 where id=$1", [Id, D], _) ->
        ?assertEqual(true, get(locked)),
        put(updates, get(updates) ++ [{Id, z_analytics:decode(D)}]), 1
    end),
    meck:expect(m_config, get_value, fun(mod_analytics, row_budget, _) -> get(budget) end),
    ok.

cleanup(_) ->
    meck:unload(m_config),
    meck:unload(z_db).

upgrade() ->
    Definitions = [
        definition(<<"page_request">>, <<"traffic">>, 4),
        definition(<<"api_request">>, <<"traffic">>, 3),
        definition(<<"request">>, <<"traffic">>, 4),
        definition(<<"request">>, <<"traffic">>, 6),
        definition(<<"page_request">>, <<"language">>, 4),
        definition(<<"custom:event">>, <<"traffic">>, 4)
    ],
    Rows = [#{<<"id">> => I, <<"name">> => <<"Renamed recording">>,
        <<"definition">> => z_analytics:encode(D), <<"state">> => State, <<"started">> => 100}
        || {I, D, State} <- lists:zip3(lists:seq(1,6), Definitions,
            [<<"active">>, <<"paused">>, <<"draft">>, <<"active">>, <<"active">>, <<"active">>])],
    Reserved = 3578 * lists:sum([maps:get(groups,D) || D <- Definitions]),
    ?assertEqual([], run(Rows, Reserved)),
    %% The second recording needs two slots and is skipped; the third still fits.
    Limited = run(Rows, Reserved + 2*3578),
    ?assertEqual([1,3], [Id || {Id,_} <- Limited]),
    All = run(Rows, Reserved + 4*3578),
    ?assertEqual([1,2,3], [Id || {Id,_} <- All]),
    lists:foreach(fun({Id,D}) ->
        ?assertEqual(5, maps:get(groups,D)),
        ?assertEqual((lists:nth(Id,Definitions))#{groups => 5, name => <<"Renamed recording">>}, D)
    end, All),
    UpdatedRows = [case lists:keyfind(maps:get(<<"id">>,R),1,All) of
        false -> R;
        {_,D} -> R#{<<"definition">> => z_analytics:encode(D)}
    end || R <- Rows],
    ?assertEqual([], run(UpdatedRows, Reserved + 4*3578)),
    ?assertEqual([], run([], 1000000)).

run(Rows, Budget) ->
    put(rows, Rows), put(budget, Budget), put(updates, []), put(locked, false),
    ?assertEqual(ok, m_analytics:store_upgrade_traffic_groups(#context{})),
    get(updates).

definition(Event, Dimension, Groups) ->
    #{name => <<"Original name">>, description => <<"Keep this description">>,
      event => Event, schema_version => 1, dimension => Dimension, groups => Groups,
      filters => #{<<"language">> => [<<"nl">>]}}.
