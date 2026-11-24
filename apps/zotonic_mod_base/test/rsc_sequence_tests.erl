%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for ordered navigation through menus and collections.
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

-module(rsc_sequence_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

navigation_test_() ->
    z_test_support:isolated(fun navigation/0).

navigation() ->
    Context = #context{},
    Modules = [m_rsc, m_edge, z_acl],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(m_rsc, rid, fun
            (undefined, _) -> undefined;
            (<<"root">>, _) -> 1;
            (Id, _) -> Id
        end),
        meck:expect(m_rsc, exists, fun(Id, _) -> Id =/= 99 end),
        meck:expect(z_acl, rsc_visible, fun(Id, _) -> Id =/= 4 end),
        meck:expect(m_rsc, p, fun
            (1, <<"menu">>, _) -> [#rsc_tree{id = 2, tree = [#rsc_tree{id = 3}]},
                {4, [{5, []}]}, {4, [{10, []}]}, {6, []}, {2, [{9, []}]}, {99, []}];
            (_, <<"menu">>, _) -> undefined
        end),
        meck:expect(m_edge, objects, fun
            (1, haspart, _) -> [7];
            (7, haspart, _) -> [8, 1]; % Nested collection and a cycle.
            (_, haspart, _) -> []
        end),
        Order = [1, 2, 3, 6, 9, 7, 8],
        lists:foreach(fun({A, B}) ->
            ?assertEqual(B, filter_next_in:next_in(A, <<"root">>, Context)),
            ?assertEqual(A, filter_prev_in:prev_in(B, 1, Context))
        end, lists:zip(lists:droplast(Order), tl(Order))),
        ?assertEqual(undefined, filter_prev_in:prev_in(1, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(8, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(5, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(10, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(99, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(1, 4, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(undefined, 1, Context)),
        ?assertEqual(undefined, filter_next_in:next_in(1, undefined, Context))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
