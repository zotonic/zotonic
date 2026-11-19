%% @hidden

%% Copyright 2009-2026 Marc Worrell, Arjan Scherpenisse
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

-module(m_rsc_update_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

update_reason_test() ->
    Before = #{<<"version">> => 4, <<"title">> => <<"Title">>, <<"category_id">> => 10},
    After = Before#{<<"version">> => 5, <<"modified">> => {{2026, 10, 12}, {12, 0, 0}},
        <<"modifier_id">> => 1},
    ?assertEqual(media_processing, m_rsc_update:update_reason(media_processing, Before, After)),
    ?assertEqual(update, m_rsc_update:update_reason(update, Before, After)),
    ?assertEqual(update, m_rsc_update:update_reason(undefined, Before, After)),
    ?assertEqual(update, m_rsc_update:update_reason(media_processing, Before, After#{<<"title">> => <<"Changed">>})),
    ?assertEqual(update, m_rsc_update:update_reason(media_processing, Before, After#{<<"category_id">> => 11})),
    ?assertEqual(update, m_rsc_update:update_reason(media_processing, Before, maps:remove(<<"title">>, After))).

menu_update_reason_test() ->
    Before = #{<<"version">> => 4, <<"title">> => <<"Unchanged">>, <<"menu">> => []},
    After = Before#{<<"version">> => 5, <<"menu">> => [#rsc_tree{id = 123}]},
    ?assertEqual(menu, m_rsc_update:update_reason(menu, Before, After)),
    ?assertEqual(menu, m_rsc_update:update_reason(menu, Before,
        After#{<<"date_start">> => undefined, <<"pivot_title">> => <<"unchanged">>})),
    ?assertEqual(update, m_rsc_update:update_reason(update, Before, After)),
    ?assertEqual(update, m_rsc_update:update_reason(menu, Before, After#{<<"title">> => <<"Changed">>})),
    ?assertEqual(update, m_rsc_update:update_reason(menu, Before, maps:remove(<<"title">>, After))).
