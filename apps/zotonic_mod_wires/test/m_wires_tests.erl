%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for widget library resolution and automatic-loading configuration.
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

%% @hidden
-module(m_wires_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

widget_test_() ->
    {setup, fun() ->
        meck:new(m_config, [passthrough, no_link]),
        meck:expect(m_config, get_boolean, fun
            (mod_wires, widget_autoload, true, #context{site=wires_widget_test} = C) ->
                z_context:get(enabled, C, true);
            (Module, Key, Default, C) -> meck:passthrough([Module, Key, Default, C])
        end),
        meck:new(z_module_indexer, [passthrough, no_link]),
        meck:expect(z_module_indexer, find, fun
            (lib, <<"js/modules/z.example.js">>, #context{site=wires_widget_test}) -> {ok, found};
            (lib, _, #context{site=wires_widget_test}) -> {error, enoent};
            (Type, Name, C) -> meck:passthrough([Type, Name, C])
        end),
        meck:new(z_lib_include, [passthrough, no_link]),
        meck:expect(z_lib_include, assets, fun
            ([File], [], #context{site=wires_widget_test}) -> [#{file => File}];
            (Files, Options, C) -> meck:passthrough([Files, Options, C])
        end)
    end, fun(_) -> meck:unload([m_config, z_module_indexer, z_lib_include]) end, [
        ?_assert(erlang:function_exported(z_module_indexer, find_all, 3)),
        ?_assertEqual({ok, {[#{file => <<"js/modules/z.example.js">>}], []}},
            m_wires:m_get([<<"widget">>, <<"example">>], undefined, #context{site=wires_widget_test})),
        ?_assertEqual({ok, {[#{file => <<"js/modules/z.example.js">>}], [<<"url">>]}},
            m_wires:m_get([<<"widget">>, <<"example">>, <<"url">>], undefined, #context{site=wires_widget_test})),
        ?_assertEqual({error, disabled}, m_wires:m_get([<<"widget">>, <<"example">>],
            undefined, z_context:set(enabled, false, #context{site=wires_widget_test}))),
        ?_assertEqual({error, badarg}, m_wires:m_get([<<"widget">>, <<"../example">>], undefined, #context{site=wires_widget_test})),
        ?_assertEqual({error, enoent}, m_wires:m_get([<<"widget">>, <<"missing">>], undefined, #context{site=wires_widget_test}))
    ]}.
