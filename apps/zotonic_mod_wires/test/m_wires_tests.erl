-module(m_wires_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

widget_test_() ->
    {setup, fun() ->
        meck:new(m_config, [no_link]),
        meck:expect(m_config, get_boolean, fun(mod_wires, widget_autoload, true, C) ->
            z_context:get(enabled, C, true)
        end),
        meck:new(z_module_indexer, [no_link]),
        meck:expect(z_module_indexer, find, fun
            (lib, <<"js/modules/z.example.js">>, _) -> {ok, found};
            (lib, _, _) -> {error, enoent}
        end),
        meck:new(z_lib_include, [no_link]),
        meck:expect(z_lib_include, assets, fun([File], [], _) -> [#{file => File}] end)
    end, fun(_) -> meck:unload([m_config, z_module_indexer, z_lib_include]) end, [
        ?_assertEqual({ok, {[#{file => <<"js/modules/z.example.js">>}], []}},
            m_wires:m_get([<<"widget">>, <<"example">>], undefined, #context{})),
        ?_assertEqual({error, disabled}, m_wires:m_get([<<"widget">>, <<"example">>],
            undefined, z_context:set(enabled, false, #context{}))),
        ?_assertEqual({error, badarg}, m_wires:m_get([<<"widget">>, <<"../example">>], undefined, #context{})),
        ?_assertEqual({error, enoent}, m_wires:m_get([<<"widget">>, <<"missing">>], undefined, #context{}))
    ]}.
