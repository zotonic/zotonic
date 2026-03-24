%% @hidden

-module(m_config_tests).

-include_lib("eunit/include/eunit.hrl").

set_default_value_sets_missing_value_test() ->
    Context = test_context(),
    Module = codex_m_config_tests,
    Key = default_value_missing,
    ok = m_config:delete(Module, Key, Context),
    try
        ?assertEqual(undefined, m_config:get_value(Module, Key, Context)),
        ok = m_config:set_default_value(Module, Key, <<"first">>, Context),
        ?assertEqual(<<"first">>, m_config:get_value(Module, Key, Context))
    after
        ok = m_config:delete(Module, Key, Context)
    end.

set_default_value_keeps_existing_value_test() ->
    Context = test_context(),
    Module = codex_m_config_tests,
    Key = default_value_existing,
    ok = m_config:delete(Module, Key, Context),
    try
        ok = m_config:set_value(Module, Key, <<"existing">>, Context),
        ok = m_config:set_default_value(Module, Key, <<"replacement">>, Context),
        ?assertEqual(<<"existing">>, m_config:get_value(Module, Key, Context))
    after
        ok = m_config:delete(Module, Key, Context)
    end.

set_default_prop_sets_missing_prop_test() ->
    Context = test_context(),
    Module = codex_m_config_tests,
    Key = default_prop_missing,
    Prop = list,
    ok = m_config:delete(Module, Key, Context),
    try
        ?assertEqual(undefined, m_config:get_prop(Module, Key, Prop, Context)),
        ok = m_config:set_default_prop(Module, Key, Prop, [ en, nl ], Context),
        ?assertEqual([ en, nl ], m_config:get_prop(Module, Key, Prop, Context))
    after
        ok = m_config:delete(Module, Key, Context)
    end.

set_default_prop_keeps_existing_prop_test() ->
    Context = test_context(),
    Module = codex_m_config_tests,
    Key = default_prop_existing,
    Prop = list,
    ok = m_config:delete(Module, Key, Context),
    try
        ok = m_config:set_prop(Module, Key, Prop, [ en ], Context),
        ok = m_config:set_default_prop(Module, Key, Prop, [ en, nl ], Context),
        ?assertEqual([ en ], m_config:get_prop(Module, Key, Prop, Context))
    after
        ok = m_config:delete(Module, Key, Context)
    end.

test_context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_context:new(zotonic_site_testsandbox).
