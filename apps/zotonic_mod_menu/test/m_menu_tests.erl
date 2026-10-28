%% @doc Menu removal warnings must include dependent descendants and backup availability.
-module(m_menu_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

removal_status_test_() ->
    z_test_support:isolated(fun removal_status/0).

removal_status() ->
    Modules = [m_rsc, z_module_manager],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(m_rsc, p_no_acl, fun
            (1, <<"is_dependent">>, _) -> false;
            (2, <<"is_dependent">>, _) -> true;
            (3, <<"is_dependent">>, _) -> undefined
        end),
        meck:expect(z_module_manager, active, fun(mod_backup, Backup) -> Backup end),
        % A non-dependent parent must not hide a dependent descendant.
        ?assertEqual(<<"delete_dependent">>, status([1, 2], false)),
        ?assertEqual(<<"recover_dependent">>, status([1, 2], true)),
        % An unset dependency flag does not trigger a dependent-page warning.
        ?assertEqual(<<"remove_only">>, status([1, 3], false)),
        ?assertEqual(<<"recover_menu">>, status([1, 3], true)),
        ?assertEqual(<<"remove_only">>, status([], false)),
        ?assertEqual(<<"recover_menu">>, status([1, 1], true)),
        lists:foreach(fun(Payload) ->
            ?assertEqual({error, badarg},
                m_menu:m_get([<<"removal_status">>], #{payload => Payload}, false))
        end, [1, <<"1">>, [0], [-1], [#{}], [<<"1">>]]),
        ?assertEqual({error, unknown_path}, m_menu:m_get([], #{}, false))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

status(Ids, Backup) ->
    {ok, {Status, [<<"rest">>]}} = m_menu:m_get(
        [<<"removal_status">>, <<"rest">>], #{payload => Ids}, Backup),
    Status.

%% Exercise the real filters, including their differing visibility semantics.
menu_queries_test_() ->
    z_test_support:isolated(fun menu_queries/0).

menu_queries() ->
    Modules = [m_rsc, m_edge, z_notifier],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    Menu = [#rsc_tree{id = 1, tree = [#rsc_tree{id = 2}, #rsc_tree{id = 3}]}],
    try
        meck:expect(m_rsc, rid, fun
            (main_menu, _) -> 10;
            (<<"main_menu">>, _) -> 10;
            (<<"child">>, _) -> 2;
            (Id, _) when is_integer(Id) -> Id;
            (_, _) -> undefined
        end),
        meck:expect(m_rsc, p, fun
            (Id, Prop, _) when
                (Id =:= 10 orelse Id =:= <<"main_menu">>),
                (Prop =:= menu orelse Prop =:= <<"menu">>) -> Menu;
            (_, _, _) -> undefined
        end),
        meck:expect(m_rsc, is_visible, fun(Id, _) -> Id =/= 3 end),
        meck:expect(m_rsc, exists, fun(_, _) -> true end),
        meck:expect(z_notifier, first, fun(_, _) -> undefined end),
        meck:expect(m_edge, objects, fun
            (2, hasmenu, _) -> [20];
            (_, hasmenu, _) -> []
        end),
        ?assertEqual(20, query([<<"rsc">>, <<"child">>])),
        ?assertEqual(10, query([<<"rsc">>, 1])),
        ?assertEqual([1, 2, 3], lists:sort(query([<<"ids">>, <<"main_menu">>]))),
        ?assertEqual([1, 2], query([<<"trail">>, <<"main_menu">>, <<"child">>])),
        ?assertEqual([], query([<<"trail">>, <<"main_menu">>, 3])),
        ?assertEqual([#rsc_tree{id = 2}], query([<<"subtree">>, <<"main_menu">>, 1])),
        ?assertEqual([], query([<<"ids">>, <<"missing">>])),
        ?assertEqual(undefined, query([<<"subtree">>, <<"main_menu">>, 99]))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

query(Path) ->
    {ok, {Result, [<<"rest">>]}} = m_menu:m_get(Path ++ [<<"rest">>], #{}, test),
    Result.
