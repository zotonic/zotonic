-module(import_csv_tests).
-moduledoc("
EUnit tests for importing resource CSV/XLSX files and edge CSV files.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


import_csv_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun import_csv_files/1}.

import_xlsx_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun import_xlsx_file/1}.

setup() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:upgrade_await(Context),
    AdminContext = z_acl:logon(?ACL_ADMIN_USER_ID, Context),
    ok = mod_import_csv:manage_schema(install, AdminContext),
    cleanup(AdminContext),
    AdminContext.

cleanup(Context) ->
    delete_rsc(<<"import_csv_test_nr1">>, Context),
    delete_rsc(<<"import_csv_test_nr2">>, Context),
    delete_rsc(<<"import_xlsx_test_nr1">>, Context),
    delete_rsc(<<"import_xlsx_test_nr2">>, Context).

import_csv_files(Context) ->
    fun() ->
        {ok, DataDef} = mod_import_csv:can_handle("test-1.csv", test_file("test-1.csv"), Context),
        Result = import_data_csv:import(test_file("test-1.csv"), DataDef, false, Context),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}], lists:sort(proplists:get_value(seen, Result))),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}], lists:sort(proplists:get_value(new, Result))),
        ?assertEqual([], proplists:get_value(errors, Result)),

        Nr1 = m_rsc:rid(<<"import_csv_test_nr1">>, Context),
        Nr2 = m_rsc:rid(<<"import_csv_test_nr2">>, Context),
        ?assert(is_integer(Nr1)),
        ?assert(is_integer(Nr2)),
        ?assertEqual(#trans{ tr = [{en, <<"small test-2">>}, {nl, <<"testje">>}] },
            m_rsc:p(Nr1, <<"title">>, Context)),

        SameResult = import_data_csv:import(test_file("test-1.csv"), DataDef, false, Context),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}],
            lists:sort(proplists:get_value(ignored, SameResult))),
        ?assertEqual([], proplists:get_value(errors, SameResult)),

        {ok, EdgeDef} = mod_import_csv:can_handle("edges.csv", test_file("edges.csv"), Context),
        EdgeResult = import_data_csv:import(test_file("edges.csv"), EdgeDef, false, Context),
        ?assertEqual([{edge, 1}], proplists:get_value(seen, EdgeResult)),
        ?assertEqual([], proplists:get_value(errors, EdgeResult)),
        ?assertEqual([Nr2], m_edge:objects(Nr1, author, Context)),
        ?assertEqual(7, edge_seq(Nr1, author, Nr2, Context))
    end.

import_xlsx_file(Context) ->
    fun() ->
        {ok, DataDef} = mod_import_csv:can_handle("test-1.xlsx", test_file("test-1.xlsx"), Context),
        Result = import_data_xlsx:import(test_file("test-1.xlsx"), DataDef, false, Context),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}], lists:sort(proplists:get_value(seen, Result))),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}], lists:sort(proplists:get_value(new, Result))),
        ?assertEqual([], proplists:get_value(errors, Result)),

        Nr1 = m_rsc:rid(<<"import_xlsx_test_nr1">>, Context),
        Nr2 = m_rsc:rid(<<"import_xlsx_test_nr2">>, Context),
        ?assert(is_integer(Nr1)),
        ?assert(is_integer(Nr2)),
        ?assertEqual(#trans{ tr = [{en, <<"small xlsx test">>}, {nl, <<"xlsx testje">>}] },
            m_rsc:p(Nr1, <<"title">>, Context)),

        SameResult = import_data_xlsx:import(test_file("test-1.xlsx"), DataDef, false, Context),
        ?assertEqual([{<<"article">>, 1}, {<<"text">>, 1}],
            lists:sort(proplists:get_value(ignored, SameResult))),
        ?assertEqual([], proplists:get_value(errors, SameResult))
    end.

test_file(Name) ->
    filename:join([code:lib_dir(zotonic_mod_import_csv), "test", "data", Name]).

delete_rsc(Name, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            ok;
        Id ->
            {ok, Id} = m_rsc:update(Id, #{<<"is_protected">> => false}, Context),
            ok = m_rsc:delete(Id, Context)
    end.

edge_seq(SubjectId, Predicate, ObjectId, Context) ->
    {ok, PredicateId} = m_predicate:name_to_id(Predicate, Context),
    z_db:q1("
        select seq
        from edge
        where subject_id = $1
          and predicate_id = $2
          and object_id = $3",
        [SubjectId, PredicateId, ObjectId],
        Context).
