-module(m_predicate_tests).
-include_lib("eunit/include/eunit.hrl").

predicate_test_() ->
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1, [
        fun minimal_maps/0, fun caller_visibility/0, fun guarded_routes/0,
        fun usage_requires_edit/0, fun hidden_categories/0, fun category_validation/0,
        fun internal_dependency_policy/0, fun unknown_name/0, fun property_visibility/0
    ]}).

setup() ->
    Mods = [m_rsc, m_category, z_acl, z_db, z_depcache],
    [meck:new(M, [passthrough, no_link]) || M <- Mods],
    meck:expect(m_category, name_to_id, fun(predicate, _) -> {ok, 99} end),
    meck:expect(m_category, get_range, fun(99, _) -> {1, 2} end),
    meck:expect(m_rsc, rid, fun
        (<<"public">>, _) -> 10;
        (<<"private">>, _) -> 20;
        (public, _) -> 10;
        (I, _) when is_integer(I) -> I;
        (_, _) -> undefined
    end),
    meck:expect(m_rsc, is_a, fun
        (I, predicate, _) -> lists:member(I, [10,20]);
        (I, category, _) -> lists:member(I, [30,40]);
        (_, _, _) -> false
    end),
    meck:expect(m_rsc, p, fun
        (_, <<"uri">>, _) -> <<"https://example.test/predicate">>;
        (_, <<"title">>, _) -> <<"Public title">>;
        (_, <<"summary">>, admin) -> <<"Restricted summary">>;
        (_, <<"summary">>, _) -> undefined;
        (_, <<"reversed">>, _) -> true
    end),
    meck:expect(m_rsc, name_to_id, fun(10, _) -> {ok, 10} end),
    meck:expect(m_rsc, p_no_acl, fun
        (20, <<"is_dependency_ignored">>, _) -> true;
        (_, <<"is_dependency_ignored">>, _) -> undefined
    end),
    meck:expect(z_acl, rsc_visible, fun
        (_, admin) -> true;
        (I, _) -> lists:member(I, [10,30])
    end),
    meck:expect(z_acl, rsc_editable, fun(_, C) -> C =:= admin end),
    %% Shared cache deliberately returns the same catalogue for every caller.
    meck:expect(z_depcache, memo, fun(F, _, _, _, _) -> F() end),
    meck:expect(z_db, q, fun(Sql, [1,2], _) ->
        ?assert(string:find(Sql, "select r.id, r.name") =/= nomatch),
        ?assertEqual(nomatch, string:find(Sql, "select *")),
        [{10, <<"public">>}, {20, <<"private">>}]
    end),
    Mods.

cleanup(Mods) -> [meck:unload(M) || M <- Mods].

minimal_maps() ->
    Expected = #{<<"id">> => 10, <<"name">> => <<"public">>,
        <<"uri">> => <<"https://example.test/predicate">>, <<"title">> => <<"Public title">>,
        <<"summary">> => undefined, <<"reversed">> => true},
    ?assertEqual(#{<<"public">> => Expected}, m_predicate:all(anonymous)),
    ?assertEqual(Expected, m_predicate:get(public, anonymous)),
    ?assertEqual(m_predicate:get(public, anonymous), m_predicate:get(10, anonymous)).

caller_visibility() ->
    ?assertEqual(2, map_size(m_predicate:all(admin))),
    ?assertEqual(1, map_size(m_predicate:all(anonymous))),
    ?assertEqual(undefined, m_predicate:get(20, anonymous)),
    ?assertMatch(#{<<"id">> := 20}, m_predicate:get(20, admin)).

guarded_routes() ->
    Routes = [<<"predicate">>, <<"is_used">>, <<"object_resources">>,
        <<"object_category">>, <<"subject_category">>, <<"is_valid_object_in_category">>,
        <<"is_valid_object_category">>, <<"is_valid_object_subcategory">>,
        <<"is_valid_subject_category">>, <<"is_valid_subject_subcategory">>],
    [?assertEqual({error,eacces},m_predicate:m_get([R,<<"private">>,30],undefined,anonymous)) || R <- Routes],
    ?assertEqual({error,eacces},m_predicate:m_get([<<"private">>],undefined,anonymous)),
    ?assertEqual(0,meck:num_calls(z_db,q,'_')).

usage_requires_edit() ->
    ?assertEqual({error,eacces},m_predicate:m_get([<<"is_used">>,<<"public">>],undefined,anonymous)),
    meck:expect(z_db,q1,fun(_, [10], admin) -> 123 end),
    ?assertEqual({ok,{true,[]}},m_predicate:m_get([<<"is_used">>,<<"public">>],undefined,admin)).

hidden_categories() ->
    meck:expect(z_db,q,fun(_, [10], _) -> [{30},{40}] end),
    ?assertEqual({error,eacces},m_predicate:m_get([<<"object_category">>,10],undefined,anonymous)),
    ?assertEqual({ok,{[30,40],[]}},m_predicate:m_get([<<"object_category">>,10],undefined,admin)).

category_validation() ->
    ?assertEqual({error,eacces},m_predicate:m_get([<<"is_valid_object_category">>,10,40],undefined,anonymous)),
    ?assertEqual({error,eacces},m_predicate:m_get([<<"is_valid_object_category">>,10,999],undefined,admin)).

internal_dependency_policy() ->
    ?assertEqual([20],m_predicate:dependency_ignored(anonymous)).

unknown_name() ->
    ?assertEqual(undefined,m_predicate:get(<<"nonexistent_predicate_without_an_atom">>,anonymous)).

property_visibility() ->
    Admin = maps:get(<<"public">>, m_predicate:all(admin)),
    ?assertEqual(<<"Restricted summary">>, maps:get(<<"summary">>, Admin)),
    Public = maps:get(<<"public">>, m_predicate:all(anonymous)),
    ?assertEqual(undefined, maps:get(<<"summary">>, Public)),
    ?assertEqual(Public, m_predicate:get(public, anonymous)).
