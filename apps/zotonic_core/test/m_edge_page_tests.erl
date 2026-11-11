-module(m_edge_page_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

page_test_() ->
    %% Core mocks must not affect background testsandbox workers.
    z_test_support:isolated({foreach, fun setup/0, fun cleanup/1,
        [fun bounded_query/0, fun filtered_candidates/0, fun validation/0,
         fun private_root/0, fun model_path/0, fun unpaged_paths/0,
         fun partial_options/0, fun removed_paths/0]}).

setup() ->
    Modules = [m_rsc, m_predicate, z_acl, z_db, z_depcache],
    lists:foreach(fun(M) -> meck:new(M, [passthrough, no_link]) end, Modules),
    meck:expect(m_rsc, rid, fun(Id, _) -> Id end),
    meck:expect(m_predicate, name_to_id, fun(Pred, _) -> {ok, Pred} end),
    meck:expect(z_depcache, memo, fun(F, _, _, _, _) -> F() end),
    meck:expect(z_acl, rsc_visible, fun(Id, _) -> Id =/= 99 end),
    %% Tests must opt into SQL; rejected requests must never reach the database.
    meck:expect(z_db, qmap, fun(_, _, _, _) -> error(unexpected_query) end),
    meck:expect(z_db, q, fun(_, _, _) -> error(unexpected_query) end),
    meck:expect(z_db, assoc, fun(_, _, _) -> error(unexpected_query) end),
    Modules.

cleanup(Modules) -> lists:foreach(fun meck:unload/1, Modules).

bounded_query() ->
    lists:foreach(fun({Direction, Root, Endpoint}) ->
        meck:expect(z_db, qmap, fun(Sql, Params, Options, _) ->
            ?assertEqual([1, 4, 90, 7], Params),
            ?assertEqual(3000, proplists:get_value(timeout, Options)),
            ?assertEqual(iolist_to_binary(["select ", Endpoint,
                " as endpoint_id, predicate_id from edge where ", Root,
                " = $1 and predicate_id = $4 order by predicate_id, ", Endpoint,
                " limit $2 offset $3"]), iolist_to_binary(Sql)),
            {ok, [#{endpoint_id => N, predicate_id => 7} || N <- [10, 11, 12, 13]]}
        end),
        {ok, Page} = m_edge:get_page(Direction, 1, 7, 90, 3, #context{}),
        ?assertEqual([10, 11, 12], [maps:get(endpoint_id, X) || X <- maps:get(items, Page)]),
        ?assertEqual(true, maps:get(has_more, Page))
    end, [{in, "object_id", "subject_id"}, {out, "subject_id", "object_id"}]).

filtered_candidates() ->
    meck:expect(z_db, qmap, fun(_, [1, 3, 0], _, _) ->
        {ok, [#{endpoint_id => 99, predicate_id => 7},
              #{endpoint_id => 10, predicate_id => 99},
              #{endpoint_id => 11, predicate_id => 7}]}
    end),
    ?assertEqual({ok, #{items => [], has_more => true}},
        m_edge:get_page(in, 1, undefined, 0, 2, #context{})),
    meck:expect(z_db, qmap, fun(_, [1, 3, 2], _, _) ->
        {ok, [#{endpoint_id => 11, predicate_id => 7}]}
    end),
    ?assertMatch({ok, #{items := [#{endpoint_id := 11}], has_more := false}},
        m_edge:get_page(in, 1, undefined, 2, 2, #context{})).

validation() ->
    lists:foreach(fun({Offset, Limit}) ->
        ?assertEqual({error, badarg}, m_edge:get_page(in, 1, 7, Offset, Limit, #context{}))
    end, [{-1, 20}, {0, 0}, {0, 101}, {9999, 2}, {<<"0">>, 20}]),
    lists:foreach(fun(Args) ->
        ?assertEqual({error, badarg},
            m_edge:m_get([<<"s">>, 1, 7], #{payload => Args}, #context{}))
    end, [#{<<"limit">> => <<"bad">>}, #{<<"offset">> => <<"1000000">>},
          #{<<"offset">> => -1}, #{<<"limit">> => 0},
          #{<<"limit">> => 101}, #{<<"offset">> => 9999, <<"limit">> => 2}]).

private_root() ->
    ?assertEqual({error, eacces}, m_edge:get_page(in, 99, 7, 0, 20, #context{})),
    ?assertEqual({error, eacces}, m_edge:get_page(out, 1, 99, 0, 20, #context{})),
    ?assertEqual({error, eacces}, m_edge:get_page(in, undefined, 7, 0, 20, #context{})).

model_path() ->
    lists:foreach(fun({Method, Root, Endpoint, DbFun, Rows}) ->
        meck:expect(z_db, DbFun, fun(Sql, Params, _) ->
            ?assertEqual([1, 7, 3, 90], Params),
            Columns = case DbFun of q -> [Endpoint, ", id"]; assoc -> "*" end,
            ?assertEqual(iolist_to_binary(["select ", Columns, " from edge where ", Root,
                " = $1 and predicate_id = $2 order by seq, id limit $3 offset $4"]),
                iolist_to_binary(Sql)),
            Rows
        end),
        ?assertEqual({ok, {Rows, [<<"next_lookup">>]}},
            m_edge:m_get([Method, 1, 7, <<"next_lookup">>],
                #{payload => #{<<"offset">> => <<"90">>, <<"limit">> => <<"3">>}}, #context{})),
        ?assertEqual({error, eacces}, m_edge:m_get([Method, 99, 7],
            #{payload => #{<<"limit">> => 3}}, #context{}))
    end, methods()).

unpaged_paths() ->
    lists:foreach(fun({Method, _, _, DbFun, Rows}) ->
        meck:expect(z_db, DbFun, fun(Sql, Params, _) ->
            ?assertEqual([1, 7], Params),
            ?assertEqual(nomatch, binary:match(iolist_to_binary(Sql), <<"limit">>)),
            Rows
        end),
        lists:foreach(fun(Msg) ->
            ?assertEqual({ok, {Rows, []}}, m_edge:m_get([Method, 1, 7], Msg, #context{}))
        end, [undefined, #{payload => undefined}, #{payload => #{}},
              #{payload => #{<<"unrelated">> => true}}])
    end, methods()).

partial_options() ->
    lists:foreach(fun({Args, Expected}) ->
        meck:expect(z_db, q, fun(_, Params, _) ->
            ?assertEqual(Expected, Params), []
        end),
        ?assertEqual({ok, {[], []}}, m_edge:m_get([<<"o">>, 1, 7],
            #{payload => Args}, #context{}))
    end, [{#{<<"limit">> => 5}, [1, 7, 5, 0]},
          {#{<<"offset">> => 50}, [1, 7, 20, 50]}]).

removed_paths() ->
    lists:foreach(fun(Method) ->
        ?assertEqual({error, unknown_path},
            m_edge:m_get([Method, 1, 7], undefined, #context{}))
    end, [<<"o_page">>, <<"s_page">>]).

methods() ->
    Props = [[{id, 101}, {subject_id, 1}, {predicate_id, 7}, {object_id, 10},
        {seq, 2}, {creator_id, 1}, {created, {{2026, 11, 11}, {0, 0, 0}}}]],
    [{<<"o">>, "subject_id", "object_id", q, [{10, 101}]},
     {<<"s">>, "object_id", "subject_id", q, [{10, 101}]},
     {<<"o_props">>, "subject_id", "object_id", assoc, Props},
     {<<"s_props">>, "object_id", "subject_id", assoc, Props}].
