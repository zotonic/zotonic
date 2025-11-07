%% @copyright 2025 Marc Worrell
%% @doc Tests for combining nested SQL search terms.
%% @end

-module(z_search_terms_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


anyof_local_joins_use_subqueries_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                edge_term(<<"edge_a">>, <<"edge_a.predicate_id = 10">>),
                edge_term(<<"edge_b">>, <<"edge_b.predicate_id = 20">>)
            ]
        }
    ]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assertEqual(2, count(Query#search_sql.where, <<"EXISTS (">>)),
    ?assertNot(contains(Query#search_sql.from, <<"edge_a">>)),
    ?assertNot(contains(Query#search_sql.from, <<"edge_b">>)),
    ?assert(contains(Query#search_sql.where, <<"FROM edge edge_a">>)),
    ?assert(contains(Query#search_sql.where, <<"FROM edge edge_b">>)),
    ?assert(contains(Query#search_sql.where, <<"where (edge_a.subject_id = rsc.id) AND">>)),
    ?assertNot(contains(Query#search_sql.where, <<"z_search_subquery">>)),
    ?assert(contains(Query#search_sql.where, <<" OR ">>)).

noneof_local_joins_use_subqueries_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"noneof">>,
            terms = [
                edge_term(<<"edge_a">>, <<"edge_a.predicate_id = 10">>),
                edge_term(<<"edge_b">>, <<"edge_b.predicate_id = 20">>)
            ]
        }
    ]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assertEqual(2, count(Query#search_sql.where, <<"EXISTS (">>)),
    ?assert(contains(Query#search_sql.where, <<"NOT ">>)),
    ?assert(contains(Query#search_sql.where, <<" OR ">>)).

where_iolist_is_kept_as_one_condition_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                #search_sql_term{
                    select = [],
                    where = [<<"rsc.category_id = ANY(">>, '$1', <<"::int[])">>],
                    args = [[10, 20]]
                }
            ]
        }
    ]),
    ?assert(contains(Query#search_sql.where, <<"rsc.category_id = ANY($1::int[])">>)),
    ?assertNot(contains(Query#search_sql.where, <<"AND $1">>)),
    ?assertEqual([[10, 20]], Query#search_sql.args).

projection_keeps_join_in_outer_query_test() ->
    Alias = <<"edge_projected">>,
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                edge_term(Alias, <<"edge_projected.predicate_id = 10">>)
            ]
        },
        #search_sql_term{
            select = [<<"edge_projected.object_id">>],
            tables = #{ <<"rsc">> => <<"rsc">> }
        }
    ]),
    ?assert(contains(Query#search_sql.from, <<"join edge edge_projected">>)),
    ?assertNot(contains(Query#search_sql.where, <<"EXISTS (">>)).

alias_shared_by_alternatives_uses_one_subquery_test() ->
    Alias = <<"edge_shared">>,
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                edge_term(Alias, <<"edge_shared.predicate_id = 10">>),
                edge_term(Alias, <<"edge_shared.predicate_id = 20">>)
            ]
        }
    ]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assertEqual(1, count(Query#search_sql.where, <<"EXISTS (">>)),
    ?assertEqual(1, count(Query#search_sql.where, <<"FROM edge edge_shared">>)),
    ?assert(contains(Query#search_sql.where, <<" OR ">>)).

alias_shared_by_noneof_uses_not_exists_test() ->
    Alias = <<"edge_shared">>,
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"noneof">>,
            terms = [
                edge_term(Alias, <<"edge_shared.predicate_id = 10">>),
                edge_term(Alias, <<"edge_shared.predicate_id = 20">>)
            ]
        }
    ]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assertEqual(1, count(Query#search_sql.where, <<"NOT EXISTS (">>)),
    ?assertEqual(1, count(Query#search_sql.where, <<"FROM edge edge_shared">>)),
    ?assert(contains(Query#search_sql.where, <<" OR ">>)).

tables_and_join_types_are_kept_in_subquery_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                #search_sql_term{
                    select = [],
                    tables = #{
                        <<"rsc">> => <<"rsc">>,
                        <<"pivot_a">> => <<"pivot_a_table">>
                    },
                    join_inner = #{
                        <<"edge_a">> => {
                            <<"edge">>,
                            <<"edge_a.subject_id = rsc.id">>
                        }
                    },
                    join_left = #{
                        <<"medium_a">> => {
                            <<"medium">>,
                            <<"medium_a.id = rsc.id">>
                        }
                    },
                    where = [<<"edge_a.id IS NOT NULL">>]
                }
            ]
        }
    ]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assert(contains(Query#search_sql.where, <<"FROM pivot_a_table pivot_a">>)),
    ?assert(contains(Query#search_sql.where, <<"join edge edge_a">>)),
    ?assert(contains(Query#search_sql.where, <<"left join medium medium_a">>)).

additional_tables_are_cross_joined_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                #search_sql_term{
                    select = [],
                    tables = #{
                        <<"rsc">> => <<"rsc">>,
                        <<"pivot_a">> => <<"pivot_a_table">>,
                        <<"pivot_b">> => <<"pivot_b_table">>
                    },
                    where = [<<"pivot_a.id = pivot_b.id">>]
                }
            ]
        }
    ]),
    ?assertEqual(1, count(Query#search_sql.where, <<"cross join">>)),
    ?assert(contains(Query#search_sql.where, <<"pivot_a_table pivot_a">>)),
    ?assert(contains(Query#search_sql.where, <<"pivot_b_table pivot_b">>)).

left_join_only_subquery_keeps_base_row_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                #search_sql_term{
                    select = [],
                    tables = #{ <<"rsc">> => <<"rsc">> },
                    join_left = #{
                        <<"medium_a">> => {
                            <<"medium">>,
                            <<"medium_a.id = rsc.id">>
                        }
                    },
                    where = [<<"medium_a.id IS NULL">>]
                }
            ]
        }
    ]),
    ?assert(contains(Query#search_sql.where, <<"FROM (SELECT 1) AS z_search_subquery">>)),
    ?assert(contains(Query#search_sql.where, <<"left join medium medium_a">>)).

local_alias_category_restrictions_are_not_hoisted_test() ->
    lists:foreach(
        fun(Operator) ->
            Alias = <<"edge_local">>,
            Query = z_search_terms:combine([
                #search_sql_nested{
                    operator = Operator,
                    terms = [ category_edge_term(Alias) ]
                }
            ]),
            ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
            ?assert(contains(Query#search_sql.where, <<"FROM edge edge_local">>)),
            ?assertEqual([], Query#search_sql.cats),
            ?assertEqual([], Query#search_sql.cats_exact),
            ?assertEqual([], Query#search_sql.cats_exclude)
        end,
        [<<"anyof">>, <<"noneof">>]).

outer_alias_category_restrictions_are_kept_test() ->
    Alias = <<"edge_outer">>,
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [ category_edge_term(Alias) ]
        },
        #search_sql_term{
            select = [<<"edge_outer.object_id">>],
            tables = #{ <<"rsc">> => <<"rsc">> }
        }
    ]),
    ?assert(contains(Query#search_sql.from, <<"join edge edge_outer">>)),
    ?assertEqual([<<"article">>], proplists:get_value(Alias, Query#search_sql.cats)),
    ?assertEqual([<<"event">>], proplists:get_value(Alias, Query#search_sql.cats_exact)),
    ?assertEqual([<<"person">>], proplists:get_value(Alias, Query#search_sql.cats_exclude)).

only_local_alias_category_restrictions_are_removed_test() ->
    Alias = <<"edge_local">>,
    Term = category_edge_term(Alias),
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"anyof">>,
            terms = [
                Term#search_sql_term{
                    cats = [{<<"rsc">>, [<<"text">>]} | Term#search_sql_term.cats],
                    cats_exact = [{<<"rsc">>, [<<"article">>]} | Term#search_sql_term.cats_exact],
                    cats_exclude = [{<<"rsc">>, [<<"event">>]} | Term#search_sql_term.cats_exclude]
                }
            ]
        }
    ]),
    ?assertEqual([<<"text">>], proplists:get_value(<<"rsc">>, Query#search_sql.cats)),
    ?assertEqual([<<"article">>], proplists:get_value(<<"rsc">>, Query#search_sql.cats_exact)),
    ?assertEqual([<<"event">>], proplists:get_value(<<"rsc">>, Query#search_sql.cats_exclude)),
    ?assertEqual(undefined, proplists:get_value(Alias, Query#search_sql.cats)),
    ?assertEqual(undefined, proplists:get_value(Alias, Query#search_sql.cats_exact)),
    ?assertEqual(undefined, proplists:get_value(Alias, Query#search_sql.cats_exclude)).


edge_term(Alias, Where) ->
    #search_sql_term{
        select = [],
        tables = #{ <<"rsc">> => <<"rsc">> },
        join_inner = #{
            Alias => {
                <<"edge">>,
                [Alias, <<".subject_id = rsc.id">>]
            }
        },
        where = [Where]
    }.

category_edge_term(Alias) ->
    Alias1 = edge_term(Alias, [Alias, <<".predicate_id = 10">>]),
    Alias1#search_sql_term{
        cats = [{Alias, [<<"article">>]}],
        cats_exact = [{Alias, [<<"event">>]}],
        cats_exclude = [{Alias, [<<"person">>]}]
    }.

contains(Text, S) ->
    binary:match(Text, S) =/= nomatch.

count(Text, S) ->
    length(binary:matches(Text, S)).
