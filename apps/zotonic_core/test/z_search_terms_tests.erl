%% @copyright 2025 Marc Worrell
%% @doc Tests for combining nested SQL search terms.
%% @end

-module(z_search_terms_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-export([observe_acl_add_sql_check/2]).


scalar_exists_arguments_and_fields_test() ->
    Exists = {search_sql_exists, exists, [#search_sql_term{
        select = [],
        tables = #{<<"inner_rsc">> => <<"rsc">>},
        where = [[<<"inner_rsc.id = ">>, '$1']],
        args = [101]
    }]},
    Query = z_search_terms:combine([#search_sql_term{
        select = [Exists],
        where = [[Exists, <<" OR rsc.id = ">>, '$1']],
        group_by = [Exists],
        having = [Exists],
        sort = [[Exists, <<" ASC">>]],
        args = [202]
    }]),
    ?assertEqual(<<"rsc rsc">>, Query#search_sql.from),
    ?assertEqual([202, 101], Query#search_sql.args),
    lists:foreach(fun(Fragment) ->
        ?assert(contains(Fragment, <<"EXISTS (SELECT 1 FROM rsc inner_rsc">>)),
        ?assert(contains(Fragment, <<"inner_rsc.id = $2">>))
    end, [Query#search_sql.select, Query#search_sql.where,
        Query#search_sql.group_by, Query#search_sql.having, Query#search_sql.order]),
    ?assert(contains(Query#search_sql.where, <<"rsc.id = $1">>)).

scalar_exists_multiple_conditions_without_context_test() ->
    lists:foreach(
        fun({Operator, Prefix}) ->
            Terms = [#search_sql_term{select = [{search_sql_exists, Operator, [
                #search_sql_term{select = [], where = [<<"rsc.id > 0">>]},
                #search_sql_term{select = [], where = [<<"rsc.id < 10 OR rsc.id = 20">>]}
            ]}]}],
            Query = z_search_terms:combine(Terms),
            ?assertEqual(Query, z_search_terms:combine(Terms, undefined)),
            ?assertEqual(
                <<"rsc.id, ", Prefix/binary,
                  " (SELECT 1 where (rsc.id < 10 OR rsc.id = 20) AND (rsc.id > 0))">>,
                Query#search_sql.select)
        end,
        [{exists, <<"EXISTS">>}, {not_exists, <<"NOT EXISTS">>}]).

scalar_exists_inside_noneof_test() ->
    Query = z_search_terms:combine([#search_sql_nested{
        operator = <<"noneof">>,
        terms = [#search_sql_term{
            select = [],
            where = [{search_sql_exists, exists, []}]
        }]
    }]),
    ?assert(contains(Query#search_sql.where, <<"NOT (EXISTS (SELECT 1))">>)).

query_check_scope_test() ->
    ?assertNot(z_search:is_query_check()),
    ?assert(z_search:with_query_check(fun z_search:is_query_check/0)),
    ?assertNot(z_search:is_query_check()),
    ?assertError(
        query_check_error,
        z_search:with_query_check(fun() -> error(query_check_error) end)),
    ?assertNot(z_search:is_query_check()).

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

noneof_single_allof_uses_direct_not_exists_test() ->
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = <<"noneof">>,
            terms = [
                #search_sql_nested{
                    operator = <<"allof">>,
                    terms = [
                        edge_term(
                            <<"edge_a">>,
                            <<"edge_a.predicate_id = 10">>),
                        #search_sql_term{
                            select = [],
                            where = [<<"rsc.is_published">>]
                        }
                    ]
                }
            ]
        }
    ]),
    ?assertEqual(1, count(Query#search_sql.where, <<"NOT EXISTS (">>)),
    ?assert(contains(Query#search_sql.where, <<"edge_a.predicate_id = 10">>)),
    ?assert(contains(Query#search_sql.where, <<"rsc.is_published">>)).

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

fulltext_rank_after_created_is_removed_test() ->
    Query = z_search_terms:combine([
        #search_sql_term{
            sort = [
                <<"ts_rank_cd('{0.1, 0.2, 0.4, 1.0}', rsc.pivot_tsv, $1, 5) DESC">>
            ],
            args = [<<"query">>]
        },
        #search_sql_term{
            sort = [
                <<"rsc.created DESC">>
            ]
        }
    ]),
    ?assertEqual(<<"rsc.created DESC">>, Query#search_sql.order).

fulltext_rank_after_modified_is_removed_test() ->
    Query = z_search_terms:combine([
        #search_sql_term{
            sort = [
                {<<"rsc">>, $+, <<"modified">>},
                <<"public.word_similarity($1, rsc.pivot_title) DESC">>
            ],
            args = [<<"query">>]
        }
    ]),
    ?assertEqual(<<"rsc.modified ASC">>, Query#search_sql.order).

fulltext_rank_before_id_is_kept_test() ->
    Query = z_search_terms:combine([
        #search_sql_term{
            sort = [
                <<"ts_rank(rsc.pivot_tsv, query) DESC">>,
                {<<"rsc">>, $+, <<"id">>}
            ]
        }
    ]),
    ?assertEqual(
        <<"ts_rank(rsc.pivot_tsv, query) DESC, rsc.id ASC">>,
        Query#search_sql.order).

non_rank_sort_after_id_is_kept_test() ->
    Query = z_search_terms:combine([
        #search_sql_term{
            sort = [
                {<<"rsc">>, $+, <<"id">>},
                <<"ts_rank(rsc.pivot_tsv, query) DESC">>,
                {<<"edge">>, $+, <<"object_id">>}
            ]
        }
    ]),
    ?assertEqual(
        <<"rsc.id ASC, edge.object_id ASC">>,
        Query#search_sql.order).

local_resource_acl_is_added_inside_exists_test() ->
    with_acl_observer(
        fun(Context) ->
            Alias = <<"rsc_local">>,
            Query0 = z_search_terms:combine([
                #search_sql_nested{
                    operator = <<"anyof">>,
                    terms = [resource_term(Alias)]
                }
            ], Context),
            ?assert(contains(Query0#search_sql.where, <<"EXISTS (">>)),
            ?assert(contains(Query0#search_sql.where, <<"rsc_local.visible_for = $1">>)),
            ?assertEqual([{acl, Alias}], Query0#search_sql.args),
            Query1 = z_search_acl:reformat_sql_query(Query0, #{}, Context),
            ?assert(contains(Query1#search_sql.where, <<"rsc.visible_for = $2">>)),
            ?assertEqual([{acl, Alias}, {acl, <<"rsc">>}], Query1#search_sql.args)
        end).

local_resource_acl_is_added_inside_not_exists_test() ->
    with_acl_observer(
        fun(Context) ->
            Alias = <<"rsc_local">>,
            Query = z_search_terms:combine([
                #search_sql_nested{
                    operator = <<"noneof">>,
                    terms = [resource_term(Alias)]
                }
            ], Context),
            ?assert(contains(Query#search_sql.where, <<"NOT ">>)),
            ?assert(contains(Query#search_sql.where, <<"EXISTS (">>)),
            ?assert(contains(Query#search_sql.where, <<"rsc_local.visible_for = $1">>))
        end).

local_resource_acl_arguments_are_unique_test() ->
    with_acl_observer(
        fun(Context) ->
            Query = z_search_terms:combine([
                #search_sql_nested{
                    operator = <<"anyof">>,
                    terms = [
                        resource_term(<<"rsc_a">>),
                        resource_term(<<"rsc_b">>)
                    ]
                }
            ], Context),
            ?assert(contains(Query#search_sql.where, <<"rsc_a.visible_for = $1">>)),
            ?assert(contains(Query#search_sql.where, <<"rsc_b.visible_for = $2">>)),
            ?assertEqual(
                [{acl, <<"rsc_a">>}, {acl, <<"rsc_b">>}],
                Query#search_sql.args)
        end).

outer_resource_alias_is_declared_for_acl_test() ->
    Alias = <<"rsc_outer">>,
    Query = z_search_terms:combine([
        (resource_term(Alias))#search_sql_term{
            select = [[Alias, <<".id">>]]
        }
    ]),
    ?assertEqual(
        [{rsc, <<"rsc">>}, {rsc, Alias}],
        Query#search_sql.tables).

left_lateral_join_keeps_right_scope_isolated_test() ->
    OptionalAlias = <<"optional_a">>,
    EdgeAlias = <<"edge_optional">>,
    Query = z_search_terms:combine([
        #search_sql_nested{
            operator = {left_join, OptionalAlias},
            terms = [
                #search_sql_term{
                    select = [[EdgeAlias, <<".object_id AS value_1">>]],
                    tables = #{ <<"rsc">> => <<"rsc">> },
                    join_inner = #{
                        EdgeAlias => {
                            <<"edge">>,
                            [EdgeAlias, <<".subject_id = rsc.id">>]
                        }
                    },
                    where = [[EdgeAlias, <<".predicate_id = 10">>]]
                }
            ]
        },
        #search_sql_term{
            select = [[OptionalAlias, <<".value_1">>]]
        }
    ]),
    ?assert(contains(Query#search_sql.from, <<"left join LATERAL (SELECT">>)),
    ?assert(contains(Query#search_sql.from, <<"FROM edge edge_optional">>)),
    ?assert(contains(Query#search_sql.from, <<"edge_optional.predicate_id = 10">>)),
    ?assertNot(contains(Query#search_sql.where, <<"edge_optional">>)),
    ?assertNot(contains(Query#search_sql.from, <<"FROM rsc rsc">>)).

left_lateral_join_adds_local_acl_inside_subquery_test() ->
    with_acl_observer(
        fun(Context) ->
            OptionalAlias = <<"optional_acl">>,
            ResourceAlias = <<"rsc_optional">>,
            Query0 = z_search_terms:combine([
                #search_sql_nested{
                    operator = {left_join, OptionalAlias},
                    terms = [
                        (resource_term(ResourceAlias))#search_sql_term{
                            select = [[ResourceAlias, <<".id AS value_1">>]]
                        },
                        #search_sql_term{
                            select = [],
                            tables = #{},
                            where = [[ResourceAlias, <<".is_published">>]]
                        }
                    ]
                },
                #search_sql_term{
                    select = [[OptionalAlias, <<".value_1">>]]
                }
            ], Context),
            ?assert(contains(
                Query0#search_sql.from,
                <<"rsc_optional.visible_for = $1">>)),
            ?assert(contains(Query0#search_sql.from, <<"rsc_optional.is_published">>)),
            ?assert(count(Query0#search_sql.from, <<") AND (">>) >= 2),
            ?assertNot(contains(Query0#search_sql.where, <<"rsc_optional">>)),
            ?assertEqual([{acl, ResourceAlias}], Query0#search_sql.args),

            Query1 = z_search_acl:reformat_sql_query(Query0, #{}, Context),
            ?assert(contains(Query1#search_sql.where, <<"rsc.visible_for = $2">>)),
            ?assertEqual(
                [{acl, ResourceAlias}, {acl, <<"rsc">>}],
                Query1#search_sql.args)
        end).


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

resource_term(Alias) ->
    #search_sql_term{
        select = [],
        tables = #{
            <<"rsc">> => <<"rsc">>,
            Alias => <<"rsc">>
        },
        where = [[Alias, <<".id = rsc.id">>]]
    }.

with_acl_observer(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_notifier:observe(acl_add_sql_check, {?MODULE, observe_acl_add_sql_check}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(acl_add_sql_check, Context)
    end.

observe_acl_add_sql_check(#acl_add_sql_check{ alias = Alias, args = Args }, _Context) ->
    Nr = length(Args) + 1,
    {
        [Alias, <<".visible_for = $">>, integer_to_binary(Nr)],
        Args ++ [{acl, Alias}]
    }.

contains(Text, S) ->
    binary:match(Text, S) =/= nomatch.

count(Text, S) ->
    length(binary:matches(Text, S)).
