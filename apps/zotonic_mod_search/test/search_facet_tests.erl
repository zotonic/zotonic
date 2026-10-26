-module(search_facet_tests).
-moduledoc("Facet regression tests, discovered by bin/zotonic runtests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

deduplicate_facet_values_test_() ->
    {setup, fun setup_templates/0, fun(_) -> meck:unload(z_template) end,
        fun() ->
            lists:foreach(fun({Field, Input, Expected}) ->
                {ok, Term} = search_facet:qterm(Field, <<"@>">>, Input, #context{}),
                ?assertEqual([Expected], Term#search_sql_term.args)
            end, [
                {<<"tags">>, <<" red ||red||blue|| blue ||">>, [<<"red">>, <<"blue">>]},
                {<<"tags">>, [<<"blue">>, <<"red">>, <<" blue ">>, <<>>], [<<"blue">>, <<"red">>]},
                {<<"tags">>, <<" || || ">>, undefined},
                {<<"refs">>, [42, <<"42">>, 43, 42], [42, 43]}
            ])
        end}.

array_facet_operators_test_() ->
    {setup,
        fun() ->
            meck:new(z_template, [no_link]),
            meck:expect(z_template, blocks, fun(_, _, _) ->
                {ok, [tags_list, refs_ids]}
            end)
        end,
        fun(_) -> meck:unload(z_template) end,
        [
            {binary_to_list(Field), fun() ->
                Ref = <<"facet.f_", Field/binary>>,
                lists:foreach(fun(Op) ->
                    {ok, Term} = search_facet:qterm(Field, Op, [A, B], #context{}),
                    Query = z_search_terms:combine([Term]),
                    ?assertEqual(<<Ref/binary, " ", Op/binary, " $1">>, Query#search_sql.where),
                    ?assertEqual([[A, B]], Query#search_sql.args),
                    {ok, Empty} = search_facet:qterm(Field, Op, [], #context{}),
                    ?assertEqual([[]], Empty#search_sql_term.args)
                end, [<<"@>">>, <<"<@">>, <<"&&">>]),
                {ok, Include} = search_facet:qterm(Field, <<"=">>, [A, B], #context{}),
                IncludeQuery = z_search_terms:combine([Include]),
                ?assertEqual(<<"(", Ref/binary, " @> $1 OR ", Ref/binary, " @> $2)">>,
                    IncludeQuery#search_sql.where),
                ?assertEqual([[A], [B]], IncludeQuery#search_sql.args),
                lists:foreach(fun(Op) ->
                    {ok, Exclude} = search_facet:qterm(Field, Op, [A, B], #context{}),
                    ExcludeQuery = z_search_terms:combine([Exclude]),
                    ?assertEqual(<<"(NOT (", Ref/binary, " @> $1) AND NOT (", Ref/binary, " @> $2))">>,
                        ExcludeQuery#search_sql.where),
                    ?assertEqual([[A], [B]], ExcludeQuery#search_sql.args)
                end, [<<"<>">>, <<"!=">>]),
                {ok, Prefixed} = search_facet:qterm(Field, undefined, [
                    <<"<>", (z_convert:to_binary(A))/binary>>,
                    <<"!=", (z_convert:to_binary(B))/binary>>
                ], #context{}),
                PrefixedQuery = z_search_terms:combine([Prefixed]),
                ?assertEqual(<<"(NOT (", Ref/binary, " @> $1) AND NOT (", Ref/binary, " @> $2))">>,
                    PrefixedQuery#search_sql.where),
                ?assertEqual([[A], [B]], PrefixedQuery#search_sql.args),
                ?assertEqual({error, {unsupported_facet_operator, <<"~">>}},
                    search_facet:qterm(Field, <<"~">>, [A, B], #context{}))
            end}
            || {Field, A, B} <- [{<<"tags">>, <<"red">>, <<"blue">>}, {<<"refs">>, 42, 43}]
        ]}.

empty_fulltext_facet_test_() ->
    {setup,
        fun() ->
            meck:new(z_template, [no_link]),
            meck:new(z_module_manager, [no_link]),
            meck:expect(z_module_manager, active_not_running, fun(_) -> [] end),
            meck:new(z_db, [no_link]),
            meck:expect(z_db, columns, fun(search_facet, _) ->
                {ok, {Columns, _}} = search_facet:facet_table(#context{}),
                Columns
            end),
            meck:expect(z_db, q1, fun(_, [42], _) -> 42 end)
        end,
        fun(_) -> meck:unload([z_template, z_module_manager, z_db]) end,
        [
            {atom_to_list(Block) ++ " " ++ atom_to_list(Kind), fun() ->
                meck:reset(z_db),
                meck:expect(z_template, blocks, fun(_, _, _) -> {ok, [Block]} end),
                meck:expect(z_template, render_block_to_iolist,
                    fun(_, _, _, Context) -> {Output, Context} end),
                meck:expect(z_db, update, fun(search_facet, 42, Update, _) ->
                    % Both columns must be explicitly cleared in the DB update.
                    ?assertEqual(#{<<"f_body">> => undefined, SearchColumn => undefined}, Update),
                    {ok, 1}
                end),
                ?assertEqual(ok, search_facet:pivot_rsc(42, #context{})),
                ?assert(meck:called(z_db, update, [search_facet, 42, '_', #context{}]))
            end}
            || {Block, SearchColumn} <- [{body_ft, <<"ft_body">>}, {body_fts, <<"fts_body">>}],
               {Kind, Output} <- [{empty, <<>>}, {whitespace, <<" \n\t ">>}]
        ]}.

nested_facet_query_test_() ->
    {setup, fun setup_search/0, fun cleanup_search/1, [
        {binary_to_list(Op), fun() ->
            Facet = #search_sql_term{
                label = {facet, <<"status">>},
                where = [<<"facet.f_status = ">>, '$1'],
                args = [<<"active">>],
                join_inner = #{
                    <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
                }
            },
            Nested = #search_sql_nested{
                operator = Op,
                terms = [
                    Facet,
                    #search_sql_nested{
                        operator = <<"allof">>,
                        terms = [#search_sql_term{where = <<"rsc.is_featured = true">>}]
                    }
                ]
            },
            % Capture the base query after public facet processing partitions terms.
            meck:expect(z_search, reformat_sql_query, fun(Query, _, _) ->
                ?assertEqual(ExpectedWhere, Query#search_sql.where),
                ?assertEqual([<<"active">>], Query#search_sql.args),
                ?assertEqual(
                    <<"rsc rsc join search_facet facet on facet.id = rsc.id">>,
                    Query#search_sql.from),
                Query
            end),
            Result = search_facet:search_query_facets(#search_result{},
                #search_sql{select = <<"rsc.id">>, from = <<"rsc rsc">>,
                    search_sql_terms = [Facet, Nested]}, #context{db = {test, z_db_pgsql}}),
            ?assert(is_map(Result#search_result.facets))
        end}
        || {Op, ExpectedWhere} <- [
            {<<"allof">>, <<"((rsc.is_featured = true) AND facet.f_status = $1)">>},
            {<<"anyof">>, <<"((rsc.is_featured = true) OR facet.f_status = $1)">>},
            {<<"noneof">>, <<" NOT((rsc.is_featured = true) OR facet.f_status = $1)">>}
        ]
    ]}.

selected_facet_values_test_() ->
    {setup, fun setup_search/0, fun cleanup_search/1,
        [
            {"existing scalar and list selections keep the original counts", fun() ->
                Fs = selected_facet_fixture(<<"status">>, <<"text">>, <<"AKTIV">>),
                lists:foreach(
                    fun(Value) ->
                        ?assertEqual(Fs, selected_facet_result(<<"status">>, Value, Fs))
                    end,
                    [<<"AKTIV">>, [<<"AKTIV">>], [<<"AKTIV">>, <<"AKTIV">>], []]),
                Fs2 = selected_facet_result(<<"status">>, <<"PLANERAD">>, Fs),
                ?assertEqual(Fs2, selected_facet_result(
                    <<"status">>, [<<"AKTIV">>, <<"PLANERAD">>], Fs2))
            end},
            {"missing selections get separate zero-count entries", fun() ->
                lists:foreach(
                    fun({Name, Type, Existing, Missing, Label}) ->
                        Fs = selected_facet_fixture(Name, Type, Existing),
                        Result = selected_facet_result(Name, [Existing, Missing, Missing], Fs),
                        #{Name := #{<<"counts">> := [Original]}} = Fs,
                        ?assertEqual(Fs#{Name => (maps:get(Name, Fs))#{
                            <<"counts">> => [Original, #{
                                <<"count">> => 0,
                                <<"facet">> => Name,
                                <<"value">> => Missing,
                                <<"label">> => Label
                            }]
                        }}, Result)
                    end,
                    [
                        {<<"status">>, <<"text">>, <<"AKTIV">>, <<"PLANERAD">>, <<"PLANERAD">>},
                        {<<"tags">>, <<"list">>, <<"red">>, <<"blue">>, <<"blue">>},
                        {<<"refs">>, <<"ids">>, 42, 43, <<"43">>}
                    ])
            end}
        ]}.

selected_facet_fixture(Name, Type, Value) ->
    #{Name => #{
        <<"type">> => <<"count">>,
        <<"facet_type">> => Type,
        <<"counts">> => [#{<<"facet">> => Name, <<"label">> => z_convert:to_binary(Value),
            <<"value">> => Value, <<"count">> => 7}]
    }}.

selected_facet_result(Name, Value, Fs) ->
    #{Name := #{<<"counts">> := Counts}} = Fs,
    Rows = [#{
        <<"facet">> => Name,
        <<"facet_id">> => 42,
        <<"value">> => z_convert:to_binary(maps:get(<<"value">>, F)),
        <<"count">> => z_convert:to_binary(maps:get(<<"count">>, F))
    } || F <- Counts],
    Block = case Name of
        <<"status">> -> status;
        <<"tags">> -> tags_list;
        <<"refs">> -> refs_ids
    end,
    meck:expect(z_template, blocks, fun(_, _, _) -> {ok, [Block]} end),
    meck:expect(z_db, qmap, fun(_, _, _) -> {ok, Rows} end),
    Result = search_facet:search_query_facets(#search_result{
        search_args = #{<<"q">> => [#{
            <<"term">> => <<"facet:", Name/binary>>, <<"value">> => Value
        }]}
    }, #search_sql{select = <<"rsc.id">>, from = <<"rsc rsc">>, search_sql_terms = []},
        #context{db = {test, z_db_pgsql}}),
    Result#search_result.facets.

setup_templates() ->
    meck:new(z_template, [no_link]),
    meck:expect(z_template, blocks, fun(_, _, _) ->
        {ok, [status, tags_list, refs_ids]}
    end).

setup_search() ->
    setup_templates(),
    meck:new(z_db, [no_link]),
    meck:expect(z_db, qmap, fun(_, _, _) -> {ok, []} end),
    meck:new(z_acl, [no_link]),
    meck:expect(z_acl, sudo, fun(Context) -> Context end),
    meck:new(z_search, [passthrough, no_link]),
    meck:expect(z_search, reformat_sql_query, fun(Query, _, _) -> Query end),
    meck:new(m_rsc, [no_link]),
    meck:expect(m_rsc, rid, fun(_, _) -> undefined end).

cleanup_search(_) ->
    meck:unload([z_template, z_db, z_acl, z_search, m_rsc]).
