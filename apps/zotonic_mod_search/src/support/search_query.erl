%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2022 Arjan Scherpenisse
%% @doc Handler for m.search[{query, Args..}]

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

-module(search_query).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

%% interface functions
-export([
         search/2,
         parse_request_args/1,
         parse_query_text/1,
         build_query/2
        ]).

%% For testing
-export([
    qterm/2,
    expand_object_predicates/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SQL_SAFE_REGEXP, "^[0-9a-zA-Z_\.]+$").


%% @doc Build a SQL search query from the filter arguments.
-spec search( map() | proplists:proplist(), z:context() ) -> #search_sql_terms{} | #search_result{}.
search(Query, Context) ->
    Query1 = filter_empty(Query),
    Query2 = lists:filtermap(
        fun
            ({K, V}) when is_binary(K) ->
                case request_arg(K) of
                    undefined -> false;
                    A -> {true, {A, V}}
                end;
            ({K, _} = KV) when is_atom(K) ->
                {true, KV}
        end,
        Query1),
    Query3 = lists:flatten(
        lists:map(
            fun
                ({K, #{ <<"all">> := All, <<"any">> := Any }}) ->
                    All1 = filter_empty( lists:map(fun(V) -> {K, V} end, All) ),
                    case lists:filter(fun z_utils:is_empty/1, Any) of
                        [] -> All1;
                        Any1 -> [ {K, Any1} | All1 ]
                    end;
                ({K, #{ <<"all">> := All }}) ->
                    filter_empty( lists:map(fun(V) -> {K, V} end, All) );
                ({K, #{ <<"any">> := Any }}) ->
                    case lists:filter(fun z_utils:is_empty/1, Any) of
                        [] -> [];
                        Any1 -> {K, Any1}
                    end;
                (KV) ->
                    KV
            end,
            Query2)),
    Query4 = case lists:flatten( proplists:get_all_values(cat, Query3) ) of
        [] -> Query3;
        Cats -> [{cat, Cats} | proplists:delete(cat, Query3)]
    end,
    Query5 = case lists:flatten( proplists:get_all_values(cat_exclude, Query4) ) of
        [] -> Query4;
        CatsX -> [{cat_exclude, CatsX} | proplists:delete(cat_exclude, Query4)]
    end,
    build_query(lists:sort(Query5), Context).


-spec build_query(list(), z:context()) -> #search_sql_terms{} | #search_result{}.
build_query(Terms, Context) ->
    Ts = lists:flatten(lists:map(fun(T) -> qterm(T, Context) end, Terms)),
    case lists:member(none, Ts) of
        true ->
            #search_result{};
        false ->
            #search_sql_terms{ terms = Ts }
    end.

%% @doc Fetch all arguments from the query string in the HTTP request.
-spec qargs( z:context() ) -> list( {binary(), term()} ).
qargs(Context) ->
    Args = z_context:get_q_all_noz(Context),
    lists:filtermap(
                fun
                    ({<<"qargs">>, _}) -> false;
                    ({<<"qs">>, V}) -> {true, {<<"text">>, V}};
                    ({<<"q", Term/binary>>, V}) -> {true, {Term, V}};
                    (_) -> false
                end,
                Args).

-spec parse_request_args( list( {binary(), term()} ) ) -> list( {atom(), term()} ).
parse_request_args(Args) ->
    parse_request_args(Args, []).

parse_request_args([], Acc) ->
    Acc;
parse_request_args([{K,V}|Rest], Acc) when is_binary(K) ->
    case z_context:is_zotonic_arg(K) of
        true ->
            parse_request_args(Rest, Acc);
        false ->
            case request_arg(K) of
                undefined -> parse_request_args(Rest, Acc);
                Arg -> parse_request_args(Rest, [{Arg, V}|Acc])
            end
    end;
parse_request_args([{K,V}|Rest], Acc) ->
    parse_request_args([{z_convert:to_binary(K), V}|Rest], Acc).


%% @doc Parses a query text. Every line is an argument; of which the first
%% '=' separates argument key from argument value.
-spec parse_query_text( binary() | string() | undefined ) -> list( {atom(), term()} ).
parse_query_text(undefined) ->
    [];
parse_query_text(Text) when is_list(Text) ->
    parse_query_text(list_to_binary(Text));
parse_query_text(Text) when is_binary(Text) ->
    Lines = binary:split(Text, <<"\n">>, [global]),
    KVs = [ split_arg(z_string:trim(Line)) || Line <- Lines],
    Args = [ {request_arg(K), V} || {K,V} <- KVs, K =/= <<>> ],
    [ {K,V} || {K,V} <- Args, K =/= undefined ].

split_arg(<<>>) ->
    {<<>>, <<>>};
split_arg(B) ->
    case binary:split(B, <<"=">>) of
        [K,V] -> {z_string:trim(K), z_string:trim(V)};
        [K] -> {z_string:trim(K), <<"true">>}
    end.


% Convert known request arguments to atoms.
request_arg(<<"content_group">>)       -> content_group;
request_arg(<<"cat">>)                 -> cat;
request_arg(<<"cat_exact">>)           -> cat_exact;
request_arg(<<"cat_exclude">>)         -> cat_exclude;
request_arg(<<"creator_id">>)          -> creator_id;
request_arg(<<"modifier_id">>)         -> modifier_id;
request_arg(<<"facet.", F/binary>>)    -> {facet, F};
request_arg(<<"filter">>)              -> filter;
request_arg(<<"filter.facet.", F/binary>>)-> {facet, F};
request_arg(<<"filter.", F/binary>>)   -> {filter, F};
request_arg(<<"pivot.", _/binary>> = F)-> {filter, F};
request_arg(<<"pivot_", F/binary>>)    -> {filter, <<"pivot.", F/binary>>};
request_arg(<<"id">>)                  -> id;
request_arg(<<"id_exclude">>)          -> id_exclude;
request_arg(<<"hasobject">>)           -> hasobject;
request_arg(<<"hasobjectpredicate">>)  -> hasobjectpredicate;
request_arg(<<"hassubject">>)          -> hassubject;
request_arg(<<"hassubjectpredicate">>) -> hassubjectpredicate;
request_arg(<<"hasanyobject">>)        -> hasanyobject;
request_arg(<<"hasmedium">>)           -> hasmedium;
request_arg(<<"is_authoritative">>)    -> is_authoritative;
request_arg(<<"is_featured">>)         -> is_featured;
request_arg(<<"is_published">>)        -> is_published;
request_arg(<<"is_public">>)           -> is_public;
request_arg(<<"date_start_after">>)    -> date_start_after;
request_arg(<<"date_start_before">>)   -> date_start_before;
request_arg(<<"date_start_year">>)     -> date_start_year;
request_arg(<<"date_end_after">>)      -> date_end_after;
request_arg(<<"date_end_before">>)     -> date_end_before;
request_arg(<<"date_end_year">>)       -> date_end_year;
request_arg(<<"publication_month">>)   -> publication_month;
request_arg(<<"publication_year">>)    -> publication_year;
request_arg(<<"publication_after">>)   -> publication_after;
request_arg(<<"publication_before">>)  -> publication_before;
request_arg(<<"qargs">>)               -> qargs;
request_arg(<<"query_id">>)            -> query_id;
request_arg(<<"rsc_id">>)              -> rsc_id;
request_arg(<<"name">>)                -> name;
request_arg(<<"language">>)            -> language;
request_arg(<<"sort">>)                -> sort;
request_arg(<<"asort">>)               -> asort;
request_arg(<<"zsort">>)               -> zsort;
request_arg(<<"text">>)                -> text;
request_arg(<<"match_objects">>)       -> match_objects;
request_arg(<<"match_object_ids">>)    -> match_object_ids;
request_arg(<<"upcoming">>)            -> upcoming;
request_arg(<<"ongoing">>)             -> ongoing;
request_arg(<<"finished">>)            -> finished;
request_arg(<<"unfinished">>)          -> unfinished;
request_arg(<<"unfinished_or_nodate">>)-> unfinished_or_nodate;
% Skip these
request_arg(<<"page">>)                -> undefined;
request_arg(<<"pagelen">>)             -> undefined;
request_arg(<<"options">>)             -> undefined;
% Complain about all else
request_arg(<<"custompivot">>)         ->
    ?LOG_ERROR("The query term 'custompivot' has been removed. Use filters with 'pivot.pivotname.field' instead."),
    throw({error, {unknown_query_term, custompivot}});
request_arg(Term) ->
    ?LOG_ERROR(#{
        text => <<"Skipping unknown query term">>,
        term => Term
    }),
    undefined.


%% Private methods start here

%% @doc Drop all empty query arguments. Search forms have empty values
%% for unused filters.
filter_empty(Q) when is_map(Q) ->
    filter_empty(maps:to_list(Q));
filter_empty(Q) when is_list(Q) ->
    lists:filter(fun({_, X}) -> not(empty_term(X)) end, Q).

empty_term([]) -> true;
empty_term(<<>>) -> true;
empty_term(undefined) -> true;
empty_term(null) -> true;
empty_term([X, _]) -> empty_term(X);
empty_term(_) -> false.

qterm(undefined, _Context) ->
    [];
qterm([], _Context) ->
    [];
qterm(Ts, Context) when is_list(Ts) ->
    lists:map(fun(T) -> qterm(T, Context) end, Ts);
qterm({cat, Cats}, Context) ->
    %% cat=categoryname
    %% Filter results on a certain category.
    Cats1 = assure_categories(Cats, Context),
    % Cats2 = add_or_append(<<"rsc">>, Cats1, []),
    #search_sql_term{ cats = [ {<<"rsc">>, Cats1}] };
    % parse_query(Rest, Context, Result#search_sql{cats=Cats2});
qterm({cat_exclude, Cats}, Context) ->
    %% cat_exclude=categoryname
    %% Filter results outside a certain category.
    Cats1 = assure_categories(Cats, Context),
    #search_sql_term{ cats_exclude = [ {<<"rsc">>, Cats1} ] };
qterm({cat_exact, Cats}, Context) ->
    %% cat_exact=categoryname
    %% Filter results excactly of a category (excluding subcategories)
    Cats1 = assure_categories(Cats, Context),
    #search_sql_term{ cats_exact = [ {<<"rsc">>, Cats1} ] };
qterm({content_group, ContentGroup}, Context) ->
    %% content_group=id
    %% Include only resources which are member of the given content group (or one of its children)
    Q = #search_sql_term{
        extra = [ no_content_group_check ]
    },
    case rid(ContentGroup, Context) of
        any ->
            Q;
        undefined ->
            % Force an empty result
            none;
        CGId ->
            case m_rsc:is_a(CGId, content_group, Context) of
                true ->
                    List = m_hierarchy:contains(<<"content_group">>, ContentGroup, Context),
                    case m_rsc:p_no_acl(CGId, name, Context) of
                        <<"default_content_group">> ->
                            Q#search_sql_term{
                                where = [
                                    <<"(rsc.content_group_id IN (SELECT(unnest(">>, '$1',
                                    <<"::int[]))) or rsc.content_group_id is null)">>
                                ],
                                args = [
                                    List
                                ]
                            };
                        _ ->
                            Q#search_sql_term{
                                where = [
                                    <<"rsc.content_group_id IN (SELECT(unnest(">>, '$1',
                                    <<"::int[])))">>
                                ],
                                args = [
                                    List
                                ]
                            }
                    end;
                false ->
                    Q#search_sql_term{
                        where = [
                            <<"rsc.content_group_id = ">>, '$1'
                        ],
                        args = [
                            CGId
                        ]
                    }
            end
    end;
qterm({id_exclude, Ids}, Context) when is_list(Ids) ->
    %% id_exclude=resource-id
    %% Exclude an id or multiple ids from the result
    RscIds = lists:filtermap(
        fun(Id) ->
            case m_rsc:rid(Id, Context) of
                undefined -> false;
                RscId -> {true, RscId}
            end
        end,
        Ids),
    #search_sql_term{
        where = [
            <<"rsc.id NOT IN (SELECT(unnest(">>, '$1', <<"::int[])))">>
        ],
        args = [ RscIds ]
    };
qterm({id_exclude, Id}, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        RscId ->
            #search_sql_term{
                where = [ <<"rsc.id <> ">>, '$1'],
                args = [ RscId ]
            }
    end;
qterm({id, Ids}, Context) when is_list(Ids) ->
    %% id=resource-id
    %% Limit to an id or multiple ids
    RscIds = lists:filtermap(
        fun(Id) ->
            case m_rsc:rid(Id, Context) of
                undefined -> false;
                RscId -> {true, RscId}
            end
        end,
        Ids),
    #search_sql_term{
        where = [
            <<"rsc.id IN (SELECT(unnest(">>, '$1', <<"::int[])))">>
        ],
        args = [ RscIds ]
    };
qterm({id, Id}, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        RscId ->
            #search_sql_term{
                where = [ <<"rsc.id = ">>, '$1' ],
                args = [ RscId ]
            }
    end;
qterm({hasmedium, HasMedium}, _Context) ->
    %% hasmedium=true|false
    %% Give all things which have a medium record attached (or not)
    case z_convert:to_bool(HasMedium) of
        true ->
            #search_sql_term{
                join_inner = #{
                    <<"medium">> => {<<"medium">>, <<"medium.id = rsc.id">>}
                }
            };
        false ->
            #search_sql_term{
                join_left = #{
                    <<"medium">> => {<<"medium">>, <<"medium.id = rsc.id">>}
                },
                where = [
                    <<"medium.id is null ">>
                ]
            }
    end;
qterm({hassubject, Id}, Context) ->
    parse_edges(hassubject, maybe_split_list(Id), Context);
qterm({hasobject, Id}, Context) ->
    parse_edges(hasobject, maybe_split_list(Id), Context);
qterm({hasanyobject, ObjPreds}, Context) ->
    %% hasanyobject=[[id,predicate]|id, ...]
    %% Give all things which have an outgoing edge to Id with any of the given object/predicate combinations
    OPs = expand_object_predicates(ObjPreds, Context),
    % rsc.id in (select subject_id from edge where (object_id = ... and predicate_id = ... ) or (...) or ...)
    Alias = edge_alias(),
    OPClauses = [ object_predicate_clause(Alias, Obj, Pred) || {Obj, Pred} <- OPs ],
    #search_sql_term{
        where = [
            "rsc.id in (select ", Alias ,".subject_id from edge ",Alias," where (",
                lists:join(") or (", OPClauses),
            "))"
        ]
    };
qterm({hasobjectpredicate, Predicate}, Context) ->
    %% hasobjectpredicate=predicate
    %% Give all things which have any outgoing edge with given predicate
    Alias = edge_alias(),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".subject_id = rsc.id ">>,
            <<" and ">>, Alias, <<".predicate_id = ">>, '$1'
        ],
        args = [
            predicate_to_id(Predicate, Context)
        ]
    };
qterm({hassubjectpredicate, Predicate}, Context) ->
    %% hassubjectpredicate=predicate
    %% Give all things which have any incoming edge with given predicate
    Alias = edge_alias(),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".object_id = rsc.id ">>,
            <<" and ">>, Alias, <<".predicate_id = ">>, '$1'
        ],
        args = [
            predicate_to_id(Predicate, Context)
        ]
    };
qterm({is_featured, Boolean}, _Context) ->
    %% is_featured or is_featured={false,true}
    %% Filter on whether an item is featured or not.
    #search_sql_term{
        where = [
            <<"rsc.is_featured = ">>, '$1'
        ],
        args = [
            z_convert:to_bool(Boolean)
        ]
    };
qterm({is_published, Boolean}, _Context) ->
    %% is_published or is_published={false,true,all}
    %% Filter on whether an item is published or not.
    case z_convert:to_binary(Boolean) of
        <<"all">> ->
            #search_sql_term{
                extra = [ no_publish_check ]
            };
        _ ->
            case z_convert:to_bool(Boolean) of
                true ->
                    #search_sql_term{
                        extra = [ no_publish_check ],
                        where = [
                            <<"rsc.is_published = true and "
                              "rsc.publication_start <= now() and "
                              "rsc.publication_end >= now()">>
                        ]
                    };
              false ->
                    #search_sql_term{
                        extra = [ no_publish_check ],
                        where = [
                            <<"(rsc.is_published = false or "
                            "rsc.publication_start > now() or "
                            "rsc.publication_end < now())">>
                        ]
                    }
            end
    end;
qterm({is_public, Boolean}, _Context) ->
    %% is_public or is_public={false,true,all}
    %% Filter on whether an item is publicly visible or not.
    %% TODO: Adapt this for the different ACL modules
    case z_convert:to_binary(Boolean) of
        <<"all">> ->
            [];
        _ ->
            case z_convert:to_bool(Boolean) of
                true ->
                    #search_sql_term{
                        where = [
                            <<"rsc.visible_for = 0">>
                        ]
                    };
              false ->
                    #search_sql_term{
                        where = [
                            <<"rsc.visible_for > 0">>
                        ]
                    }
          end
    end;
qterm({upcoming, Boolean}, _Context) ->
    %% upcoming
    %% Filter on items whose start date lies in the future
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start >= current_timestamp">>
                ]
            };
        false ->
            []
    end;
qterm({ongoing, Boolean}, _Context) ->
    %% ongoing
    %% Filter on items whose date range is around the current date
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start <= current_timestamp ",
                      "and rsc.pivot_date_end >= current_timestamp">>
                ]
            };
        false ->
            []
    end;
qterm({finished, Boolean}, _Context) ->
    %% finished
    %% Filter on items whose end date lies in the past
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end < current_timestamp">>
                ]
            };
        false ->
            []
    end;
qterm({unfinished, Boolean}, _Context) ->
    %% Filter on items whose start date lies in the future
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end >= current_timestamp">>
                ]
            };
        false ->
            []
    end;
qterm({unfinished_or_nodate, Boolean}, _Context) ->
    %% Filter on items whose start date lies in the future or don't have an end_date
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"(rsc.pivot_date_end >= current_date "
                      "or rsc.pivot_date_start is null)">>
                ]
            };
        false ->
            []
    end;
qterm({is_authoritative, Boolean}, _Context) ->
    %% authoritative={true|false}
    %% Filter on items which are authoritative or not
    #search_sql_term{
        where = [
            <<"rsc.is_authoritative = ">>, '$1'
        ],
        args = [
            z_convert:to_bool(Boolean)
        ]
    };
qterm({creator_id, Id}, Context) ->
    %% creator_id=<rsc id>
    %% Filter on items which are created by <rsc id>
    #search_sql_term{
        where = [
            <<"rsc.creator_id = ">>, '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
qterm({modifier_id, Id}, Context) ->
    %% modifier_id=<rsc id>
    %% Filter on items which are last modified by <rsc id>
    #search_sql_term{
        where = [
            <<"rsc.modifier_id = ">>, '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
qterm({qargs, Boolean}, Context) ->
    %% qargs
    %% Add all query terms from the current query arguments
    case z_convert:to_bool(Boolean) of
        true ->
            Terms = parse_request_args(qargs(Context)),
            qterm(Terms, Context);
        false ->
            []
    end;
qterm({query_id, Id}, Context) ->
    %% query_id=<rsc id>
    %% Get the query terms from given resource ID, and use those terms.
    QArgs = try
        parse_query_text(z_html:unescape(m_rsc:p(Id, 'query', Context)))
    catch
        throw:{error,{unknown_query_term,Term}} ->
            ?LOG_ERROR(#{
                text => <<"Unknown query term in search query">>,
                result => error,
                reason => unknown_query_term,
                query_id => Id,
                term => Term
            }),
            []
    end,
    qterm(QArgs, Context);
qterm({rsc_id, Id}, Context) ->
    %% rsc_id=<rsc id>
    %% Filter to *only* include the given rsc id. Can be used for resource existence check.
    #search_sql_term{
        where = [
            <<"rsc.id = ">>, '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
qterm({name, Name}, Context) ->
    %% name=<name-pattern>
    %% Filter on the unique name of a resource.
    case z_string:to_lower(mod_search:trim(z_convert:to_binary(Name), Context)) of
        All when All =:= <<>>; All =:= <<"*">>; All =:= <<"%">> ->
            #search_sql_term{
                where = [
                    <<"rsc.name is not null">>
                ]
            };
        Name1 ->
            Name2 = binary:replace(Name1, <<"*">>, <<"%">>, [global]),
            #search_sql_term{
                where = [
                    <<"rsc.name like ">>, '$1'
                ],
                args = [
                    Name2
                ]
            }
    end;
qterm({language, []}, _Context) ->
    %% language=<iso-code>
    %% Filter on the presence of a translation
    [];
qterm({language, [ Lang | _ ] = Langs}, Context) when not is_integer(Lang) ->
    lists:map(
        fun(Code) ->
            qterm({language, Code}, Context)
        end,
        Langs);
qterm({language, Lang}, _Context) ->
    case z_language:to_language_atom(Lang) of
        {ok, Code} ->
            #search_sql_term{
                where = [
                    <<"rsc.language @> ">>, '$1'
                ],
                args = [
                    [ z_convert:to_binary(Code) ]
                ]
            };
        {error, _} ->
            % Unknown iso code, ignore
            []
    end;
qterm({sort, Sort}, _Context) ->
    %% sort=fieldname
    %% Order by a given field. Putting a '-' in front of the field name reverts the ordering.
    sort_term(Sort);
qterm({asort, Sort}, _Context) ->
    asort_term(Sort);
qterm({zsort, Sort}, _Context) ->
    zsort_term(Sort);
qterm({{facet, Field}, <<"[", _>> = V}, Context) ->
    %% facet.foo=value
    %% Add a join with the search_facet table.
    V1 = maybe_split_list(V),
    qterm({{facet, Field}, V1}, Context);
qterm({{facet, Field}, V}, Context) ->
    case search_facet:qterm(sql_safe(Field), V, Context) of
        {ok, Res1} ->
            Res1;
        {error, _} ->
            none
    end;
qterm({filter, R}, Context) ->
    add_filters(R, Context);
qterm({{filter, Field}, V}, Context) ->
    {Tab, Col, Q1} = map_filter_column(Field, #search_sql_term{}),
    case pivot_qterm(Tab, Col, V, Q1, Context) of
        {ok, QTerm} ->
            QTerm;
        {error, _} ->
            none
    end;
qterm({text, Text}, Context) ->
    %% text=...
    %% Perform a fulltext search
    case mod_search:trim(z_convert:to_binary(Text), Context) of
        <<>> ->
            [];
        <<"id:", S/binary>> ->
            #search_sql_term{
                where = [
                    <<"rsc.id = $1">>
                ],
                args = [
                    m_rsc:rid(S, Context)
                ]
            };
        _ ->
            TsQuery = mod_search:to_tsquery(Text, Context),
            #search_sql_term{
                where = [
                    '$1', <<"@@ rsc.pivot_tsv">>
                ],
                sort = [
                    [
                      "ts_rank_cd(", mod_search:rank_weight(Context),
                      ", rsc.pivot_tsv, ", '$1', ", ", '$2', ") desc"
                    ]
                ],
                args = [
                    TsQuery,
                    mod_search:rank_behaviour(Context)
                ]
            }
    end;
qterm({match_objects, RId}, Context) ->
    %% match_objects=<id>
    %% Match on the objects of the resource, best matching return first.
    %% Similar to the {match_objects id=...} query.
    case m_rsc:rid(RId, Context) of
        undefined ->
            none;
        Id ->
            ObjectIds = m_edge:objects(Id, Context),
            qterm([
                {match_object_ids, ObjectIds},
                {id_exclude, Id}
            ], Context)
    end;
qterm({match_object_ids, ObjectIds}, Context) ->
    ObjectIds1 = [ m_rsc:rid(OId, Context) || OId <- lists:flatten(ObjectIds) ],
    MatchTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds1, is_integer(ObjId) ],
    TsQuery = lists:flatten(lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            none;
        _ ->
            #search_sql_term{
                tables = #{
                    <<"matchquery">> => [ <<"to_tsquery(">>, '$1', <<")">> ]
                },
                where = [
                    <<"matchquery @@ rsc.pivot_rtsv">>
                ],
                sort = [
                    <<"ts_rank(rsc.pivot_rtsv, matchquery) desc">>
                ],
                args = [
                    TsQuery
                ]
            }
    end;
qterm({date_start_after, Date}, Context) ->
    %% date_start_after=date
    %% Filter on date_start after a specific date.
    #search_sql_term{
        where = [
            <<"rsc.pivot_date_start >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm({date_start_before, Date}, Context) ->
    %% date_start_after=date
    %% Filter on date_start before a specific date.
    #search_sql_term{
        where = [
            <<"rsc.pivot_date_start <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm({date_start_year, Year}, _Context) ->
    %% date_start_year=year
    %% Filter on year of start date
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.pivot_date_start) ">>, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm({date_end_after, Date}, Context) ->
    %% date_end_after=date
    %% Filter on date_end after a specific date.
    #search_sql_term{
        where = [
            <<"rsc.pivot_date_end >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm({date_end_before, Date}, Context) ->
    %% date_end_after=date
    %% Filter on date_end before a specific date.
    #search_sql_term{
        where = [
            <<"rsc.pivot_date_end <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm({date_end_year, Year}, _Context) ->
    %% date_end_year=year
    %% Filter on year of end date
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.pivot_date_end) = ">>, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm({publication_year, Year}, _Context) ->
    %% publication_year=year
    %% Filter on year of publication
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.publication_start) = ">>, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm({publication_month, Month}, _Context) ->
    %% publication_month=month
    %% Filter on month of publication
    #search_sql_term{
        where = [
            <<"date_part('month', rsc.publication_start) = ">>, '$1'
        ],
        args = [
            z_convert:to_integer(Month)
        ]
    };
qterm({publication_after, Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.publication_start >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm({publication_before, Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.publication_start <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(Term, _Context) ->
    %% No match found
    throw({error, {unknown_query_term, Term}}).

%%
%% Helper functions
%%

%% @doc Parse hassubject and hasobject edges.
-spec parse_edges(hassubject | hasobject, list(), z:context()) -> #search_sql_term{}.
parse_edges(Term, [[Id, Predicate]], Context) ->
    parse_edges(Term, [[Id, Predicate, "rsc"]], Context);
parse_edges(hassubject, [[Id, Predicate, JoinAlias]], Context) ->
    Alias = edge_alias(),
    JoinAlias1 = sql_safe(JoinAlias),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".object_id = ">>, JoinAlias1, <<".id">>,
            <<" and ">>, Alias, <<".subject_id = ">>, '$1',
            <<" and ">>, Alias, <<".predicate_id = ">>, '$2'
        ],
        args = [
            m_rsc:rid(Id, Context),
            predicate_to_id(Predicate, Context)
        ]
    };
parse_edges(hassubject, [Id], Context) ->
    Alias = edge_alias(),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".object_id = rsc.id">>,
            <<" and ">>, Alias, <<".subject_id = ">>,
            '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
parse_edges(hasobject, [[Id, Predicate, JoinAlias]], Context) ->
    Alias = edge_alias(),
    JoinAlias1 = sql_safe(JoinAlias),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".subject_id = ">>, JoinAlias1, <<".id">>,
            <<" and ">>, Alias, <<".object_id = ">>, '$1',
            <<" and ">>, Alias, <<".predicate_id = ">>, '$2'
        ],
        args = [
            m_rsc:rid(Id, Context),
            predicate_to_id(Predicate, Context)
        ]
    };
parse_edges(hasobject, [Id], Context) ->
    Alias = edge_alias(),
    #search_sql_term{
        tables = #{
            Alias => <<"edge">>
        },
        where = [
            Alias, <<".subject_id = rsc.id">>,
            <<" and ">>, Alias, <<".object_id = ">>,
            '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    }.

edge_alias() ->
    Nr = z_ids:identifier(6),
    <<"edge_", Nr/binary>>.


%% Add a join on the hierarchy table.
% add_hierarchy_join(HierarchyName, Lft, Rght, Search) ->
%     {NameArg, Search1} = add_arg(HierarchyName, Search),
%     {LftArg, Search2} = add_arg(Lft, Search1),
%     {RghtArg, Search3} = add_arg(Rght, Search2),
%     A = "h" ++ integer_to_list(length(Search#search_sql.tables)),
%     Search4 = add_where(
%                      A ++ ".name = " ++ NameArg ++ " AND "
%                   ++ A ++ ".lft >= " ++ LftArg ++ " AND "
%                   ++ A ++ ".rght <= " ++ RghtArg, Search3),
%     Search4#search_sql{
%       tables=Search1#search_sql.tables ++ [{hierarchy, A}],
%       from=Search1#search_sql.from ++ ", hierarchy " ++ A
%      }.

zsort_term(Sort) ->
    T = add_order(Sort, #search_sql_term{}),
    #search_sql_term{
        zsort = T#search_sql_term.sort
    }.

asort_term(Sort) ->
    T = add_order(Sort, #search_sql_term{}),
    #search_sql_term{
        asort = T#search_sql_term.sort
    }.

sort_term(Sort) ->
    add_order(Sort, #search_sql_term{}).

%% Add an ORDER clause.
add_order(<<>>, Search) ->
    Search;
add_order([ Order | Os ], Search) when not is_integer(Order) ->
    Search1 = add_order(Order, Search),
    add_order(Os, Search1);
add_order(Order, Search) when is_atom(Order) ->
    add_order(atom_to_binary(Order, utf8), Search);
add_order(Order, Search) when is_list(Order) ->
    add_order(list_to_binary(Order), Search);
add_order(<<_, "random">>, Search) ->
    Search#search_sql_term{
        sort = Search#search_sql_term.sort
               ++ [ <<"random()">> ]
    };
add_order(<<C, "seq">>, Search) when C =:= $-; C =:= $+ ->
    Search#search_sql_term{
        sort = Search#search_sql_term.sort
               ++ [ {edge, C, <<"seq">>}, {edge, C, <<"id">>} ]
    };
add_order(<<C, "edge.", Column/binary>>, Search) when C =:= $-; C =:= $+ ->
    Column1 = sql_safe(Column),
    Search#search_sql_term{
        sort = Search#search_sql_term.sort
               ++ [ {edge, C, Column1} ]
    };
add_order(<<C, "pivot.", Pivot/binary>>, Search)  when C =:= $-; C =:= $+ ->
    case binary:split(Pivot, <<".">>) of
        [ PivotTable, Column ] ->
            Tab1 = sql_safe(PivotTable),
            Col1 = <<"pivot_", Column/binary>>,
            Col2 = sql_safe(Col1),
            Join = Search#search_sql_term.join_inner,
            Search#search_sql_term{
                join_inner = Join#{
                    Tab1 => {Tab1, <<Tab1/binary, ".id = rsc.id">>}
                },
                sort = Search#search_sql_term.sort
                        ++ [ {Tab1, C, Col2} ]
            };
        [ Column ] ->
            Col1 = <<"pivot_", Column/binary>>,
            Col2 = sql_safe(Col1),
            Search#search_sql_term{
                sort = Search#search_sql_term.sort
                       ++ [ {<<"rsc">>, C, Col2} ]
            }
    end;
add_order(<<C, "facet.", Column/binary>>, Search)  when C =:= $-; C =:= $+ ->
    Col1 = <<"f_", Column/binary>>,
    Col2 = sql_safe(Col1),
    Join = Search#search_sql_term.join_inner,
    Search#search_sql_term{
        join_inner = Join#{
            <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
        },
        sort = Search#search_sql_term.sort
               ++ [ {<<"facet">>, C, Col2} ]
    };
add_order(<<C, Sort/binary>>, Search) when C =:= $-; C =:= $+ ->
    Sort1 = sql_safe(Sort),
    [ Alias, Column ] = case binary:split(Sort1, <<".">>) of
        [ Col ] ->
            [ <<"rsc">>, Col ];
        [ _, _ ] = AC ->
            AC
    end,
    Search#search_sql_term{
        sort = Search#search_sql_term.sort
               ++ [ {Alias, C, Column} ]
    };
add_order(Sort, Search) ->
    add_order(<<"+", Sort/binary>>, Search).



%% Make sure that parts of the query are safe to append to the search query.
sql_safe(String) when is_list(String); is_binary(String) ->
    case re:run(String, ?SQL_SAFE_REGEXP) of
        {match, _} ->
            String;
        _ ->
            throw({error, {unsafe_expression, String}})
    end;
sql_safe(String) ->
    sql_safe(z_convert:to_binary(String)).


%% Make sure the input is a list of valid categories.
assure_categories(Name, Context) ->
    Cats = assure_cats_list(Name),
    Cats1 = assure_cat_flatten(Cats),
    lists:foldl(fun(C, Acc) ->
                    case assure_category(C, Context) of
                        undefined -> Acc;
                        error -> ['$error'|Acc];
                        {ok, N} -> [N|Acc]
                    end
                end,
                [],
                Cats1).

%% Make a single category a list
assure_cats_list(Names) when is_list(Names) ->
    Names;
assure_cats_list(Name) ->
    [ Name ].

%% Split strings with comma separated lists of categories
-spec assure_cat_flatten(any() | list()) -> list().
assure_cat_flatten(Names) ->
    lists:flatten(
        lists:map(
            fun
                (S) when is_binary(S) ->
                    binary:split(S, <<",">>, [ global ]);
                (Name) ->
                    Name
            end,
            Names)).

%% Make sure the given name is a category.
assure_category(undefined, _) -> undefined;
assure_category(null, _) -> undefined;
assure_category("", _) -> undefined;
assure_category("*", _) -> undefined;
assure_category(<<>>, _) -> undefined;
assure_category(<<"*">>, _) -> undefined;
assure_category(<<$', _/binary>> = Name, Context) ->
    case binary:last(Name) of
        $' -> assure_category_1(z_string:trim(Name, $'), Context);
        _ -> assure_category_1(Name, Context)
    end;
assure_category(<<$", _/binary>> = Name, Context) ->
    case binary:last(Name) of
        $" -> assure_category_1(z_string:trim(Name, $"), Context);
        _ -> assure_category_1(Name, Context)
    end;
assure_category(Name, Context) ->
    assure_category_1(Name, Context).

assure_category_1(Name, Context) ->
    case m_category:name_to_id(Name, Context) of
        {ok, _Id} ->
            {ok, Name};
        _ ->
            case m_rsc:rid(Name, Context) of
                undefined ->
                    ?LOG_NOTICE(#{
                        text => <<"Query: unknown category">>,
                        name => Name
                    }),
                    % display_error([ ?__("Unknown category", Context), 32, $", z_html:escape(z_convert:to_binary(Name)), $" ], Context),
                    error;
                CatId ->
                    case m_category:id_to_name(CatId, Context) of
                        undefined ->
                            ?LOG_NOTICE(#{
                                text => <<"Query: term is not a category">>,
                                name => Name
                            }),
                            % display_error([ $", z_html:escape(z_convert:to_binary(Name)), $", 32, ?__("is not a category", Context) ], Context),
                            error;
                        Name1 ->
                            {ok, Name1}
                    end
            end
    end.

%% If the current user is an administrator or editor, show an error message about this search
% display_error(Msg, Context) ->
%     case z_acl:is_allowed(use, mod_admin, Context) of
%         true ->
%             ContextPruned = z_context:prune_for_async(Context),
%             z_session_page:add_script(z_render:growl_error(Msg, ContextPruned));
%         false ->
%             ok
%     end.

-spec pivot_qterm(Table, Column, Value, Q, Context) -> {ok, QResult} | {error, term()}
    when Table :: binary(),
         Column :: binary(),
         Value :: term(),
         Q :: #search_sql_term{},
         QResult :: #search_sql_term{},
         Context :: z:context().
pivot_qterm(_Tab, _Col, [], Q, _Context) ->
    {ok, Q};
pivot_qterm(Tab, Col, [Value], Q, Context) ->
    pivot_qterm_1(Tab, Col, Value, Q, Context);
pivot_qterm(Tab, Col, Vs, Q, Context) when is_list(Vs) ->
    % 'OR' query for all values
    Q2 = lists:foldl(
        fun(V, QAcc) ->
            case pivot_qterm_1(Tab, Col, V, QAcc, Context) of
                {ok, QAcc1} ->
                    QAcc1;
                {error, _} ->
                    QAcc
            end
        end,
        Q,
        Vs),
    Q3 = Q2#search_sql_term{
        where = [
            <<"(">>,
            lists:join(<<" OR ">>, Q2#search_sql_term.where),
            <<")">>
        ]
    },
    {ok, Q3};
pivot_qterm(Tab, Col, Value, Q, Context) ->
    pivot_qterm_1(Tab, Col, Value, Q, Context).

pivot_qterm_1(Tab, Col, Value, Query, Context) ->
    {Op, Value1} = extract_op(Value),
    case z_db:to_column_value(Tab, Col, Value1, Context) of
        {ok, Value2} ->
            {ArgN, Query2} = add_term_arg(Value2, Query),
            W = [
                <<Tab/binary, $., Col/binary>>, Op, ArgN
            ],
            Query3 = Query2#search_sql_term{
                where = Query2#search_sql_term.where ++ [ W ]
            },
            {ok, Query3};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                text => <<"Pivot value error, dropping query term.">>,
                result => error,
                reason => Reason,
                table => Tab,
                column => Col,
                value => Value1
            }),
            Error
    end.

add_term_arg(ArgValue, #search_sql_term{ args = Args } = Q) ->
    Arg = [$$] ++ integer_to_list(length(Args) + 1),
    {list_to_atom(Arg), Q#search_sql_term{args = Args ++ [ ArgValue ]}}.

extract_op(<<"=", V/binary>>) ->
    {"=", V};
extract_op(<<">", V/binary>>) ->
    {">", V};
extract_op(<<"<", V/binary>>) ->
    {"<", V};
extract_op(<<"<=", V/binary>>) ->
    {"<=", V};
extract_op(<<">=", V/binary>>) ->
    {">=", V};
extract_op(<<"!=", V/binary>>) ->
    {"<>", V};
extract_op(<<"<>", V/binary>>) ->
    {"<>", V};
extract_op(V) ->
    {"=", V}.




add_filters(Filters, Context) ->
    add_filters(Filters, #search_sql_term{}, Context).

%% Add filters
add_filters(<<"[", _/binary>> = Filter, Q, Context) ->
    add_filters(maybe_split_list(Filter), Q, Context);
add_filters([ [Column|_] | _ ] = Filters, Q, Context)
    when is_list(Column);
         is_binary(Column);
         is_atom(Column) ->
    add_filters_or(Filters, Q, Context);
add_filters({'or', Filters}, Q, Context) ->
    add_filters_or(Filters, Q, Context);
add_filters([Column, Value], R, Context) ->
    add_filters([Column, eq, Value], R, Context);
add_filters([Column, Operator, Value], Q, Context) ->
    {Tab, Col, Q1} = map_filter_column(Column, Q),
    case z_db:to_column_value(Tab, Col, Value, Context) of
        {ok, V1} ->
            {Expr, Q2} = create_filter(Tab, Col, Operator, V1, Q1),
            add_filter_where(Expr, Q2);
        {error, _} ->
            Q
    end.

add_filters_or(Filters, Q, Context) ->
    {Exprs, Q1} = lists:foldr(
                        fun(V, Acc) ->
                            add_filters_or_1(V, Acc, Context)
                        end,
                        {[], Q},
                        Filters),
    Or = [ "(", lists:join(<<" or ">>, Exprs), ")" ],
    add_filter_where(Or, Q1).

add_filters_or_1([ C, O, V ], {Es, QAcc}, Context) ->
    {Tab, Col, QAcc1} = map_filter_column(C, QAcc),
    case z_db:to_column_value(Tab, Col, V, Context) of
        {ok, V1} ->
            {E, QAcc2} = create_filter(Tab, Col, O, V1, QAcc1),
            {[E|Es], QAcc2};
        {error, _} ->
            {Es, QAcc}
    end;
add_filters_or_1([ C, V ], {Es, QAcc}, Context) ->
    add_filters_or_1([ C, eq, V ], {Es, QAcc}, Context).

create_filter(Tab, Col, Operator, null, Q) ->
    create_filter(Tab, Col, Operator, undefined, Q);
create_filter(Tab, Col, Operator, undefined, Q) ->
    Operator1 = map_filter_operator(Operator),
    {create_filter_null(Tab, Col, Operator1), Q};
create_filter(Tab, Col, Operator, Value, Q) ->
    {Arg, Q1} = add_filter_arg(Value, Q),
    Operator1 = map_filter_operator(Operator),
    {[Tab, $., Col, <<" ">>, Operator1, <<" ">>, Arg], Q1}.


map_filter_column(<<"pivot.", P/binary>>, #search_sql_term{ join_inner = Join } = Q) ->
    case binary:split(P, <<".">>) of
        [ Table, Field ] ->
            T1 = sql_safe(Table),
            T2 = <<"pivot_", T1/binary>>,
            F1 = sql_safe(Field),
            Q1 = Q#search_sql_term{
                join_inner = Join#{
                    T2 => {T2, <<T2/binary, ".id = rsc.id">>}
                }
            },
            {T2, F1, Q1};
        [ Field ] ->
            F1 = z_convert:to_binary(sql_safe(Field)),
            {<<"rsc">>, <<"pivot_", F1/binary>>, Q}
    end;
map_filter_column(<<"facet.", P/binary>>, #search_sql_term{ join_inner = Join } = Q) ->
    Q1 = Q#search_sql_term{
        join_inner = Join#{
            <<"facet">> => {<<"facet">>, <<"facet.id = rsc.id">>}
        }
    },
    Field = sql_safe(P),
    {<<"facet">>, <<"f_", Field/binary>>, Q1};
map_filter_column(Column, Q) ->
    Field = sql_safe(Column),
    {<<"rsc">>, Field, Q}.

%% Add an AND clause to the WHERE of a #search_sql_term
%% Clause is already supposed to be safe.
add_filter_where(Clause, #search_sql_term{ where = [] } = Q) ->
    Q#search_sql_term{ where = Clause };
add_filter_where(Clause, #search_sql_term{ where = C } = Q) ->
    Q#search_sql_term{ where = [ C, <<" and ">>, Clause ] }.

%% Append an argument to a #search_sql_term
add_filter_arg(ArgValue, #search_sql_term{ args = Args } = Q) ->
    Arg = [$$] ++ integer_to_list(length(Args) + 1),
    {list_to_atom(Arg), Q#search_sql_term{args = Args ++ [ ArgValue ]}}.

create_filter_null(Tab, Col, "=") ->
    [ Tab, $., Col, <<" is null">> ];
create_filter_null(Tab, Col, "<>") ->
    [ Tab, $., Col, <<" is not null">> ];
create_filter_null(_Tab, _Col, _Op) ->
    "false".

map_filter_operator(eq) -> "=";
map_filter_operator('=') -> "=";
map_filter_operator(ne) -> "<>";
map_filter_operator('<>') -> "<>";
map_filter_operator(gt) -> ">";
map_filter_operator('>') -> ">";
map_filter_operator(lt) -> "<";
map_filter_operator('<') -> "<";
map_filter_operator(gte) -> ">=";
map_filter_operator('>=') -> ">=";
map_filter_operator(lte) -> "<=";
map_filter_operator('<=') -> "<=";
map_filter_operator("=") -> "=";
map_filter_operator("<>") -> "<>";
map_filter_operator(">") -> ">";
map_filter_operator("<") -> "<";
map_filter_operator(">=") -> ">=";
map_filter_operator("<=") -> "<=";
map_filter_operator(<<"=">>) -> "=";
map_filter_operator(<<"<>">>) -> "<>";
map_filter_operator(<<">">>) -> ">";
map_filter_operator(<<"<">>) -> "<";
map_filter_operator(<<">=">>) -> ">=";
map_filter_operator(<<"<=">>) -> "<=";
map_filter_operator(Op) -> throw({error, {unknown_filter_operator, Op}}).


% Convert an expression like [123,hasdocument]
maybe_split_list(<<"[", _/binary>> = Term) ->
    unquote_all(search_parse_list:parse(Term));
maybe_split_list("[" ++ _ = Term) ->
    unquote_all(search_parse_list:parse(Term));
maybe_split_list(Other) ->
    [ Other ].

unquote_all(L) when is_list(L) ->
    lists:map(fun unquote_all/1, L);
unquote_all(B) when is_binary(B) ->
    unquot(z_string:trim(B));
unquote_all(T) ->
    T.

unquot(<<C, Rest/binary>>) when C =:= $'; C =:= $"; C =:= $` ->
    binary:replace(Rest, <<C>>, <<>>);
unquot([C|Rest]) when C =:= $'; C =:= $"; C =:= $` ->
    [ X || X <- Rest, X =/= C ];
unquot(B) ->
    B.

%% Expand the argument for hasanyobject, make pairs of {ObjectId,PredicateId}
expand_object_predicates(Bin, Context) when is_binary(Bin) ->
    map_rids(search_parse_list:parse(Bin), Context);
expand_object_predicates(OPs, Context) ->
    map_rids(OPs, Context).

map_rids({rsc_list, L}, Context) ->
    map_rids(L, Context);
map_rids(L, Context) when is_list(L) ->
    [ map_rid(unquot(X),Context) || X <- L, X =/= <<>> ];
map_rids(Id, Context) ->
    map_rid(Id, Context).

map_rid([], _Context) ->  {any, any};
map_rid([Obj,Pred|_], Context) -> {rid(Obj,Context),rid(Pred,Context)};
map_rid([Obj], Context) ->  {rid(Obj, Context), any};
map_rid(Obj, Context) ->  {rid(Obj, Context), any}.

rid(undefined, _Context) -> undefined;
rid(<<"*">>, _Context) -> any;
rid('*', _Context) -> any;
rid("*", _Context) -> any;
rid("", _Context) -> any;
rid(<<>>, _Context) -> any;
rid(Id, _Context) when is_integer(Id) -> Id;
rid(Id, Context) -> m_rsc:rid(Id, Context).

predicate_to_id([$'|_] = Name, Context) ->
    case lists:last(Name) of
        $' -> predicate_to_id_1(z_string:trim(Name, $'), Context);
        _ -> predicate_to_id_1(Name, Context)
    end;
predicate_to_id([$"|_] = Name, Context) ->
    case lists:last(Name) of
        $" -> predicate_to_id_1(z_string:trim(Name, $"), Context);
        _ -> predicate_to_id_1(Name, Context)
    end;
predicate_to_id(<<$', _/binary>> = Name, Context) ->
    case binary:last(Name) of
        $' -> predicate_to_id_1(z_string:trim(Name, $'), Context);
        _ -> predicate_to_id_1(Name, Context)
    end;
predicate_to_id(<<$", _/binary>> = Name, Context) ->
    case binary:last(Name) of
        $" -> predicate_to_id_1(z_string:trim(Name, $"), Context);
        _ -> predicate_to_id_1(Name, Context)
    end;
predicate_to_id(Pred, Context) ->
    predicate_to_id_1(Pred, Context).

predicate_to_id_1(Pred, Context) ->
    case m_predicate:name_to_id(Pred, Context) of
        {ok, Id} ->
            Id;
        {error, _} ->
            ?LOG_NOTICE(#{
                text => <<"Query: unknown predicate">>,
                predicate => Pred
            }),
            % display_error([ ?__("Unknown predicate", Context), 32, $", z_html:escape(z_convert:to_binary(Pred)), $" ], Context),
            0
    end.

%% Support routine for "hasanyobject"
object_predicate_clause(_Alias, undefined, undefined) ->
    "false";
object_predicate_clause(_Alias, _Object, undefined) ->
    "false";
object_predicate_clause(_Alias, undefined, _Predicate) ->
    "false";
object_predicate_clause(Alias, any, any) ->
    [Alias, ".subject_id = rsc.id"];
object_predicate_clause(Alias, any, PredicateId) when is_integer(PredicateId) ->
    [Alias, ".predicate_id = ", integer_to_list(PredicateId)];
object_predicate_clause(Alias, ObjectId, any) when is_integer(ObjectId) ->
    [Alias, ".object_id = ", integer_to_list(ObjectId)];
object_predicate_clause(Alias, ObjectId, PredicateId) when is_integer(PredicateId), is_integer(ObjectId) ->
    [Alias, ".object_id=", integer_to_list(ObjectId),
     " and ", Alias, ".predicate_id=", integer_to_list(PredicateId)].
