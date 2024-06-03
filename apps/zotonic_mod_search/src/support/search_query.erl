%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2023 Arjan Scherpenisse
%% @doc Handler for m.search[{query, Args..}]
%% @end

%% Copyright 2009-2023 Arjan Scherpenisse
%%
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
%% @todo: also return the options and paging?
-spec search(Query, Context) -> SqlTerms | EmptyResult when
    Query ::  map() | proplists:proplist(),
    Context :: z:context(),
    SqlTerms :: #search_sql_terms{},
    EmptyResult :: #search_result{}.
search(#{ <<"q">> := Qs }, Context) when is_list(Qs) ->
    % Qs1 = lists:flatten(
    %     lists:map(
    %         fun
    %             ({K, #{ <<"all">> := All, <<"any">> := Any }}) ->
    %                 All1 = filter_empty( lists:map(fun(V) -> {K, V} end, All) ),
    %                 case lists:filter(fun z_utils:is_empty/1, Any) of
    %                     [] -> All1;
    %                     Any1 -> [ {K, Any1} | All1 ]
    %                 end;
    %             ({K, #{ <<"all">> := All }}) ->
    %                 filter_empty( lists:map(fun(V) -> {K, V} end, All) );
    %             ({K, #{ <<"any">> := Any }}) ->
    %                 case lists:filter(fun z_utils:is_empty/1, Any) of
    %                     [] -> [];
    %                     Any1 -> {K, Any1}
    %                 end;
    %             (KV) ->
    %                 KV
    %         end,
    %         Qs)),
    Qs1 = filter_empty(Qs),
    build_query(lists:sort(Qs1), Context);
search(Query, Context) when is_map(Query) ->
    search(z_search_props:from_map(Query), Context);
search(Query, Context) when is_list(Query) ->
    search(z_search_props:from_list(Query), Context).

-spec build_query(list(), z:context()) -> #search_sql_terms{} | #search_result{}.
build_query(Terms, Context) ->
    Ts = lists:flatten(lists:map(fun(T) -> qterm(T, Context) end, Terms)),
    case lists:member(none(), Ts) of
        true ->
            #search_result{};
        false ->
            #search_sql_terms{ terms = Ts }
    end.


-spec parse_request_args( list( {binary(), term()} ) ) -> list( {binary(), term()} ).
parse_request_args(Args) ->
    parse_request_args(Args, []).

parse_request_args([], Acc) ->
    Acc;
parse_request_args([{K,V}|Rest], Acc) when is_binary(K) ->
    case z_context:is_zotonic_arg(K) of
        true ->
            parse_request_args(Rest, Acc);
        false ->
            case is_request_arg(K) of
                false -> parse_request_args(Rest, Acc);
                true -> parse_request_args(Rest, [{K, V}|Acc])
            end
    end;
parse_request_args([{K,V}|Rest], Acc) ->
    parse_request_args([{z_convert:to_binary(K), V}|Rest], Acc).


% Skip these
is_request_arg(<<"page">>) -> false;
is_request_arg(<<"pagelen">>) -> false;
is_request_arg(<<"options">>) -> false;
% Complain about deprecated terms
is_request_arg(<<"custompivot">>)         ->
    ?LOG_ERROR(#{
        in => zotonic_mod_search,
        text => <<"The query term 'custompivot' has been removed. Use filters with 'pivot:pivotname:field' instead.">>,
        result => error,
        reason => unknown_query_term,
        term => <<"custompivot">>
    }),
    false;
is_request_arg(_Term) ->
    true.


%% Private methods start here

%% @doc Drop all undefined query arguments.
filter_empty(Q) when is_list(Q) ->
    lists:filter(
        fun
            (#{ <<"terms">> := [] }) -> false;
            (#{ <<"value">> := undefined }) -> false;
            (#{ <<"value">> := null }) -> false;
            (#{ <<"value">> := [] }) -> false;
            (#{ <<"value">> := <<>> }) -> false;
            (#{}) -> true
        end,
        Q).

-spec qterm(Term, Context) -> QueryTerms when
    Term :: list() | map(),
    Context :: z:context(),
    QueryTerms :: list() | #search_sql_term{}.
qterm(undefined, _Context) ->
    [];
qterm([], _Context) ->
    [];
qterm(Ts, Context) when is_list(Ts) ->
    lists:flatten(lists:map(fun(T) -> qterm(T, Context) end, Ts));
qterm(#{ <<"operator">> := Op, <<"terms">> := Terms }, Context) ->
    #search_sql_nested{
        operator = z_convert:to_binary(Op),
        terms = qterm(Terms, Context)
    };
qterm(#{ <<"term">> := Term } = Q, Context) when is_atom(Term) ->
    qterm(Q#{ <<"term">> => atom_to_binary(Term, utf8) }, Context);
qterm(#{ <<"term">> := <<"cat">>, <<"value">> := Cats}, Context) ->
    %% cat=categoryname
    %% Filter results on a certain category.
    Cats1 = assure_categories(Cats, Context),
    % Cats2 = add_or_append(<<"rsc">>, Cats1, []),
    #search_sql_term{ cats = [ {<<"rsc">>, Cats1}] };
    % parse_query(Rest, Context, Result#search_sql{cats=Cats2});
qterm(#{ <<"term">> := <<"cat_exclude">>, <<"value">> := Cats}, Context) ->
    %% cat_exclude=categoryname
    %% Filter results outside a certain category.
    Cats1 = assure_categories(Cats, Context),
    #search_sql_term{ cats_exclude = [ {<<"rsc">>, Cats1} ] };
qterm(#{ <<"term">> := <<"cat_exact">>, <<"value">> := Cats}, Context) ->
    %% cat_exact=categoryname
    %% Filter results excactly of a category (excluding subcategories)
    Cats1 = assure_categories(Cats, Context),
    #search_sql_term{ cats_exact = [ {<<"rsc">>, Cats1} ] };
qterm(#{ <<"term">> := <<"content_group">>, <<"value">> := ContentGroup}, Context) when not is_list(ContentGroup) ->
    %% content_group=id
    case rid(ContentGroup, Context) of
        any ->
            #search_sql_term{ extra = [ no_content_group_check ] };
        undefined ->
            % Force an empty result
            none();
        CGId ->
            qterm(#{ <<"term">> => <<"content_group">>, <<"value">> => [ CGId ] }, Context)
        end;
qterm(#{ <<"term">> := <<"content_group">>, <<"value">> := ContentGroups}, Context) when is_list(ContentGroups) ->
    %% content_group=[id,..]
    %% Include only resources which are member of the given content groups (or of their children)
    Q = #search_sql_term{ extra = [ no_content_group_check ] },
    {WithDefaultGroup, GroupsAndSubgroups} =
        lists:foldl(
          fun (CGId, {DG, CGs}) ->
            case m_rsc:is_a(CGId, content_group, Context) of
                true ->
                    List = m_hierarchy:contains(<<"content_group">>, CGId, Context),
                    case m_rsc:p_no_acl(CGId, name, Context) of
                        <<"default_content_group">> ->
                            { true, CGs ++ List };
                        _ ->
                            { DG, CGs ++ List }
                    end;
                false ->
                    { DG, CGs ++ [CGId] }
                end
          end,
          {false, []},
          ContentGroups
         ),
    case WithDefaultGroup of
        true ->
            Q#search_sql_term{
                where = [
                    <<"(rsc.content_group_id = any(">>, '$1',
                    <<"::int[]) or rsc.content_group_id is null)">>
                ],
                args = [ GroupsAndSubgroups ]
            };
        false ->
            Q#search_sql_term{
                where = [
                    <<"rsc.content_group_id = any(">>, '$1',
                    <<"::int[])">>
                ],
                args = [ GroupsAndSubgroups ]
            }
    end;
qterm(#{ <<"term">> := <<"visible_for">>, <<"value">> := VisFor}, _Context) when is_list(VisFor) ->
    %% visible_for=[5,6]
    %% Filter results for visibility levels
    try
        VisFor1 = lists:map(fun z_convert:to_integer/1, VisFor),
        VisFor2 = lists:filter(fun is_integer/1, VisFor1),
        #search_sql_term{
            where = [ <<"rsc.visible_for = any(">>, '$1', <<"::int[])">> ],
            args = [ VisFor2 ]
        }
    catch
        error:badarg ->
            ?LOG_WARNING(#{
                in => zotonic_mod_search,
                text => <<"Search: error converting visible_for search term">>,
                result => error,
                reason => badarg,
                visible_for => VisFor
            }),
            []
    end;
qterm(#{ <<"term">> := <<"visible_for">>, <<"value">> := VisFor} = T, _Context) ->
    %% visible_for=5
    %% Filter results for a certain visibility level
    try
        case z_convert:to_integer(VisFor) of
            undefined ->
                [];
            VisFor1 ->
                Op = extract_term_op(T, <<"=">>),
                #search_sql_term{
                    where = [ <<"rsc.visible_for">>, Op, '$1'],
                    args = [ VisFor1 ]
                }
        end
    catch
        error:badarg ->
            ?LOG_WARNING(#{
                in => zotonic_mod_search,
                text => <<"Search: error converting visible_for search term">>,
                result => error,
                reason => badarg,
                visible_for => VisFor
            }),
            []
    end;
qterm(#{ <<"term">> := <<"id_exclude">>, <<"value">> := Ids}, Context) when is_list(Ids) ->
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
            <<"rsc.id <> any(">>, '$1', <<"::int[])">>
        ],
        args = [ RscIds ]
    };
qterm(#{ <<"term">> := <<"id_exclude">>, <<"value">> := Id}, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        RscId ->
            #search_sql_term{
                where = [ <<"rsc.id <> ">>, '$1'],
                args = [ RscId ]
            }
    end;
qterm(#{ <<"term">> := <<"id">>, <<"value">> := Ids}, Context) when is_list(Ids) ->
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
            <<"rsc.id = any(">>, '$1', <<"::int[])">>
        ],
        args = [ RscIds ]
    };
qterm(#{ <<"term">> := <<"id">>, <<"value">> := Id} = T, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            [];
        RscId ->
            Op = extract_term_op(T, <<"=">>),
            #search_sql_term{
                where = [ <<"rsc.id">>, Op, '$1' ],
                args = [ RscId ]
            }
    end;
qterm(#{ <<"term">> := <<"hasmedium">>, <<"value">> := HasMedium}, _Context) ->
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
qterm(#{ <<"term">> := <<"hassubject">>, <<"value">> := Subject }, Context) ->
    parse_edges(hassubject, Subject, Context);
qterm(#{ <<"term">> := <<"hasobject">>, <<"value">> := Object }, Context) ->
    parse_edges(hasobject, Object, Context);
qterm(#{ <<"term">> := <<"hasanyobject">>, <<"value">> := ObjPreds}, Context) ->
    %% hasanyobject=[[id,predicate]|id, ...]
    %% Give all things which have an outgoing edge to Id with any of the given object/predicate combinations
    OPs = expand_object_predicates(ObjPreds, Context),
    % rsc.id in (select subject_id from edge where (object_id = ... and predicate_id = ... ) or (...) or ...)
    OPClauses = [ object_predicate_clause(Obj, Pred) || {Obj, Pred} <- OPs ],
    #search_sql_term{
        where = [
            "rsc.id in (select subject_id from edge where (",
                lists:join(") or (", OPClauses),
            "))"
        ]
    };
qterm(#{ <<"term">> := <<"hasanysubject">>, <<"value">> := ObjPreds}, Context) ->
    %% hasanysubbject=[[id,predicate]|id, ...]
    %% Give all things which have an incoming edge to Id with any of the given subject/predicate combinations
    OPs = expand_object_predicates(ObjPreds, Context),
    OPClauses = [ subject_predicate_clause(Obj, Pred) || {Obj, Pred} <- OPs ],
    #search_sql_term{
        where = [
            "rsc.id in (select object_id from edge where (",
                lists:join(") or (", OPClauses),
            "))"
        ]
    };
qterm(#{ <<"term">> := <<"hasobjectpredicate">>, <<"value">> := Predicate}, Context) ->
    %% hasobjectpredicate=predicate
    %% Give all things which have any outgoing edge with given predicate
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.subject_id = rsc.id AND edge.predicate_id = ">>, '$1', <<")">>
        ],
        args = [
            predicate_to_id(Predicate, Context)
        ]
    };
qterm(#{ <<"term">> := <<"hassubjectpredicate">>, <<"value">> := Predicate}, Context) ->
    %% hassubjectpredicate=predicate
    %% Give all things which have any incoming edge with given predicate
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.object_id = rsc.id AND edge.predicate_id = ">>, '$1', <<")">>
        ],
        args = [
            predicate_to_id(Predicate, Context)
        ]
    };
qterm(#{ <<"term">> := <<"is_featured">>, <<"value">> := Boolean}, _Context) ->
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
qterm(#{ <<"term">> := <<"is_published">>, <<"value">> := Boolean}, _Context) ->
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
qterm(#{ <<"term">> := <<"is_public">>, <<"value">> := Boolean}, _Context) ->
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
qterm(#{ <<"term">> := <<"is_findable">>, <<"value">> := Boolean}, _Context) ->
    %% is_findable or is_findable={false,true}
    %% Filter on whether an item is findable or not.
    #search_sql_term{
        where = [
            <<"rsc.is_unfindable = ">>, '$1'
        ],
        args = [
            not z_convert:to_bool(Boolean)
        ]
    };
qterm(#{ <<"term">> := <<"is_unfindable">>, <<"value">> := Boolean}, _Context) ->
    %% is_unfindable or is_unfindable={false,true}
    %% Filter on whether an item is unfindable or not.
    #search_sql_term{
        where = [
            <<"rsc.is_unfindable = ">>, '$1'
        ],
        args = [
            z_convert:to_bool(Boolean)
        ]
    };
qterm(#{ <<"term">> := <<"is_protected">>, <<"value">> := Boolean}, _Context) ->
    %% is_protected or is_protected={false,true}
    %% Filter on whether an item is protected or not.
    #search_sql_term{
        where = [
            <<"rsc.is_protected = ">>, '$1'
        ],
        args = [
            z_convert:to_bool(Boolean)
        ]
    };
qterm(#{ <<"term">> := <<"is_dependent">>, <<"value">> := Boolean}, _Context) ->
    %% is_dependent or is_dependent={false,true}
    %% Filter on whether an item is dependent or not.
    #search_sql_term{
        where = [
            <<"rsc.is_dependent = ">>, '$1'
        ],
        args = [
            z_convert:to_bool(Boolean)
        ]
    };
qterm(#{ <<"term">> := <<"upcoming">>, <<"value">> := Boolean}, _Context) ->
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
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start < current_timestamp">>
                ]
            }
    end;
qterm(#{ <<"term">> := <<"upcoming_on">>, <<"value">> := DateTime}, Context) ->
    %% upcoming_on
    %% Filter on items whose start date lies after the datetime
    case z_datetime:to_datetime(DateTime, Context) of
        {_,_} = DT ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start >= ">>, '$1'
                ],
                args = [ DT ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"upcoming_date">>, <<"value">> := Date}, Context) ->
    %% upcoming_date
    %% Filter on items whose start date lies after the date
    case z_datetime:to_datetime(Date, Context) of
        {Day,_} ->
            Start = z_datetime:to_utc({Day, {0,0,0}}, Context),
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start >= ">>, '$1'
                ],
                args = [ Start ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"ongoing">>, <<"value">> := Boolean}, _Context) ->
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
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start > current_timestamp ",
                      "or rsc.pivot_date_end < current_timestamp">>
                ]
            }
    end;
qterm(#{ <<"term">> := <<"ongoing_on">>, <<"value">> := DateTime}, Context) ->
    %% ongoing_on
    %% Filter on items whose date range is around the given date
    case z_datetime:to_datetime(DateTime, Context) of
        {_,_} = DT ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start <= ">>, '$1',
                    <<" and rsc.pivot_date_end >= ">>, '$1'
                ],
                args = [ DT ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"ongoing_date">>, <<"value">> := Date}, Context) ->
    %% ongoing_date
    %% Filter on items whose date range is around the given day
    case z_datetime:to_datetime(Date) of
        {Day,_} ->
            Start = z_datetime:to_utc({Day, {0,0,0}}, Context),
            End = z_datetime:to_utc({Day, {23,59,59}}, Context),
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_start <= ">>, '$1',
                    <<" and rsc.pivot_date_end >= ">>, '$2'
                ],
                args = [ End, Start ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"finished">>, <<"value">> := Boolean}, _Context) ->
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
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end >= current_timestamp">>
                ]
            }
    end;
qterm(#{ <<"term">> := <<"finished_on">>, <<"value">> := DateTime}, Context) ->
    %% finished_on
    %% Filter on items whose end date lies before a given moment
    case z_datetime:to_datetime(DateTime, Context) of
        {_,_} = DT ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end < ">>, '$1'
                ],
                args = [ DT ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"finished_date">>, <<"value">> := Date}, Context) ->
    %% finished_date
    %% Filter on items whose end date lies before a date
    case z_datetime:to_datetime(Date) of
        {Day,_} ->
            Start = z_datetime:to_utc({Day, {0,0,0}}, Context),
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end < ">>, '$1'
                ],
                args = [ Start ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"unfinished">>, <<"value">> := Boolean}, _Context) ->
    %% Filter on items whose end date lies in the future
    case z_convert:to_bool(Boolean) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end >= current_timestamp">>
                ]
            };
        false ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end < current_timestamp">>
                ]
            }
    end;
qterm(#{ <<"term">> := <<"unfinished_on">>, <<"value">> := DateTime}, Context) ->
    %% Filter on items whose end date lies after the given moment
    case z_datetime:to_datetime(DateTime, Context) of
        {_,_} = DT ->
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end >= ">>, '$1'
                ],
                args = [ DT ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"unfinished_date">>, <<"value">> := Date}, Context) ->
    %% Filter on items whose end date lies after the date
    case z_datetime:to_datetime(Date, Context) of
        {Day,_} ->
            Start = z_datetime:to_utc({Day, {0,0,0}}, Context),
            #search_sql_term{
                where = [
                    <<"rsc.pivot_date_end >= ">>, '$1'
                ],
                args = [ Start ]
            };
        undefined ->
            []
    end;
qterm(#{ <<"term">> := <<"unfinished_or_nodate">>, <<"value">> := Boolean}, _Context) ->
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
            #search_sql_term{
                where = [
                    <<"(rsc.pivot_date_end < current_date "
                      "and rsc.pivot_date_start is not null)">>
                ]
            }
    end;
qterm(#{ <<"term">> := <<"is_authoritative">>, <<"value">> := Boolean}, _Context) ->
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
qterm(#{ <<"term">> := <<"creator_id">>, <<"value">> := Id}, Context) ->
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
qterm(#{ <<"term">> := <<"modifier_id">>, <<"value">> := Id}, Context) ->
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
qterm(#{ <<"term">> := <<"qargs">>, <<"value">> := Boolean}, Context) ->
    %% qargs
    %% Add all query terms from the current query arguments
    case z_convert:to_bool(Boolean) of
        true ->
            #{ <<"q">> := Terms } = z_search_props:from_qargs(Context),
            qterm(Terms, Context);
        false ->
            []
    end;
qterm(#{ <<"term">> := <<"query_id">>, <<"value">> := Id}, Context) ->
    %% query_id=<rsc id>
    %% Get the query terms from given resource ID, and use those terms.
    QueryText = z_html:unescape(m_rsc:p(Id, <<"query">>, Context)),
    QueryTerms = try
        #{ <<"q">> := Terms } = z_search_props:from_text(QueryText),
        Terms
    catch
        throw:{error,{unknown_query_term,Term}}:S ->
            ?LOG_ERROR(#{
                text => <<"Unknown query term in search query">>,
                in => zotonic_mod_search,
                result => error,
                reason => unknown_query_term,
                query_id => Id,
                term => Term,
                stack => S
            }),
            []
    end,
    qterm(QueryTerms, Context);
qterm(#{ <<"term">> := <<"rsc_id">>, <<"value">> := Id} = T, Context) ->
    %% rsc_id=<rsc id>
    %% Filter to *only* include the given rsc id. Can be used for resource existence check.
    Op = extract_term_op(T, <<"=">>),
    #search_sql_term{
        where = [
            <<"rsc.id">>, Op, '$1'
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
qterm(#{ <<"term">> := <<"name">>, <<"value">> := Name} = T, Context) ->
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
            case binary:match(Name2, <<"%">>) of
                nomatch ->
                    Op = extract_term_op(T, <<"=">>),
                    #search_sql_term{
                        where = [
                            <<"rsc.name">>, Op, '$1'
                        ],
                        args = [
                            Name2
                        ]
                    };
                {_,_} ->
                    #search_sql_term{
                        where = [
                            <<"rsc.name like ">>, '$1'
                        ],
                        args = [
                            Name2
                        ]
                    }
            end
    end;
qterm(#{ <<"term">> := <<"language">>, <<"value">> := []}, _Context) ->
    %% language=<iso-code>
    %% Filter on the presence of a translation
    [];
qterm(#{ <<"term">> := <<"language">>, <<"value">> := [ Lang | _ ] = Langs}, Context) when is_list(Lang) ->
    lists:map(
        fun(Code) ->
            qterm(#{ <<"term">> => <<"language">>, <<"value">> => Code }, Context)
        end,
        Langs);
qterm(#{ <<"term">> := <<"language">>, <<"value">> := [ Lang | _ ] = Langs}, Context) when is_atom(Lang); is_binary(Lang) ->
    Langs1 = lists:map(
        fun(Lng) ->
            case to_language_atom(Lng, Context) of
                {ok, Code} ->
                    z_convert:to_binary(Code);
                {error, _} ->
                    <<"x-default">>
            end
        end,
        Langs),
    #search_sql_term{
        where = [
            <<"rsc.language && ">>, '$1'
        ],
        args = [ lists:usort(Langs1) ]
    };
qterm(#{ <<"term">> := <<"language">>, <<"value">> := Lang}, Context) ->
    case to_language_atom(Lang, Context) of
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
qterm(#{ <<"term">> := <<"notlanguage">>, <<"value">> := []}, _Context) ->
    %% notlanguage=<iso-code>
    %% Filter on the presence of a translation
    [];
qterm(#{ <<"term">> := <<"notlanguage">>, <<"value">> := [ Lang | _ ] = Langs}, Context) when is_list(Lang) ->
    lists:map(
        fun(Code) ->
            qterm(#{ <<"term">> => <<"notlanguage">>, <<"value">> => Code }, Context)
        end,
        Langs);
qterm(#{ <<"term">> := <<"notlanguage">>, <<"value">> := [ Lang | _ ] = Langs}, Context) when is_atom(Lang); is_binary(Lang) ->
    Langs1 = lists:map(
        fun(Lng) ->
            case to_language_atom(Lng, Context) of
                {ok, Code} ->
                    z_convert:to_binary(Code);
                {error, _} ->
                    <<"x-default">>
            end
        end,
        Langs),
    #search_sql_term{
        where = [
            <<"not (rsc.language && ">>, '$1', <<")">>
        ],
        args = [ lists:usort(Langs1) ]
    };
qterm(#{ <<"term">> := <<"notlanguage">>, <<"value">> := Lang}, Context) ->
    case to_language_atom(Lang, Context) of
        {ok, Code} ->
            #search_sql_term{
                where = [
                    <<"not (rsc.language @> ">>, '$1', <<")">>
                ],
                args = [
                    [ z_convert:to_binary(Code) ]
                ]
            };
        {error, _} ->
            % Unknown iso code, ignore
            []
    end;
qterm(#{ <<"term">> := <<"sort">>, <<"value">> := Sort}, _Context) ->
    %% sort=fieldname
    %% Order by a given field. Putting a '-' in front of the field name reverts the ordering.
    sort_term(Sort);
qterm(#{ <<"term">> := <<"asort">>, <<"value">> := Sort}, _Context) ->
    asort_term(Sort);
qterm(#{ <<"term">> := <<"zsort">>, <<"value">> := Sort}, _Context) ->
    zsort_term(Sort);
qterm(#{ <<"term">> := <<"facet">>, <<"value">> := V }, Context) when is_map(V) ->
    maps:fold(
        fun(Field, FV, Acc) ->
            Term = qterm(#{
                    <<"term">> => <<"facet:", Field/binary>>,
                    <<"value">> => FV
                }, Context),
            [ Term | Acc ]
        end,
        [],
        V);
qterm(#{ <<"term">> := <<"facet:", Field/binary>>, <<"value">> := V}, Context) ->
    case search_facet:qterm(sql_safe(Field), V, Context) of
        {ok, Res1} ->
            Res1;
        {error, _} ->
            none()
    end;
qterm(#{ <<"term">> := <<"filter">>, <<"value">> := V }, Context) when is_map(V) ->
    maps:fold(
        fun(Field, FV, Acc) ->
            Term = qterm(#{
                    <<"term">> => <<"filter:", Field/binary>>,
                    <<"value">> => FV
                }, Context),
            [ Term | Acc ]
        end,
        [],
        V);
qterm(#{ <<"term">> := <<"filter">>, <<"value">> := R}, Context) ->
    add_filters(R, Context);
qterm(#{ <<"term">> := <<"filter:", Field/binary>>, <<"value">> := V } = T, Context) ->
    {Tab, Alias, Col, Q1} = map_filter_column(Field, #search_sql_term{}),
    Op = extract_term_op(T, undefined),
    case pivot_qterm(Tab, Alias, Col, Op, V, Q1, Context) of
        {ok, QTerm} ->
            QTerm;
        {error, _} ->
            none()
    end;
qterm(#{ <<"term">> := <<"pivot:", _/binary>> = Field, <<"value">> := V} = T, Context) ->
    {Tab, Alias, Col, Q1} = map_filter_column(Field, #search_sql_term{}),
    Op = extract_term_op(T, undefined),
    case pivot_qterm(Tab, Alias, Col, Op, V, Q1, Context) of
        {ok, QTerm} ->
            QTerm;
        {error, _} ->
            none()
    end;
qterm(#{ <<"term">> := <<"text">>, <<"value">> := Text}, Context) ->
    %% text=...
    %% Perform a fulltext search
    case mod_search:trim(z_convert:to_binary(Text), Context) of
        <<>> ->
            [];
        <<"id:", S/binary>> ->
            #search_sql_term{
                where = [
                    <<"rsc.id = ">>, '$1'
                ],
                args = [
                    m_rsc:rid(S, Context)
                ]
            };
        _ ->
            TsQuery = mod_search:to_tsquery(Text, Context),
            #search_sql_term{
                where = [
                    '$1', <<" @@ rsc.pivot_tsv">>
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
qterm(#{ <<"term">> := <<"match_objects">>, <<"value">> := RId}, Context) ->
    %% match_objects=<id>
    %% Match on the objects of the resource, best matching return first.
    %% Similar to the {match_objects id=...} query.
    case m_rsc:rid(RId, Context) of
        undefined ->
            none();
        Id ->
            ObjectIds = m_edge:objects(Id, Context),
            qterm(#{
                    <<"operator">> => <<"allof">>,
                    <<"terms">> => [
                        #{
                            <<"term">> => <<"match_object_ids">>,
                            <<"value">> => ObjectIds
                        },
                        #{
                            <<"term">> => <<"id_exclude">>,
                            <<"value">> => Id
                        }
                    ]
                },
                Context)
    end;
qterm(#{ <<"term">> := <<"match_object_ids">>, <<"value">> := ObjectIds}, Context) ->
    ObjectIds1 = [ m_rsc:rid(OId, Context) || OId <- lists:flatten(ObjectIds) ],
    MatchTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds1, is_integer(ObjId) ],
    TsQuery = lists:flatten(lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            none();
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
qterm(#{ <<"term">> := <<"date_start_after">>, <<"value">> := Date}, Context) ->
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
qterm(#{ <<"term">> := <<"date_start_before">>, <<"value">> := Date}, Context) ->
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
qterm(#{ <<"term">> := <<"date_start_year">>, <<"value">> := Year} = T, _Context) ->
    %% date_start_year=year
    %% Filter on year of start date
    Op = extract_term_op(T, <<"=">>),
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.pivot_date_start) ">>, Op, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm(#{ <<"term">> := <<"date_end_after">>, <<"value">> := Date}, Context) ->
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
qterm(#{ <<"term">> := <<"date_end_before">>, <<"value">> := Date}, Context) ->
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
qterm(#{ <<"term">> := <<"date_end_year">>, <<"value">> := Year} = T, _Context) ->
    %% date_end_year=year
    %% Filter on year of end date
    Op = extract_term_op(T, <<"=">>),
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.pivot_date_end)">>, Op, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm(#{ <<"term">> := <<"publication_year">>, <<"value">> := Year} = T, _Context) ->
    %% publication_year=year
    %% Filter on year of publication
    Op = extract_term_op(T, <<"=">>),
    #search_sql_term{
        where = [
            <<"date_part('year', rsc.publication_start)">>, Op, '$1'
        ],
        args = [
            z_convert:to_integer(Year)
        ]
    };
qterm(#{ <<"term">> := <<"publication_month">>, <<"value">> := Month} = T, _Context) ->
    %% publication_month=month
    %% Filter on month of publication
    Op = extract_term_op(T, <<"=">>),
    #search_sql_term{
        where = [
            <<"date_part('month', rsc.publication_start)">>, Op, '$1'
        ],
        args = [
            z_convert:to_integer(Month)
        ]
    };
qterm(#{ <<"term">> := <<"publication_after">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.publication_start >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := <<"publication_before">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.publication_start <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := <<"created_after">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.created >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := <<"created_before">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.created <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := <<"modified_after">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.modified >= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := <<"modified_before">>, <<"value">> := Date}, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.modified <= ">>, '$1'
        ],
        args = [
            z_datetime:to_datetime(Date, Context)
        ]
    };
qterm(#{ <<"term">> := Term, <<"value">> := Arg}, Context) ->
    case z_notifier:first(#search_query_term{ term = Term, arg = Arg }, Context) of
        undefined ->
            ?LOG_WARNING(#{
                in => zotonic_mod_search,
                text => <<"Ignored unknown query search term">>,
                term => Term,
                arg => Arg,
                result => error,
                reason => unknown_query_term
            }),
            [];
        [] ->
            [];
        Map when is_map(Map) ->
            qterm(Map, Context);
        List when is_list(List) ->
            qterm(List, Context);
        #search_sql_term{} = SQL ->
            SQL
    end.

%%
%% Helper functions
%%

none() ->
    #search_sql_term{
        where = <<" false ">>
    }.

to_language_atom(<<"z_language">>, Context) ->
    {ok, z_context:language(Context)};
to_language_atom(z_language, Context) ->
    {ok, z_context:language(Context)};
to_language_atom(Code, _Context) ->
    z_language:to_language_atom(Code).


%% @doc Parse hassubject and hasobject edges.
-spec parse_edges(hassubject | hasobject, list(), z:context()) -> #search_sql_term{}.
parse_edges(Term, [H|_] = Es, Context) when is_list(H) ->
    lists:map(
        fun(E) ->
            parse_edges(Term, E, Context)
        end,
        Es);
parse_edges(Term, Id, Context) when is_number(Id); is_binary(Id); is_atom(Id) ->
    parse_edges(Term, [Id], Context);
parse_edges(Term, [Id, Predicate], Context) ->
    parse_edges(Term, [Id, Predicate, <<"rsc">>], Context);
parse_edges(hassubject, [Id, Predicate, JoinAlias], Context) ->
    JoinAlias1 = sql_safe(JoinAlias),
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.object_id = ">>, JoinAlias1, <<".id">>,
                             <<" AND edge.subject_id = ">>, '$1',
                             <<" AND edge.predicate_id = ">>, '$2', <<")">>
        ],
        args = [
            m_rsc:rid(Id, Context),
            predicate_to_id(Predicate, Context)
        ]
    };
parse_edges(hassubject, [Id], Context) ->
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.object_id = rsc.id AND edge.subject_id = ">>, '$1', <<")">>
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    };
parse_edges(hasobject, [Id, Predicate, JoinAlias], Context) ->
    JoinAlias1 = sql_safe(JoinAlias),
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.subject_id = ">>, JoinAlias1, <<".id">>,
                             <<" AND edge.object_id = ">>, '$1',
                             <<" AND edge.predicate_id = ">>, '$2', <<")">>
        ],
        args = [
            m_rsc:rid(Id, Context),
            predicate_to_id(Predicate, Context)
        ]
    };
parse_edges(hasobject, [Id], Context) ->
    #search_sql_term{
        where = [
            <<"EXISTS (SELECT id FROM edge WHERE edge.subject_id = rsc.id AND edge.object_id = ">>, '$1', <<")">>
        ],
        args = [
            m_rsc:rid(Id, Context)
        ]
    }.

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
add_order(<<C, "pivot.", _/binary>> = S, Search) when C =:= $-; C =:= $+ ->
    add_order(binary:replace(S, <<".">>, <<":">>, [global]), Search);
add_order(<<C, "pivot:", Pivot/binary>>, Search)  when C =:= $-; C =:= $+ ->
    case binary:split(Pivot, <<":">>) of
        [ PivotTable, Column ] ->
            Tab1 = sql_safe(<<"pivot_", PivotTable/binary>>),
            Col1 = sql_safe(Column),
            Join = Search#search_sql_term.join_inner,
            Search#search_sql_term{
                join_inner = Join#{
                    Tab1 => {Tab1, <<Tab1/binary, ".id = rsc.id">>}
                },
                sort = Search#search_sql_term.sort
                        ++ [ {Tab1, C, Col1} ]
            };
        [ Column ] ->
            Col1 = <<"pivot_", Column/binary>>,
            Col2 = sql_safe(Col1),
            Search#search_sql_term{
                sort = Search#search_sql_term.sort
                       ++ [ {<<"rsc">>, C, Col2} ]
            }
    end;
add_order(<<C, "facet.", _/binary>> = S, Search) when C =:= $-; C =:= $+ ->
    add_order(binary:replace(S, <<".">>, <<":">>, [global]), Search);
add_order(<<C, "facet:", Column/binary>>, Search)  when C =:= $-; C =:= $+ ->
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
                        in => zotonic_mod_search,
                        name => Name
                    }),
                    % display_error([ ?__("Unknown category", Context), 32, $", z_html:escape(z_convert:to_binary(Name)), $" ], Context),
                    error;
                CatId ->
                    case m_category:id_to_name(CatId, Context) of
                        undefined ->
                            ?LOG_NOTICE(#{
                                text => <<"Query: term is not a category">>,
                                in => zotonic_mod_search,
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

-spec pivot_qterm(Table, Alias, Column, Value, Op, Q, Context) -> {ok, QResult} | {error, term()}
    when Table :: binary(),
         Alias :: binary(),
         Column :: binary(),
         Op :: binary() | undefined,
         Value :: term(),
         Q :: #search_sql_term{},
         QResult :: #search_sql_term{},
         Context :: z:context().
pivot_qterm(_Tab, _Alias, _Col, _Op, [], Q, _Context) ->
    {ok, Q};
pivot_qterm(Tab, Alias, Col, Op, [Value], Q, Context) ->
    pivot_qterm_1(Tab, Alias, Col, Op, Value, Q, Context);
pivot_qterm(Tab, Alias, Col, Op, Vs, Q, Context) when is_list(Vs) ->
    % 'OR' query for all values
    Q2 = lists:foldl(
        fun(V, QAcc) ->
            case pivot_qterm_1(Tab, Alias, Col, Op, V, QAcc, Context) of
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
pivot_qterm(Tab, Alias, Col, Op, Value, Q, Context) ->
    pivot_qterm_1(Tab, Alias, Col, Op, Value, Q, Context).

pivot_qterm_1(Tab, Alias, Col, undefined, Value, Query, Context) ->
    {Op, Value1} = extract_value_op(Value, <<"=">>),
    pivot_qterm_op(Tab, Alias, Col, Op, Value1, Query, Context);
pivot_qterm_1(Tab, Alias, Col, Op, Value, Query, Context) ->
    {Op, Value1} = extract_value_op(Value, Op),
    pivot_qterm_op(Tab, Alias, Col, Op, Value1, Query, Context).

pivot_qterm_op(Tab, Alias, Col, Op, Value, Query, Context) ->
    case z_db:to_column_value(Tab, Col, Value, Context) of
        {ok, Value2} ->
            {ArgN, Query2} = add_term_arg(Value2, Query),
            W = [
                <<Alias/binary, $., Col/binary>>, Op, ArgN
            ],
            Query3 = Query2#search_sql_term{
                where = Query2#search_sql_term.where ++ [ W ]
            },
            {ok, Query3};
        {error, Reason} = Error ->
            ?LOG_WARNING(#{
                text => <<"Pivot value error, dropping query term.">>,
                in => zotonic_mod_search,
                result => error,
                reason => Reason,
                table => Tab,
                alias => Alias,
                column => Col,
                value => Value
            }),
            Error
    end.

add_term_arg(ArgValue, #search_sql_term{ args = Args } = Q) ->
    Arg = [$$] ++ integer_to_list(length(Args) + 1),
    {list_to_atom(Arg), Q#search_sql_term{args = Args ++ [ ArgValue ]}}.

% extract_value_op(#{
%         <<"operator">> := Op,
%         <<"value">> := V
%     }) ->
%     {sanitize_op(z_convert:to_binary(Op)), V};
% extract_value_op(#{
%         <<"value">> := V
%     }) ->
%     {<<"=">>, V};
extract_value_op(<<"<>", V/binary>>, _Op) ->
    {<<"<>">>, V};
extract_value_op(<<"<=", V/binary>>, _Op) ->
    {<<"<=">>, V};
extract_value_op(<<">=", V/binary>>, _Op) ->
    {<<">=">>, V};
extract_value_op(<<"!=", V/binary>>, _Op) ->
    {<<"<>">>, V};
extract_value_op(<<"=", V/binary>>, _Op) ->
    {<<"=">>, V};
extract_value_op(<<">", V/binary>>, _Op) ->
    {<<">">>, V};
extract_value_op(<<"<", V/binary>>, _Op) ->
    {<<"<">>, V};
extract_value_op(V, Op) ->
    {Op, V}.

extract_term_op(#{ <<"operator">> := Op }, _Op) ->
    sanitize_op(z_convert:to_binary(Op));
extract_term_op(_, Op) ->
    Op.

sanitize_op(<<"!=">>) -> <<"<>">>;
sanitize_op(<<"<>">>) -> <<"<>">>;
sanitize_op(<<">=">>) -> <<">=">>;
sanitize_op(<<"<=">>) -> <<"<=">>;
sanitize_op(<<"=">>) -> <<"=">>;
sanitize_op(<<">">>) -> <<">">>;
sanitize_op(<<"<">>) -> <<"<">>;
sanitize_op(_) -> <<"=">>.


add_filters(Filters, Context) ->
    add_filters(Filters, #search_sql_term{}, Context).

%% Add filters
% add_filters(<<"[", _/binary>> = Filter, Q, Context) ->
%     add_filters(maybe_split_list(Filter), Q, Context);
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
    {Tab, Alias, Col, Q1} = map_filter_column(Column, Q),
    case z_db:to_column_value(Tab, Col, Value, Context) of
        {ok, V1} ->
            {Expr, Q2} = create_filter(Tab, Alias, Col, Operator, V1, Q1),
            add_filter_where(Expr, Q2);
        {error, Reason} ->
            ?LOG_INFO(#{
                text => <<"Search query filter could not be added">>,
                result => error,
                reason => Reason,
                filter_column => Column,
                table => Tab,
                column => Col,
                value => Value
            }),
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
    {Tab, Alias, Col, QAcc1} = map_filter_column(C, QAcc),
    case z_db:to_column_value(Tab, Col, V, Context) of
        {ok, V1} ->
            {E, QAcc2} = create_filter(Tab, Alias, Col, O, V1, QAcc1),
            {[E|Es], QAcc2};
        {error, _} ->
            {Es, QAcc}
    end;
add_filters_or_1([ C, V ], {Es, QAcc}, Context) ->
    add_filters_or_1([ C, eq, V ], {Es, QAcc}, Context).

create_filter(Tab, Alias, Col, Operator, null, Q) ->
    create_filter(Tab, Alias, Col, Operator, undefined, Q);
create_filter(_Tab, Alias, Col, Operator, undefined, Q) ->
    Operator1 = map_filter_operator(Operator),
    {create_filter_null(Alias, Col, Operator1), Q};
create_filter(_Tab, Alias, Col, Operator, Value, Q) ->
    {Arg, Q1} = add_filter_arg(Value, Q),
    Operator1 = map_filter_operator(Operator),
    {[Alias, $., Col, <<" ">>, Operator1, <<" ">>, Arg], Q1}.


map_filter_column(<<"pivot.", _/binary>> = P, Q) ->
    map_filter_column(binary:replace(P, <<".">>, <<":">>, [global]), Q);
map_filter_column(<<"pivot:", P/binary>>, #search_sql_term{ join_inner = Join } = Q) ->
    case binary:split(P, <<":">>) of
        [ Table, Field ] ->
            T1 = sql_safe(Table),
            T2 = <<"pivot_", T1/binary>>,
            F1 = sql_safe(Field),
            Q1 = Q#search_sql_term{
                join_inner = Join#{
                    T2 => {T2, <<T2/binary, ".id = rsc.id">>}
                }
            },
            {T2, T2, F1, Q1};
        [ Field ] ->
            F1 = z_convert:to_binary(sql_safe(Field)),
            {<<"rsc">>, <<"rsc">>, <<"pivot_", F1/binary>>, Q}
    end;
map_filter_column(<<"facet.", _/binary>> = P, Q) ->
    map_filter_column(binary:replace(P, <<".">>, <<":">>, [global]), Q);
map_filter_column(<<"facet:", P/binary>>, #search_sql_term{ join_inner = Join } = Q) ->
    Q1 = Q#search_sql_term{
        join_inner = Join#{
            <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
        }
    },
    Field = sql_safe(P),
    {<<"search_facet">>, <<"facet">>, <<"f_", Field/binary>>, Q1};
map_filter_column(Column, Q) ->
    Field = sql_safe(Column),
    {<<"rsc">>, <<"rsc">>, Field, Q}.

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

create_filter_null(Alias, Col, "=") ->
    [ Alias, $., Col, <<" is null">> ];
create_filter_null(Alias, Col, "<>") ->
    [ Alias, $., Col, <<" is not null">> ];
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


%% Expand the argument for hasanyobject, make pairs of {ObjectId,PredicateId}
expand_object_predicates(Bin, Context) when is_binary(Bin) ->
    map_rids(z_parse_list:parse(Bin), Context);
expand_object_predicates(OPs, Context) ->
    map_rids(OPs, Context).

map_rids({rsc_list, L}, Context) ->
    map_rids(L, Context);
map_rids(L, Context) when is_list(L) ->
    [ map_rid(X,Context) || X <- L, X =/= <<>> ];
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

predicate_to_id(Pred, Context) ->
    case m_predicate:name_to_id(Pred, Context) of
        {ok, Id} ->
            Id;
        {error, _} ->
            case m_rsc:rid(Pred, Context) of
                undefined ->
                    ?LOG_NOTICE(#{
                        text => <<"Query: unknown predicate">>,
                        in => zotonic_mod_search,
                        predicate => Pred
                    }),
                    0;
                RId ->
                    RId
            end
    end.

%% Support routine for "hasanyobject"
object_predicate_clause(undefined, undefined) ->
    "false";
object_predicate_clause(_Object, undefined) ->
    "false";
object_predicate_clause(undefined, _Predicate) ->
    "false";
object_predicate_clause(any, any) ->
    ["edge.subject_id = rsc.id"];
object_predicate_clause(any, PredicateId) when is_integer(PredicateId) ->
    ["edge.predicate_id = ", integer_to_list(PredicateId)];
object_predicate_clause(ObjectId, any) when is_integer(ObjectId) ->
    ["edge.object_id = ", integer_to_list(ObjectId)];
object_predicate_clause(ObjectId, PredicateId) when is_integer(PredicateId), is_integer(ObjectId) ->
    ["edge.object_id=", integer_to_list(ObjectId),
     " and ", "edge.predicate_id=", integer_to_list(PredicateId)].

%% Support routine for "hasanysubject"
subject_predicate_clause(undefined, undefined) ->
    "false";
subject_predicate_clause(_Subject, undefined) ->
    "false";
subject_predicate_clause(undefined, _Predicate) ->
    "false";
subject_predicate_clause(any, any) ->
    ["edge.object_id = rsc.id"];
subject_predicate_clause(any, PredicateId) when is_integer(PredicateId) ->
    ["edge.predicate_id = ", integer_to_list(PredicateId)];
subject_predicate_clause(SubjectId, any) when is_integer(SubjectId) ->
    ["edge.subject_id = ", integer_to_list(SubjectId)];
subject_predicate_clause(SubjectId, PredicateId) when is_integer(PredicateId), is_integer(SubjectId) ->
    ["edge.subject_id=", integer_to_list(SubjectId),
     " and ", "edge.predicate_id=", integer_to_list(PredicateId)].
