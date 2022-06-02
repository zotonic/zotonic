%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%% @doc Defines PostgreSQL queries for basic content searches in Zotonic.
%% This module needs to be split in specific PostgreSQL queries and standard SQL queries when you want to
%% support other databases (like MySQL).

%% Copyright 2009-2021 Marc Worrell
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

-module(mod_search).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

-mod_title("Search Queries").

-mod_description("Defines PostgreSQL queries for basic content searches in Zotonic.").

-mod_prio(1000).

%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1]).
%% interface functions
-export([event/2,
         observe_search_query/2,
         observe_module_activate/2,
         observe_custom_pivot/2,
         observe_filewatcher/2,
         observe_module_reindexed/2,
         pid_observe_rsc_delete/3,
         pid_observe_rsc_update_done/3,
         to_tsquery/2,
         rank_weight/1,
         rank_behaviour/1,
         find_by_id/2,
         find_by_id/3,
         trim/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {context, query_watches = []}).

-define(TIMEOUT_GC, 1000).

event(#postback{ message = {facet_rebuild, _Args} }, Context) ->
    case z_acl:is_admin_editable(Context)
         orelse not z_acl:is_read_only(Context) andalso z_acl:is_allowed(use, mod_search, Context)
    of
        true ->
            search_facet:pivot_all(Context),
            z_render:growl(?__("Rebuilding the faceted search table.", Context), Context);
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to do this.", Context), Context)
    end.

observe_search_query(#search_query{
                         name = <<"facets">>,
                         args = Args
                     },
                     Context) ->
    case search_query:search(Args, Context) of
        #search_sql_terms{  } = R ->
            R#search_sql_terms{ post_func = fun search_facet:search_query_facets/3 };
        R ->
            R
    end;
observe_search_query(#search_query{
                         name = <<"subfacets">>,
                         args = Args
                     },
                     Context) ->
    case search_query:search(Args, Context) of
        #search_sql_terms{  } = R ->
            R#search_sql_terms{ post_func = fun search_facet:search_query_subfacets/3 };
        R ->
            R
    end;
observe_search_query(#search_query{ name = <<"facet_values">> }, Context) ->
    case search_facet:facet_values(Context) of
        {ok, Facets} ->
            #search_result{
                result = [],
                total = 0,
                pages = 0,
                facets = Facets
            };
        {error, _} = Error ->
            Error
    end;
observe_search_query(#search_query{
                         name = Name,
                         args = Args,
                         offsetlimit = OffsetLimit
                     },
                     Context) ->
    search(Name, Args, OffsetLimit, Context).

observe_module_activate(#module_activate{
                            module = ?MODULE,
                            pid = Pid
                        },
                        _Context) ->
    gen_server:cast(Pid, init_query_watches);
observe_module_activate(_, _Context) ->
    ok.

observe_custom_pivot(#custom_pivot{ id = Id }, Context) ->
    search_facet:pivot_rsc(Id, Context).

observe_filewatcher(#filewatcher{
                        file = File,
                        extension = <<".tpl">>
                    },
                    Context) ->
    case filename:basename(File) of
        <<"facet.tpl">> ->
            % If the facet.tpl is changed, then check if the facet table is still
            % aligned with the facet.tpl template.
            search_facet:ensure_table(Context);
        _ ->
            ok
    end;
observe_filewatcher(#filewatcher{  }, _Context) ->
    ok.

%% @doc Check the search facet table when all modules are running and the indexer reindexed
%% the templates.
observe_module_reindexed(module_reindexed, Context) ->
    z_context:logger_md(Context),
    Status = z_module_manager:get_modules_status(Context),
    NotRunning = [M || {M, S} <- Status, S =/= running],
    case NotRunning of
        [] ->
            ?LOG_DEBUG("Checking search facet table."),
            search_facet:ensure_table(Context);
        _ ->
            ?LOG_INFO("Delaying search facet check because not all modules are running.")
    end.

%% @doc Check the query watches for a match with the updated resource.
pid_observe_rsc_delete(Pid,
                       #rsc_delete{
                           id = Id,
                           is_a = IsA
                       },
                       _Context) ->
    case lists:member(query, IsA) of
        true ->
            gen_server:cast(Pid, {watches_remove, Id});
        false ->
            ok
    end.

%% @doc Check if any watches need updating.
pid_observe_rsc_update_done(_Pid, #rsc_update_done{ action = delete }, _Context) ->
    ok;
pid_observe_rsc_update_done(Pid,
                            #rsc_update_done{
                                id = Id,
                                pre_is_a = IsA,
                                post_is_a = IsA
                            },
                            Context) ->
    case lists:member(query, IsA) of
        true ->
            gen_server:cast(Pid, {watches_update, Id});
        false ->
            ok
    end,
    rsc_update_notify(Pid, Id, Context);
pid_observe_rsc_update_done(Pid,
                            #rsc_update_done{
                                id = Id,
                                pre_is_a = CatsOld,
                                post_is_a = CatsNew
                            },
                            Context) ->
    case lists:member(query, CatsOld) of
        true ->
            case lists:member(query, CatsNew) of
                true ->
                    %% It still is a query; but might have changes; update watches.
                    gen_server:cast(Pid, {watches_update, Id});
                false ->
                    %% Its no longer a query; remove from watches.
                    gen_server:cast(Pid, {watches_remove, Id})
            end;
        false ->
            case lists:member(query, CatsNew) of
                true ->
                    %% It has become a query
                    gen_server:cast(Pid, {watches_update, Id});
                false ->
                    %% It has not been a query
                    ok
            end
    end,
    rsc_update_notify(Pid, Id, Context).

rsc_update_notify(Pid, Id, Context) ->
    {ok, Watches} = gen_server:call(Pid, get_watches),
    ContextSudo = z_acl:sudo(Context),
    search_query_notify:send_notifications(Id, search_query_notify:check_rsc(Id, Watches, ContextSudo), ContextSudo).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    logger:set_process_metadata(#{
                                    site => z_context:site(Context),
                                    module => ?MODULE
                                }),
    {ok,
     #state{
         context =
             z_acl:sudo(
                 z_context:new(Context))
     },
     ?TIMEOUT_GC}.

handle_call(get_watches, _From, #state{ query_watches = Watches } = State) ->
    {reply, {ok, Watches}, State, ?TIMEOUT_GC};
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(init_query_watches, State) ->
    Watches = search_query_notify:init(State#state.context),
    {noreply, State#state{ query_watches = Watches }, ?TIMEOUT_GC};
handle_cast({watches_remove, Id},
            #state{
                context = Context,
                query_watches = Watches
            } =
                State) ->
    Watches1 = search_query_notify:watches_remove(Id, Watches, Context),
    {noreply, State#state{ query_watches = Watches1 }, ?TIMEOUT_GC};
handle_cast({watches_update, Id},
            #state{
                context = Context,
                query_watches = Watches
            } =
                State) ->
    Watches1 = search_query_notify:watches_update(Id, Watches, Context),
    {noreply, State#state{ query_watches = Watches1 }, ?TIMEOUT_GC};
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @doc Handling all non call/cast messages
handle_info(timeout, State) ->
    z_depcache:flush_process_dict(),
    erlang:garbage_collect(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% support functions
%%====================================================================

search_prevnext(Type, Args, Context) ->
    Order =
        fun (<<"next">>) ->
                "ASC";
            (<<"previous">>) ->
                "DESC"
        end,
    Operator =
        fun (<<"next">>) ->
                " > ";
            (<<"previous">>) ->
                " < "
        end,
    MapField =
        fun (<<"date_start">>) ->
                "pivot_date_start";
            (<<"date_end">>) ->
                "pivot_date_end";
            (<<"title">>) ->
                "pivot_title";
            (X) ->
                z_convert:to_list(
                    z_string:to_name(X))
        end,
    Field =
        z_convert:to_binary(
            maps:get(<<"sort">>, Args, <<"publication_start">>)),
    Limit =
        z_convert:to_integer(
            maps:get(<<"limit">>, Args, 1)),
    Id = maps:get(<<"id">>, Args),
    Cat = maps:get(<<"cat">>, Args),
    FieldValue = m_rsc:p(Id, z_convert:to_binary(Field), Context),
    #search_sql{
        select = "r.id",
        from = "rsc r",
        where = "(" ++ MapField(Field) ++ " " ++ Operator(Type) ++ " $1) and r.id <> $2",
        tables = [{rsc, "r"}],
        cats = [{"r", Cat}],
        args = [FieldValue, z_convert:to_integer(Id), Limit],
        order = MapField(Field) ++ " " ++ Order(Type) ++ ", id " ++ Order(Type),
        limit = "limit $3"
    }.

%% Retrieve the previous/next id(s) (on sort field, defaults to publication date)
search(<<"previous">>, Args, _OffsetLimit, Context) ->
    search_prevnext(<<"previous">>, Args, Context);
search(<<"next">>, Args, _OffsetLimit, Context) ->
    search_prevnext(<<"next">>, Args, Context);
search(<<"keyword_cloud">>, Args, _OffsetLimit, Context) ->
    Cat = maps:get(<<"cat">>, Args, undefined),
    KeywordCatName = maps:get(<<"keywordcat">>, Args, <<"keyword">>),
    KeywordCat = z_convert:to_binary(KeywordCatName),
    KeywordPredName = maps:get(<<"keywordpred">>, Args, <<"subject">>),
    {ok, Subject} = m_predicate:name_to_id(KeywordPredName, Context),
    #search_sql{
        select = "kw.id as id, count(*) as count",
        from = "rsc kw, edge e, rsc r",
        where = "kw.id = e.object_id AND e.predicate_id = $1 AND e.subject_id = r.id",
        tables = [{rsc, "kw"}, {edge, "e"}, {rsc, "r"}],
        cats = [{"kw", KeywordCat}, {"r", Cat}],
        args = [Subject],
        group_by = "kw.id, kw.pivot_title",
        order = "kw.pivot_title"
    };
search(<<"archive_year">>, #{ <<"cat">> := Cat }, OffsetLimit, Context) ->
    Q = #search_sql{
            select = "date_part('year', r.publication_start)::int as year, count(*) as count",
            from = "rsc r",
            tables = [{rsc, "r"}],
            assoc = true,
            cats = [{"r", Cat}],
            group_by = "date_part('year', r.publication_start)",
            order = "year desc"
        },
    R = z_search:search_result(Q, OffsetLimit, Context),
    Result =
        [[{as_date, {{z_convert:to_integer(Y), 1, 1}, {0, 0, 0}}} | Rest]
         || Rest = [{year, Y}, {count, _}] <- R#search_result.result],
    #search_result{ result = Result };
search(<<"archive_year_month">>, #{ <<"cat">> := Cat }, OffsetLimit, Context) ->
    Q = #search_sql{
            select =
                "date_part('year', r.publication_start)::int as year, date_part('month', r.publication_start)::int as month, count(*) as count",
            from = "rsc r",
            tables = [{rsc, "r"}],
            assoc = true,
            cats = [{"r", Cat}],
            group_by = "date_part('year', r.publication_start), date_part('month', r.publication_start)",
            order = "year desc, month desc"
        },
    R = z_search:search_result(Q, OffsetLimit, Context),
    Result =
        [[{month_as_date, {{z_convert:to_integer(Y), z_convert:to_integer(M), 1}, {0, 0, 0}}} | Rest]
         || Rest = [{year, Y}, {month, M}, {count, _}] <- R#search_result.result],
    #search_result{ result = z_utils:group_proplists(year, Result) };
%% @doc Return the rsc records that have similar objects
search(<<"match_objects">>,
       #{
           <<"content_group">> := CG,
           <<"id_exclude">> := Exclude,
           <<"ids">> := ObjectIds
       },
       _OffsetLimit,
       Context) ->
    CGId = m_rsc:rid(CG, Context),
    ExcludeId = m_rsc:rid(Exclude, Context),
    ObjectIds1 = [m_rsc:rid(OId, Context) || OId <- ObjectIds],
    MatchTerms = [["zpo", integer_to_list(ObjId)] || ObjId <- ObjectIds1, ObjId =/= undefined],
    TsQuery =
        lists:flatten(
            lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            #search_result{  };
        _ when is_integer(ExcludeId) ->
            #search_sql{
                select = "r.id, ts_rank(pivot_rtsv, query) AS rank",
                from = "rsc r, to_tsquery($1) query",
                where = " r.content_group_id = $3 and query @@ pivot_rtsv and id <> $2",
                order = "rank desc, r.publication_start desc",
                args = [TsQuery, z_convert:to_integer(ExcludeId), CGId],
                tables = [{rsc, "r"}]
            };
        _ ->
            #search_sql{
                select = "r.id, ts_rank(pivot_rtsv, query) AS rank",
                from = "rsc r, to_tsquery($1) query",
                where = " r.content_group_id = $2 and query @@ pivot_rtsv",
                order = "rank desc, r.publication_start desc",
                args = [TsQuery, CGId],
                tables = [{rsc, "r"}]
            }
    end;
search(<<"match_objects">>,
       #{
           <<"id_exclude">> := Exclude,
           <<"ids">> := ObjectIds
       },
       _OffsetLimit,
       Context) ->
    ExcludeId = m_rsc:rid(Exclude, Context),
    ObjectIds1 = [m_rsc:rid(OId, Context) || OId <- ObjectIds],
    MatchTerms = [["zpo", integer_to_list(ObjId)] || ObjId <- ObjectIds1, ObjId =/= undefined],
    TsQuery =
        lists:flatten(
            lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            #search_result{  };
        _ when is_integer(ExcludeId) ->
            #search_sql{
                select = "r.id, ts_rank(pivot_rtsv, query) AS rank",
                from = "rsc r, to_tsquery($1) query",
                where = " query @@ pivot_rtsv and id <> $2",
                order = "rank desc, r.publication_start desc",
                args = [TsQuery, z_convert:to_integer(ExcludeId)],
                tables = [{rsc, "r"}]
            };
        _ ->
            #search_sql{
                select = "r.id, ts_rank(pivot_rtsv, query) AS rank",
                from = "rsc r, to_tsquery($1) query",
                where = " query @@ pivot_rtsv",
                order = "rank desc, r.publication_start desc",
                args = [TsQuery],
                tables = [{rsc, "r"}]
            }
    end;
search(<<"match_objects">>,
       #{
           <<"cat">> := Cat,
           <<"content_group">> := CG,
           <<"id">> := Id
       },
       OffsetLimit,
       Context) ->
    ObjectIds = m_edge:objects(Id, Context),
    Args =
        #{
            <<"content_group">> => CG,
            <<"id_exclude">> => Id,
            <<"ids">> => ObjectIds
        },
    case search(<<"match_objects">>, Args, OffsetLimit, Context) of
        #search_sql{  } = Search ->
            Search#search_sql{ cats = [{"r", Cat}] };
        Result ->
            Result
    end;
search(<<"match_objects">>,
       #{
           <<"cat">> := Cat,
           <<"id">> := Id
       },
       OffsetLimit,
       Context) ->
    case search(<<"match_objects">>, #{ <<"id">> => Id }, OffsetLimit, Context) of
        #search_sql{  } = Search ->
            Search#search_sql{ cats = [{"r", Cat}] };
        Result ->
            Result
    end;
search(<<"match_objects">>, #{ <<"id">> := Id }, OffsetLimit, Context) ->
    ObjectIds = m_edge:objects(Id, Context),
    search(<<"match_objects">>,
           #{
               <<"id_exclude">> => Id,
               <<"ids">> => ObjectIds
           },
           OffsetLimit,
           Context);
search(<<"match_objects">>, #{ <<"ids">> := ObjectIds }, OffsetLimit, Context) ->
    search(<<"match_objects">>,
           #{
               <<"id_exclude">> => undefined,
               <<"ids">> => ObjectIds
           },
           OffsetLimit,
           Context);
%% @doc Return the rsc records that have similar objects
search(<<"match_objects_cats">>,
       #{
           <<"cat">> := Cat,
           <<"id">> := Id
       },
       OffsetLimit,
       Context) ->
    Args = #{ <<"id">> => Id },
    case search(<<"match_objects_cats">>, Args, OffsetLimit, Context) of
        #search_sql{  } = Search ->
            Search#search_sql{ cats = [{"r", Cat}] };
        Result ->
            Result
    end;
search(<<"match_objects_cats">>, #{ <<"id">> := Id }, _OffsetLimit, Context) ->
    IsCats = m_rsc:is_a_id(Id, Context),
    CatTerms = [["zpc", integer_to_list(CatId)] || CatId <- IsCats],
    ObjectIds = m_edge:objects(Id, Context),
    ObjectTerms = [["zpo", integer_to_list(ObjId)] || ObjId <- ObjectIds],
    TsQuery =
        lists:flatten(
            lists:join("|", CatTerms ++ ObjectTerms)),
    case TsQuery of
        [] ->
            #search_result{  };
        _ ->
            #search_sql{
                select = "r.id, ts_rank(pivot_rtsv, query) AS rank",
                from = "rsc r, to_tsquery($1) query",
                where = " query @@ pivot_rtsv and id <> $2",
                order = "rank desc, r.publication_start desc",
                args = [TsQuery, z_convert:to_integer(Id)],
                tables = [{rsc, "r"}]
            }
    end;
%% @doc Return a list of featured resource ids inside a category having a object_id as predicate
search(<<"featured">>,
       #{
           <<"cat">> := Cat,
           <<"object">> := ObjectId,
           <<"predicate">> := Predicate
       },
       OffsetLimit,
       Context) ->
    Args =
        #{
            <<"cat">> => Cat,
            <<"hassubject">> => [ObjectId, Predicate]
        },
    search(<<"query">>, Args, OffsetLimit, Context);
%% @doc Return a list of resource ids inside a category, featured ones first
search(<<"featured">>, Args, OffsetLimit, Context) ->
    Args1 = Args#{ <<"sort">> => [<<"-rsc.is_featured">>, <<"-rsc.publication_start">>] },
    search(<<"query">>, Args1, OffsetLimit, Context);
%% @doc Return the list of resource ids, on descending id
search(<<"all">>, #{  } = Args, OffsetLimit, Context) ->
    Args1 = Args#{ <<"sort">> => [<<"-rsc.id">>] },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"published">>, Args, OffsetLimit, Context) ->
    Args1 = Args#{ <<"sort">> => [<<"-rsc.publication_start">>] },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"latest">>, Args, OffsetLimit, Context) ->
    Args1 = Args#{ <<"sort">> => [<<"-rsc.modified">>] },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"upcoming">>, Args, OffsetLimit, Context) ->
    Args1 =
        Args#{
                <<"upcoming">> => true,
                <<"sort">> => [<<"rsc.pivot_date_start">>]
            },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"finished">>, Args, OffsetLimit, Context) ->
    Args1 =
        Args#{
                <<"finished">> => true,
                <<"sort">> => [<<"-rsc.pivot_date_start">>]
            },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"autocomplete">>, #{ <<"text">> := QueryText } = Args, _OffsetLimit, Context) ->
    case trim(QueryText, Context) of
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            case TsQuery of
                A when A == <<>> ->
                    #search_result{  };
                _ ->
                    Cat = maps:get(<<"cat">>, Args, []),
                    #search_sql{
                        select = "r.id, ts_rank_cd(" ++ rank_weight(Context) ++ ", pivot_tsv, $1, $2) AS rank",
                        from = "rsc r",
                        where = " $1 @@ r.pivot_tsv",
                        order = "rank desc",
                        args = [TsQuery, rank_behaviour(Context)],
                        cats = [{"r", Cat}],
                        tables = [{rsc, "r"}]
                    }
            end
    end;
search(<<"fulltext">>, #{ <<"cat">> := Cat } = Args, OffsetLimit, Context)
    when Cat =:= undefined orelse Cat =:= [] orelse Cat =:= <<>> ->
    search(<<"fulltext">>, maps:remove(<<"cat">>, Args), OffsetLimit, Context);
search(<<"fulltext">>,
       #{
           <<"cat">> := Cat,
           <<"text">> := QueryText
       },
       _OffsetLimit,
       Context) ->
    case trim(QueryText, Context) of
        <<>> ->
            #search_sql{
                select = "r.id, 1 AS rank",
                from = "rsc r",
                order = "r.modified desc",
                cats = [{"r", Cat}],
                tables = [{rsc, "r"}]
            };
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            #search_sql{
                select = "r.id, ts_rank_cd(" ++ rank_weight(Context) ++ ", pivot_tsv, $1, $2) AS rank",
                from = "rsc r",
                where = " $1 @@ pivot_tsv",
                order = "rank desc",
                args = [TsQuery, rank_behaviour(Context)],
                cats = [{"r", Cat}],
                tables = [{rsc, "r"}]
            }
    end;
search(<<"fulltext">>, #{ <<"text">> := QueryText }, _OffsetLimit, Context) ->
    case trim(QueryText, Context) of
        <<>> ->
            #search_sql{
                select = "r.id, 1 AS rank",
                from = "rsc r",
                order = "r.modified desc",
                args = [],
                tables = [{rsc, "r"}]
            };
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            #search_sql{
                select = "r.id, ts_rank_cd(" ++ rank_weight(Context) ++ ", pivot_tsv, $1, $2) AS rank",
                from = "rsc r",
                where = " $1 @@ r.pivot_tsv",
                order = "rank desc",
                args = [TsQuery, rank_behaviour(Context)],
                tables = [{rsc, "r"}]
            }
    end;
search(<<"referrers">>, #{ <<"id">> := Id }, _OffsetLimit, _Context) ->
    #search_sql{
        select = "o.id, e.predicate_id",
        from = "edge e join rsc o on o.id = e.subject_id",
        where = "e.object_id = $1",
        order = "e.id desc",
        args = [z_convert:to_integer(Id)],
        tables = [{rsc, "o"}]
    };
search(<<"media_category_image">>, #{ <<"cat">> := Cat }, _OffsetLimit, _Context) ->
    #search_sql{
        select = "m.filename",
        from = "rsc r, medium m",
        where = "m.id = r.id",
        cats = [{"r", Cat}],
        tables = [{rsc, "r"}, {medium, "m"}]
    };
search(<<"media_category_depiction">>, #{ <<"cat">> := Cat }, _OffsetLimit, Context) ->
    {ok, PredDepictionId} = m_predicate:name_to_id(depiction, Context),
    #search_sql{
        select = "m.filename",
        from = "rsc r, rsc ro, medium m, edge e",
        where = "ro.id = e.object_id and e.subject_id = r.id and e.predicate_id = $1 and ro.id = m.id",
        tables = [{rsc, "r"}, {rsc, "ro"}, {medium, "m"}],
        args = [PredDepictionId],
        cats = [{"r", Cat}]
    };
search(<<"media">>, Args, OffsetLimit, Context) ->
    Args1 =
        Args#{
                <<"hasmedium">> => true,
                <<"sort">> => [<<"-m.created">>]
            },
    search(<<"query">>, Args1, OffsetLimit, Context);
search(<<"all_bytitle">>, #{ <<"cat">> := Cat }, _OffsetLimit, Context) ->
    search_all_bytitle:search(Cat, all_bytitle, Context);
search(<<"all_bytitle_featured">>, #{ <<"cat">> := Cat }, _OffsetLimit, Context) ->
    search_all_bytitle:search(Cat, all_bytitle_featured, Context);
search(<<"all_bytitle">>, #{ <<"cat_is">> := Cat }, _OffsetLimit, Context) ->
    search_all_bytitle:search_cat_is(Cat, all_bytitle, Context);
search(<<"all_bytitle_featured">>, #{ <<"cat_is">> := Cat }, _OffsetLimit, Context) ->
    search_all_bytitle:search_cat_is(Cat, all_bytitle_featured, Context);
search(<<"query">>, Args, _OffsetLimit, Context) ->
    search_query:search(Args, Context);
search(<<"events">>,
       #{
           <<"end">> := End,
           <<"start">> := Start
       } =
           Args,
       _OffsetLimit,
       _Context) ->
    Cat = maps:get(<<"cat">>, Args, event),
    #search_sql{
        select = "r.id, r.pivot_date_start, r.pivot_date_end",
        from = "rsc r",
        where = "r.pivot_date_end >= $1 AND r.pivot_date_start <= $2",
        args = [Start, End],
        order = "r.pivot_date_start asc",
        cats = [{"r", Cat}],
        tables = [{rsc, "r"}]
    };
search(_, _, _, _) ->
    undefined.

-spec trim(binary() | string() | z:trans() | undefined, z:context()) -> binary().
trim(undefined, _Context) ->
    <<>>;
trim(S, _Context) when is_binary(S) ->
    z_string:trim(S);
trim(#trans{  } = Tr, Context) ->
    trim(z_trans:lookup_fallback(Tr, Context), Context);
trim(S, Context) ->
    trim(z_convert:to_binary(S), Context).

%% @doc Expand a search string like "hello wor" to a PostgreSQL tsquery string.
%%      If the search string ends in a word character then a wildcard is appended
%%      to the last search term.
-spec to_tsquery(binary() | string() | z:trans() | undefined, z:context()) -> binary().
to_tsquery(undefined, _Context) ->
    <<>>;
to_tsquery(<<>>, _Context) ->
    <<>>;
to_tsquery(#trans{  } = Tr, Context) ->
    to_tsquery(z_trans:lookup_fallback(Tr, Context), Context);
to_tsquery(Text, Context) when is_binary(Text) ->
    case to_tsquery_1(Text, Context) of
        <<>> ->
            % Check if the wildcard prefix was a stopword like the dutch "de"
            case is_separator(binary:last(Text)) of
                true ->
                    <<>>;
                false ->
                    Text1 = <<(z_convert:to_binary(Text))/binary, "xcvvcx">>,
                    TsQuery = to_tsquery_1(Text1, Context),
                    case binary:replace(TsQuery, <<"xcvvcx">>, <<>>) of
                        <<"''">> ->
                            <<>>;
                        RTsQuery ->
                            RTsQuery
                    end
            end;
        TsQuery ->
            TsQuery
    end;
to_tsquery(Text, Context) when is_list(Text) ->
    to_tsquery(z_convert:to_binary(Text), Context).

to_tsquery_1(Text, Context) when is_binary(Text) ->
    Stemmer = z_pivot_rsc:stemmer_language(Context),
    [{TsQuery}] = z_db:q("select plainto_tsquery($2, $1)", [z_pivot_rsc:cleanup_tsv_text(Text), Stemmer], Context),
    fixup_tsquery(z_convert:to_list(Stemmer), append_wildcard(Text, TsQuery)).

is_separator(C) when C < $0 ->
    true;
is_separator(C) when C >= $0, C =< $9 ->
    false;
is_separator(C) when C >= $A, C =< $Z ->
    false;
is_separator(C) when C >= $a, C =< $z ->
    false;
is_separator(C) when C >= 128 ->
    false;
is_separator(_) ->
    true.

append_wildcard(_Text, <<>>) ->
    <<>>;
append_wildcard(_Text, <<"'xcvvcx'">>) ->
    <<>>;
append_wildcard(Text, TsQ) ->
    case is_wordchar(z_string:last_char(Text)) of
        true ->
            <<TsQ/binary, ":*">>;
        false ->
            TsQ
    end.

is_wordchar(C) when C >= 0, C =< 9 ->
    true;
is_wordchar(C) when C >= $a, C =< $z ->
    true;
is_wordchar(C) when C >= $A, C =< $Z ->
    true;
is_wordchar(C) when C > 255 ->
    true;
is_wordchar(_) ->
    false.

% There are some problems with the stemming of prefixes.
% For now we fix this up by removing the one case we found.
%
% to_tsquery('dutch', 'overstee') -> 'overstee'
% to_tsquery('dutch', 'oversteek') -> 'overstek'
fixup_tsquery(_Stemmer, <<>>) ->
    <<>>;
fixup_tsquery("dutch", TsQ) ->
    iolist_to_binary(re:replace(TsQ, <<"([a-z]([aieou]))\\2':\\*">>, <<"\\1':\\*">>));
fixup_tsquery(_Stemmer, TsQ) ->
    TsQ.

%% @doc Find one more more resources by id or name, when the resources exists.
%% Input may be a single token or a comma-separated string.
%% Search results contain a list of ids.
-spec find_by_id(string() | binary(), z:context()) -> #search_result{  }.
find_by_id(S, Context) ->
    find_by_id(S, false, Context).

%% @doc As find_by_id/2, but when Rank is true, results contain a list of tuples: {id, 1}.
-spec find_by_id(string() | binary(), boolean(), z:context()) -> #search_result{  }.
find_by_id(S, Rank, Context) when is_binary(S) ->
    find_by_id(z_convert:to_list(S), Rank, Context);
find_by_id(S, Rank, Context) ->
    Searches = lists:map(fun z_string:trim/1, string:tokens(S, ", ")),
    Searches1 =
        lists:filter(fun(Id) ->
                        Id =/= ""
                     end,
                     Searches),
    ExactIds =
        lists:foldl(fun(Id, Acc) ->
                       RId = m_rsc:rid(Id, Context),
                       case m_rsc:exists(RId, Context) of
                           false ->
                               Acc;
                           true ->
                               [RId | Acc]
                       end
                    end,
                    [],
                    Searches1),
    NonExactIds =
        lists:foldl(fun(Id, Acc) ->
                       IdSafe =
                           z_convert:to_list(
                               z_string:to_name(Id)),
                       Matching =
                           z_db:q("
                select id
                from rsc
                where name like '"
                                  ++ IdSafe
                                  ++ "%'
                limit 10",
                                  Context),
                       Matching1 = [M || {M} <- Matching],
                       Matching1 ++ Acc
                    end,
                    [],
                    Searches1),
    Ids1 = lists:usort(ExactIds) ++ lists:usort(NonExactIds -- ExactIds),
    Ids2 =
        case Rank of
            false ->
                Ids1;
            true ->
                lists:map(fun(Id) ->
                             {Id, 1}
                          end,
                          Ids1)
        end,
    #search_result{
        result = Ids2,
        total = length(Ids2)
    }.

%% @doc The ranking behaviour for scoring words in a full text search
%% See also: http://www.postgresql.org/docs/9.3/static/textsearch-controls.html
-spec rank_behaviour(#context{  }) -> integer().
rank_behaviour(Context) ->
    case m_config:get_value(mod_search, rank_behaviour, Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> ->
            1 bor 4 bor 32;
        Rank ->
            z_convert:to_integer(Rank)
    end.

%% @doc The weights for the ranking of the ABCD indexing categories.
%% See also: http://www.postgresql.org/docs/9.3/static/textsearch-controls.html
-spec rank_weight(#context{  }) -> string().
rank_weight(Context) ->
    case m_config:get_value(mod_search, rank_weight, Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> ->
            "'{0.05, 0.25, 0.5, 1.0}'";
        Weight ->
            lists:flatten(
                io_lib:format("\'~s\'", [Weight]))
    end.
