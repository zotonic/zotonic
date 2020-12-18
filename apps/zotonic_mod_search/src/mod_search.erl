%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-09
%% @doc Defines PostgreSQL queries for basic content searches in Zotonic.
%% This module needs to be split in specific PostgreSQL queries and standard SQL queries when you want to
%% support other databases (like MySQL).

%% Copyright 2009 Marc Worrell
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe_search_query/2,
    observe_module_activate/2,
    to_tsquery/2,
    rank_weight/1,
    rank_behaviour/1,
    find_by_id/2,
    find_by_id/3,
    trim/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-record(state, {context, query_watches=[]}).


observe_search_query({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

observe_module_activate(#module_activate{module=?MODULE, pid=Pid}, _Context) ->
    gen_server:cast(Pid, init_query_watches);
observe_module_activate(_, _Context) ->
    ok.


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    lager:md([
        {site, z_context:site(Context)},
        {module, ?MODULE}
      ]),

    %% Watch for changes to resources
    z_notifier:observe(rsc_update_done, self(), Context),
    z_notifier:observe(rsc_delete, self(), Context),
    {ok, #state{context=z_acl:sudo(z_context:new(Context))}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Casts for updates to resources
handle_cast({#rsc_delete{id=Id, is_a=IsA}, _Ctx}, State=#state{context=Context,query_watches=Watches}) ->
    Watches1 = case lists:member('query', IsA) of
                   false -> Watches;
                   true -> search_query_notify:watches_remove(Id, Watches, Context)
               end,
    {noreply, State#state{query_watches=Watches1}};

handle_cast(init_query_watches, State) ->
    Watches = search_query_notify:init(State#state.context),
    {noreply, State#state{query_watches=Watches}};

handle_cast({#rsc_update_done{action=delete}, _Ctx}, State) ->
    {noreply, State};
handle_cast({#rsc_update_done{id=Id, pre_is_a=Cats, post_is_a=Cats}, _Ctx}, State=#state{query_watches=Watches,context=Context}) ->
    %% Update; categories have not changed.
    Watches1 = case lists:member('query', Cats) of
                   false -> Watches;
                   true -> search_query_notify:watches_update(Id, Watches, Context)
               end,
    %% Item updated; send notifications for matched queries.
    search_query_notify:send_notifications(Id, search_query_notify:check_rsc(Id, Watches1, Context), Context),
    {noreply, State#state{query_watches=Watches1}};

handle_cast({#rsc_update_done{id=Id, pre_is_a=CatsOld, post_is_a=CatsNew}, _Ctx}, State=#state{query_watches=Watches,context=Context}) ->
    %% Update; categories *have* changed.
    Watches1 = case lists:member('query', CatsOld) of
                   true ->
                       case lists:member('query', CatsNew) of
                           true ->
                               %% It still is a query; but might have changes; update watches.
                               search_query_notify:watches_update(Id, Watches, Context);
                           false ->
                               %% Its no longer a query; remove from watches.
                               search_query_notify:watches_remove(Id, Watches, Context)
                       end;
                   false ->
                       case lists:member('query', CatsNew) of
                           true ->
                               %% It has become a query
                               search_query_notify:watches_update(Id, Watches, Context);
                           false ->
                               %% It has not been a query
                               Watches
                       end
               end,
    search_query_notify:send_notifications(Id, search_query_notify:check_rsc(Id, Watches1, Context), Context),
    {noreply, State#state{query_watches=Watches1}};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    Context = State#state.context,
    z_notifier:detach(rsc_update_done, self(), Context),
    z_notifier:detach(rsc_delete, self(), Context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

search_prevnext(Type, Args, Context) ->
    Order = fun(next) -> "ASC"; (previous) -> "DESC" end,
    Operator = fun(next) -> " > "; (previous) -> " < " end,
    MapField = fun("date_start") -> "pivot_date_start";
                  ("date_end") -> "pivot_date_end";
                  ("title") -> "pivot_title";
                  (X) -> X end,
    Field = z_convert:to_list(proplists:get_value(sort, Args, publication_start)),
    Limit = z_convert:to_integer(proplists:get_value(limit, Args, 1)),
    {id, Id} = proplists:lookup(id, Args),
    {cat, Cat} = proplists:lookup(cat, Args),
    FieldValue = m_rsc:p(Id, z_convert:to_atom(Field), Context),
    #search_sql{
                 select="r.id",
                 from="rsc r",
                 where="(" ++ MapField(Field) ++ " " ++ Operator(Type) ++ " $1) and r.id <> $2",
                 tables=[{rsc, "r"}],
                 cats=[{"r", Cat}],
                 args=[FieldValue, z_convert:to_integer(Id), Limit],
                 order=MapField(Field) ++ " " ++ Order(Type) ++ ", id " ++ Order(Type),
                 limit="limit $3"
               }.


%% Retrieve the previous/next id(s) (on sort field, defaults to publication date)
search({previous, Args}, _OffsetLimit, Context) ->
    search_prevnext(previous, Args, Context);
search({next, Args}, _OffsetLimit, Context) ->
    search_prevnext(next, Args, Context);

search({keyword_cloud, Props}, _OffsetLimit, Context) ->
    Cat = proplists:get_value(cat, Props),
    KeywordCatName = proplists:get_value(keywordcat, Props, "keyword"),
    KeywordCat = list_to_atom(KeywordCatName),
    KeywordPredName = proplists:get_value(keywordpred, Props, "subject"),
    {ok, Subject} = m_predicate:name_to_id(KeywordPredName, Context),
    #search_sql{
        select="kw.id as id, count(*) as count",
        from="rsc kw, edge e, rsc r",
        where="kw.id = e.object_id AND e.predicate_id = $1 AND e.subject_id = r.id",
        tables=[{rsc, "kw"}, {edge, "e"}, {rsc, "r"}],
        cats=[{"kw", KeywordCat}, {"r", Cat}],
        args=[Subject],
        group_by="kw.id, kw.pivot_title",
        order="kw.pivot_title"
       };

search({archive_year, [{cat,Cat}]}, OffsetLimit, Context) ->
    Q = #search_sql{
      select="date_part('year', r.publication_start)::int as year, count(*) as count",
      from="rsc r",
      tables=[{rsc, "r"}],
      assoc=true,
      cats=[{"r", Cat}],
      group_by="date_part('year', r.publication_start)",
      order="year desc"
     },
    R = z_search:search_result(Q, OffsetLimit, Context),
    Result = [ [{as_date, {{z_convert:to_integer(Y),1,1},{0,0,0}}}|Rest]
               || Rest = [{year, Y}, {count, _}] <- R#search_result.result],
    #search_result{result=Result};

search({archive_year_month, [{cat,Cat}]}, OffsetLimit, Context) ->
    Q = #search_sql{
      select="date_part('year', r.publication_start)::int as year, date_part('month', r.publication_start)::int as month, count(*) as count",
      from="rsc r",
      tables=[{rsc, "r"}],
      assoc=true,
      cats=[{"r", Cat}],
      group_by="date_part('year', r.publication_start), date_part('month', r.publication_start)",
      order="year desc, month desc"
     },
    R = z_search:search_result(Q, OffsetLimit, Context),
    Result = [ [{month_as_date, {{z_convert:to_integer(Y),z_convert:to_integer(M),1},{0,0,0}}}|Rest]
               || Rest = [{year, Y}, {month, M}, {count, _}] <- R#search_result.result],
    #search_result{result=z_utils:group_proplists(year, Result)};


%% @doc Return the rsc records that have similar objects
search({match_objects, [{id, Id}]}, OffsetLimit, Context) ->
    ObjectIds = m_edge:objects(Id, Context),
    search({match_objects, [{id_exclude, Id}, {ids, ObjectIds}]}, OffsetLimit, Context);
search({match_objects, [{ids, ObjectIds}]}, OffsetLimit, Context) ->
    search({match_objects, [{id_exclude, undefined}, {ids, ObjectIds}]}, OffsetLimit, Context);
search({match_objects, [{id_exclude, Exclude}, {ids, ObjectIds}]}, _OffsetLimit, Context) ->
    ExcludeId = m_rsc:rid(Exclude, Context),
    ObjectIds1 = [ m_rsc:rid(OId, Context) || OId <- ObjectIds ],
    MatchTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds1, ObjId =/= undefined ],
    TsQuery = lists:flatten(lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            #search_result{};
        _ when is_integer(ExcludeId) ->
            #search_sql{
                select="r.id, ts_rank(pivot_rtsv, query) AS rank",
                from="rsc r, to_tsquery($1) query",
                where=" query @@ pivot_rtsv and id <> $2",
                order="rank desc, r.publication_start desc",
                args=[TsQuery, z_convert:to_integer(ExcludeId)],
                tables=[{rsc,"r"}]
            };
        _ ->
            #search_sql{
                select="r.id, ts_rank(pivot_rtsv, query) AS rank",
                from="rsc r, to_tsquery($1) query",
                where=" query @@ pivot_rtsv",
                order="rank desc, r.publication_start desc",
                args=[TsQuery],
                tables=[{rsc,"r"}]
            }
    end;
search({match_objects, [{cat,Cat},{id,Id}]}, OffsetLimit, Context) ->
    case search({match_objects, [{id,Id}]}, OffsetLimit, Context) of
        #search_sql{} = Search -> Search#search_sql{cats=[{"r", Cat}]};
        Result -> Result
    end;
search({match_objects, [{content_group, CG}, {id_exclude, Exclude}, {ids, ObjectIds}]}, _OffsetLimit, Context) ->
    CGId = m_rsc:rid(CG, Context),
    ExcludeId = m_rsc:rid(Exclude, Context),
    ObjectIds1 = [ m_rsc:rid(OId, Context) || OId <- ObjectIds ],
    MatchTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds1, ObjId =/= undefined ],
    TsQuery = lists:flatten(lists:join("|", MatchTerms)),
    case TsQuery of
        [] ->
            #search_result{};
        _ when is_integer(ExcludeId) ->
            #search_sql{
                select="r.id, ts_rank(pivot_rtsv, query) AS rank",
                from="rsc r, to_tsquery($1) query",
                where=" r.content_group_id = $3 and query @@ pivot_rtsv and id <> $2",
                order="rank desc, r.publication_start desc",
                args=[TsQuery, z_convert:to_integer(ExcludeId), CGId],
                tables=[{rsc,"r"}]
            };
        _ ->
            #search_sql{
                select="r.id, ts_rank(pivot_rtsv, query) AS rank",
                from="rsc r, to_tsquery($1) query",
                where=" r.content_group_id = $2 and query @@ pivot_rtsv",
                order="rank desc, r.publication_start desc",
                args=[TsQuery, CGId],
                tables=[{rsc,"r"}]
            }
    end;
search({match_objects, [{cat,Cat},{content_group,CG},{id,Id}]}, OffsetLimit, Context) ->
    ObjectIds = m_edge:objects(Id, Context),
    case search({match_objects, [{content_group,CG},{id_exclude, Id}, {ids, ObjectIds}]}, OffsetLimit, Context) of
        #search_sql{} = Search -> Search#search_sql{cats=[{"r", Cat}]};
        Result -> Result
    end;

%% @doc Return the rsc records that have similar objects
search({match_objects_cats, [{id,Id}]}, _OffsetLimit, Context) ->
    IsCats = m_rsc:is_a_id(Id, Context),
    CatTerms = [ ["zpc",integer_to_list(CatId)] || CatId <- IsCats ],
    ObjectIds = m_edge:objects(Id, Context),
    ObjectTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds ],
    TsQuery = lists:flatten(lists:join("|", CatTerms++ObjectTerms)),
    case TsQuery of
        [] ->
            #search_result{};
        _ ->
            #search_sql{
                select="r.id, ts_rank(pivot_rtsv, query) AS rank",
                from="rsc r, to_tsquery($1) query",
                where=" query @@ pivot_rtsv and id <> $2",
                order="rank desc, r.publication_start desc",
                args=[TsQuery, z_convert:to_integer(Id)],
                tables=[{rsc,"r"}]
            }
    end;
search({match_objects_cats, [{cat,Cat},{id,Id}]}, OffsetLimit, Context) ->
    case search({match_objects_cats, [{id,Id}]}, OffsetLimit, Context) of
        #search_sql{} = Search -> Search#search_sql{cats=[{"r", Cat}]};
        Result -> Result
    end;

%% @doc Return a list of resource ids, featured ones first
%% @spec search(SearchSpec, Range, Context) -> #search_sql{}
search({featured, []}, OffsetLimit, Context) ->
   search({'query', [{sort, "-rsc.is_featured"}, {sort, "-rsc.publication_start"}]}, OffsetLimit, Context);

%% @doc Return a list of resource ids inside a category, featured ones first
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{cat, Cat}, {sort, "-rsc.is_featured"}, {sort, "-rsc.publication_start"}]}, OffsetLimit, Context);

%% @doc Return the list of resource ids, on descending id
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({all, []}, OffsetLimit, Context) ->
    search({'query', []}, OffsetLimit, Context);

%% @doc Return the list of resource ids inside a category, on descending id
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({all, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{cat, Cat}]}, OffsetLimit, Context);

%% @doc Return a list of featured resource ids inside a category having a object_id as predicate
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat,Cat},{object,ObjectId},{predicate,Predicate}]}, OffsetLimit, Context) ->
    search({'query', [{cat, Cat}, {hassubject, [ObjectId, Predicate]}]}, OffsetLimit, Context);

search({published, []}, OffsetLimit, Context) ->
    search({'query', [{sort, "-rsc.publication_start"}]}, OffsetLimit, Context);

search({published, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{cat, Cat}, {sort, "-rsc.publication_start"}]}, OffsetLimit, Context);

search({latest, []}, OffsetLimit, Context) ->
    search({'query', [{sort, "-rsc.modified"}]}, OffsetLimit, Context);

search({latest, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{cat, Cat}, {sort, "-rsc.modified"}]}, OffsetLimit, Context);

search({latest, [{creator_id,CreatorId}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="r.id",
        from="rsc r",
        where="r.creator_id = $1",
        order="r.modified desc",
        args=[z_convert:to_integer(CreatorId)],
        tables=[{rsc,"r"}]
    };

search({latest, [{cat, Cat}, {creator_id,CreatorId}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="r.id",
        from="rsc r",
        where="r.creator_id = $1",
        order="r.modified desc",
        args=[z_convert:to_integer(CreatorId)],
        cats=[{"r", Cat}],
        tables=[{rsc,"r"}]
    };

search({upcoming, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{upcoming, true}, {cat, Cat}, {sort, "rsc.pivot_date_start"}]}, OffsetLimit, Context);

search({finished, [{cat, Cat}]}, OffsetLimit, Context) ->
    search({'query', [{finished, true}, {cat, Cat}, {sort, '-rsc.pivot_date_start'}]}, OffsetLimit, Context);

search({autocomplete, [{text,QueryText}]}, OffsetLimit, Context) ->
    search({autocomplete, [{cat,[]}, {text,QueryText}]}, OffsetLimit, Context);
search({autocomplete, [{cat,Cat}, {text,QueryText}]}, _OffsetLimit, Context) ->
    case trim(QueryText, Context) of
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            case TsQuery of
                A when A == <<>> ->
                    #search_result{};
                _ ->
                    #search_sql{
                        select="r.id, ts_rank_cd("++rank_weight(Context)++", pivot_tsv, $1, $2) AS rank",
                        from="rsc r",
                        where=" $1 @@ r.pivot_tsv",
                        order="rank desc",
                        args=[TsQuery, rank_behaviour(Context)],
                        cats=[{"r", Cat}],
                        tables=[{rsc,"r"}]
                    }
            end
    end;

search({fulltext, [{cat,Cat},{text,QueryText}]}, OffsetLimit, Context) when Cat == undefined orelse Cat == [] orelse Cat == <<>> ->
    search({fulltext, [{text,QueryText}]}, OffsetLimit, Context);

search({fulltext, [{text,QueryText}]}, _OffsetLimit, Context) ->
    case trim(QueryText, Context) of
        <<>> ->
            #search_sql{
                select="r.id, 1 AS rank",
                from="rsc r",
                order="r.modified desc",
                args=[],
                tables=[{rsc,"r"}]
            };
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            #search_sql{
                select="r.id, ts_rank_cd("++rank_weight(Context)++", pivot_tsv, $1, $2) AS rank",
                from="rsc r",
                where=" $1 @@ r.pivot_tsv",
                order="rank desc",
                args=[TsQuery, rank_behaviour(Context)],
                tables=[{rsc,"r"}]
            }
    end;

search({fulltext, [{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context) ->
    case trim(QueryText, Context) of
        <<>> ->
            #search_sql{
                select="r.id, 1 AS rank",
                from="rsc r",
                order="r.modified desc",
                cats=[{"r", Cat}],
                tables=[{rsc,"r"}]
            };
        <<"id:", S/binary>> ->
            find_by_id(S, true, Context);
        _ ->
            TsQuery = to_tsquery(QueryText, Context),
            #search_sql{
                select="r.id, ts_rank_cd("++rank_weight(Context)++", pivot_tsv, $1, $2) AS rank",
                from="rsc r",
                where=" $1 @@ pivot_tsv",
                order="rank desc",
                args=[TsQuery, rank_behaviour(Context)],
                cats=[{"r", Cat}],
                tables=[{rsc,"r"}]
            }
    end;

search({referrers, [{id,Id}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="o.id, e.predicate_id",
        from="edge e join rsc o on o.id = e.subject_id",
        where="e.object_id = $1",
        order="e.id desc",
        args=[z_convert:to_integer(Id)],
        tables=[{rsc,"o"}]
    };

search({media_category_image, [{cat,Cat}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="m.filename",
        from="rsc r, medium m",
        where="m.id = r.id",
        cats=[{"r", Cat}],
        tables=[{rsc,"r"}, {medium, "m"}]
    };

search({media_category_depiction, [{cat,Cat}]}, _OffsetLimit, Context) ->
    {ok, PredDepictionId} = m_predicate:name_to_id(depiction, Context),
    #search_sql{
        select="m.filename",
        from="rsc r, rsc ro, medium m, edge e",
        where="ro.id = e.object_id and e.subject_id = r.id and e.predicate_id = $1 and ro.id = m.id",
        tables=[{rsc,"r"}, {rsc, "ro"}, {medium, "m"}],
        args=[PredDepictionId],
        cats=[{"r", Cat}]
    };


search({media, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="m.*",
        from="media m",
        tables=[{medium, "m"}],
        order="m.created desc",
        args=[],
        assoc=true
    };

search({all_bytitle, [{cat, Cat}]}, _OffsetLimit, Context) ->
    search_all_bytitle:search(Cat, all_bytitle, Context);

search({all_bytitle_featured, [{cat, Cat}]}, _OffsetLimit, Context) ->
    search_all_bytitle:search(Cat, all_bytitle_featured, Context);

search({all_bytitle, [{cat_is, Cat}]}, _OffsetLimit, Context) ->
    search_all_bytitle:search_cat_is(Cat, all_bytitle, Context);

search({all_bytitle_featured, [{cat_is, Cat}]}, _OffsetLimit, Context) ->
    search_all_bytitle:search_cat_is(Cat, all_bytitle_featured, Context);

search({'query', Args}, _OffsetLimit, Context) ->
    search_query:search(Args, Context);

search({events, [{cat, Cat}, {'end', End}, {start, Start}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="r.id, r.pivot_date_start, r.pivot_date_end",
        from="rsc r",
        where="r.pivot_date_end >= $1 AND r.pivot_date_start <= $2",
        args =[Start, End],
        order="r.pivot_date_start asc",
        cats=[{"r", Cat}],
        tables=[{rsc,"r"}]
    };

search({events, [{'end', End}, {start, Start}]}, OffsetLimit, Context) ->
    search({events, [{cat, event}, {'end', End}, {start, Start}]}, OffsetLimit, Context);

search(_, _, _) ->
    undefined.

-spec trim( binary() | string() | #trans{} | undefined, z:context() ) -> binary().
trim(undefined, _Context) -> <<>>;
trim(S, _Context) when is_binary(S) -> z_string:trim(S);
trim(#trans{} = Tr, Context) -> trim(z_trans:lookup_fallback(Tr, Context), Context);
trim(S, Context) -> trim(z_convert:to_binary(S), Context).

%% @doc Expand a search string like "hello wor" to a PostgreSQL tsquery string.
%%      If the search string ends in a word character then a wildcard is appended
%%      to the last search term.
-spec to_tsquery(binary()|string()|{trans, list()}|undefined, #context{}) -> binary().
to_tsquery(undefined, _Context) ->
    <<>>;
to_tsquery(<<>>, _Context) ->
    <<>>;
to_tsquery({trans, _} = Tr, Context) ->
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
                    binary:replace(TsQuery, <<"xcvvcx">>, <<>>)
            end;
        TsQuery ->
            TsQuery
    end;
to_tsquery(Text, Context) when is_list(Text) ->
    to_tsquery(z_convert:to_binary(Text), Context).


to_tsquery_1(Text, Context) when is_binary(Text) ->
    Stemmer = z_pivot_rsc:stemmer_language(Context),
    [{TsQuery, Version}] = z_db:q("select plainto_tsquery($2, $1), version()",
                                  [z_pivot_rsc:cleanup_tsv_text(Text), Stemmer],
                                  Context),
    % Version is something like "PostgreSQL 8.3.5 on i386-apple-darwin8.11.1, compiled by ..."
    fixup_tsquery(z_convert:to_list(Stemmer), append_wildcard(Text, TsQuery, Version)).

is_separator(C) when C < $0 -> true;
is_separator(C) when C >= $0, C =< $9 -> false;
is_separator(C) when C >= $A, C =< $Z -> false;
is_separator(C) when C >= $a, C =< $z -> false;
is_separator(C) when C >= 128 -> false;
is_separator(_) -> true.

append_wildcard(_Text, <<>>, _Version) ->
    <<>>;
append_wildcard(_Text, TsQ, Version) when Version < <<"PostgreSQL 8.4">> ->
    TsQ;
append_wildcard(Text, TsQ, _Version) ->
    case is_wordchar(z_string:last_char(Text)) of
        true -> <<TsQ/binary, ":*">>;
        false -> TsQ
    end.

is_wordchar(C) when C >= 0, C =< 9 -> true;
is_wordchar(C) when C >= $a, C =< $z -> true;
is_wordchar(C) when C >= $A, C =< $Z -> true;
is_wordchar(C) when C > 255 -> true;
is_wordchar(_) -> false.

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
-spec find_by_id(string() | binary(), z:context()) -> #search_result{}.
find_by_id(S, Context) ->
    find_by_id(S, false, Context).

%% @doc As find_by_id/2, but when Rank is true, results contain a list of tuples: {id, 1}.
-spec find_by_id(string() | binary(), boolean(), z:context()) -> #search_result{}.
find_by_id(S, Rank, Context) when is_binary(S) ->
  find_by_id(z_convert:to_list(S), Rank, Context);
find_by_id(S, Rank, Context) ->
    Searches = lists:map(fun z_string:trim/1, string:tokens(S, ", ")),
    Searches1 = lists:filter(fun(Id) -> Id =/= "" end, Searches),
    ExactIds = lists:foldl(
      fun(Id, Acc) ->
        RId = m_rsc:rid(Id, Context),
        case m_rsc:exists(RId, Context) of
            false -> Acc;
            true -> [RId|Acc]
        end
    end, [], Searches1),
    NonExactIds = lists:foldl(
        fun(Id, Acc) ->
            IdSafe = z_convert:to_list( z_string:to_name(Id) ),
            Matching = z_db:q("
                select id
                from rsc
                where name like '"++ IdSafe ++"%'
                limit 10", Context),
            Matching1 = [ M || {M} <- Matching ],
            Matching1 ++ Acc
        end, [], Searches1),
    Ids1 = lists:usort(ExactIds) ++ lists:usort(NonExactIds -- ExactIds),
    Ids2 = case Rank of
        false -> Ids1;
        true -> lists:map(fun(Id) -> {Id, 1} end, Ids1)
    end,
    #search_result{
        result = Ids2,
        total = length(Ids2)
    }.

%% @doc The ranking behaviour for scoring words in a full text search
%% See also: http://www.postgresql.org/docs/9.3/static/textsearch-controls.html
-spec rank_behaviour(#context{}) -> integer().
rank_behaviour(Context) ->
    case m_config:get_value(mod_search, rank_behaviour, Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> -> 1 bor 4 bor 32;
        Rank -> z_convert:to_integer(Rank)
    end.

%% @doc The weights for the ranking of the ABCD indexing categories.
%% See also: http://www.postgresql.org/docs/9.3/static/textsearch-controls.html
-spec rank_weight(#context{}) -> string().
rank_weight(Context) ->
    case m_config:get_value(mod_search, rank_weight, Context) of
        Empty when Empty =:= undefined; Empty =:= <<>> ->
            "'{0.05, 0.25, 0.5, 1.0}'";
        Weight ->
            lists:flatten(io_lib:format("\'~s\'", [Weight]))
    end.
