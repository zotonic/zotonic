%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2017 Arjan Scherpenisse
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
         parse_query_text/1
        ]).

%% For testing
-export([
    expand_object_predicates/2,
    parse_query/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SQL_SAFE_REGEXP, "^[0-9a-zA-Z_\.]+$").

search(Query, Context) ->
    Start = #search_sql{select="rsc.id",
                        from="rsc rsc",
                        tables=[{rsc, "rsc"}]},
    Query1 = filter_empty(Query),
    case parse_query(Query1, Context, Start) of
        #search_sql{} = S ->
            case z_utils:is_empty(S#search_sql.order) of
                true -> add_order("-rsc.id", S);
                false -> S
            end;
        [] -> #search_result{};
        Other -> Other
    end.

qargs(Context) ->
    Args = z_context:get_q_all_noz(Context),
    lists:filtermap(
                fun
                    ({<<"qargs">>, _}) -> false;
                    ({<<"qs">>, V}) -> {true, {"text", V}};
                    ({<<"q", Term/binary>>, V}) -> {true, {Term, V}};
                    (_) -> false
                end,
                Args).

parse_request_args(Args) ->
    parse_request_args(Args, []).

parse_request_args([], Acc) ->
    Acc;
parse_request_args([{K,V}|Rest], Acc) when is_binary(K) ->
    case z_context:is_zotonic_arg(K) of
        true -> parse_request_args(Rest, Acc);
        false ->
            case request_arg(K) of
                undefined -> parse_request_args(Rest, Acc);
                Arg -> parse_request_args(Rest, [{Arg,z_convert:to_binary(V)}|Acc])
            end
    end;
parse_request_args([{K,V}|Rest], Acc) ->
    parse_request_args([{z_convert:to_binary(K), V}|Rest], Acc).

%% Parses a query text. Every line is an argument; of which the first
%% '=' separates argument key from argument value.
%% @doc parse_query_text(string()) -> [{K, V}]
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
request_arg(Arg) when is_list(Arg) -> request_arg(list_to_binary(Arg));
request_arg(<<"content_group">>)       -> content_group;
request_arg(<<"cat">>)                 -> cat;
request_arg(<<"cat_exact">>)           -> cat_exact;
request_arg(<<"cat_exclude">>)         -> cat_exclude;
request_arg(<<"creator_id">>)          -> creator_id;
request_arg(<<"modifier_id">>)         -> modifier_id;
request_arg(<<"custompivot">>)         -> custompivot;
request_arg(<<"filter">>)              -> filter;
request_arg(<<"id_exclude">>)          -> id_exclude;
request_arg(<<"hasobject">>)           -> hasobject;
request_arg(<<"hasobjectpredicate">>)  -> hasobjectpredicate;
request_arg(<<"hassubject">>)          -> hassubject;
request_arg(<<"hassubjectpredicate">>) -> hassubjectpredicate;
request_arg(<<"hasanyobject">>)        -> hasanyobject;
request_arg(<<"is_authoritative">>)    -> is_authoritative;
request_arg(<<"is_featured">>)         -> is_featured;
request_arg(<<"is_published">>)        -> is_published;
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
request_arg(Term) ->
    lager:error("Unknown query term: ~p", [Term]),
    undefined.


%% Private methods start here
filter_empty(Q) ->
    lists:filter(fun({_, X}) -> not(empty_term(X)) end, Q).

empty_term([]) -> true;
empty_term(<<>>) -> true;
empty_term(undefined) -> true;
empty_term([X, _]) -> empty_term(X);
empty_term(_) -> false.

parse_query([], _Context, Result) ->
    Result;

%% cat=categoryname
%% Filter results on a certain category.
parse_query([{cat, Cats}|Rest], Context, Result) ->
    Cats1 = assure_categories(Cats, Context),
    Cats2 = add_or_append("rsc", Cats1, Result#search_sql.cats),
    parse_query(Rest, Context, Result#search_sql{cats=Cats2});

%% cat_exclude=categoryname
%% Filter results outside a certain category.
parse_query([{cat_exclude, Cats}|Rest], Context, Result) ->
    Cats1 = assure_categories(Cats, Context),
    Cats2 = add_or_append("rsc", Cats1, Result#search_sql.cats_exclude),
    parse_query(Rest, Context, Result#search_sql{cats_exclude=Cats2});

%% cat_exact=categoryname
%% Filter results excactly of a category (excluding subcategories)
parse_query([{cat_exact, Cats}|Rest], Context, Result) ->
    Cats1 = assure_categories(Cats, Context),
    Cats2 = add_or_append("rsc", Cats1, Result#search_sql.cats_exact),
    parse_query(Rest, Context, Result#search_sql{cats_exact=Cats2});

parse_query([{filter, R}|Rest], Context, Result) ->
    Result1 = add_filters(R, Result),
    parse_query(Rest, Context, Result1);

%% content_group=id
%% Include only resources which are member of the given content group (or one of its children)
parse_query([{content_group, ContentGroup}|Rest], Context, Result0) ->
    Result = Result0#search_sql{extra=[no_content_group_check | Result0#search_sql.extra ]},
    Result2 = case rid(ContentGroup, Context) of
                    any ->
                        Result;
                    undefined ->
                        % Force an empty result
                        add_where("rsc.content_group_id = 0", Result);
                    ContentGroupId ->
                        % TODO: allow NULL for the default content group
                        case m_rsc:is_a(ContentGroupId, content_group, Context) of
                            true ->
                                List = m_hierarchy:contains(<<"content_group">>, ContentGroup, Context),
                                {Arg, Result1} = add_arg(List, Result),
                                add_where("rsc.content_group_id IN (SELECT(unnest("++Arg++"::int[])))", Result1);
                            false ->
                                {Arg, Result1} = add_arg(ContentGroupId, Result),
                                add_where("rsc.content_group_id = "++Arg, Result1)
                        end
              end,
    parse_query(Rest, Context, Result2);

%% id_exclude=resource-id
%% Exclude an id from the result
parse_query([{id_exclude, Id}|Rest], Context, Result)  when is_integer(Id) ->
    Result1 = add_where("rsc.id <> " ++ integer_to_list(Id), Result),
    parse_query(Rest, Context, Result1);

parse_query([{id_exclude, _Id}|Rest], Context, Result)  ->
    parse_query(Rest, Context, Result);


%% hassubject=[id]
%% Give all things which have an incoming edge to Id
parse_query([{hassubject, Id}|Rest], Context, Result) when is_integer(Id); is_binary(Id) ->
    parse_query([{hassubject, maybe_split_list(Id)}|Rest], Context, Result);
parse_query([{hassubject, [$[|_] = Arg}|Rest], Context, Result) ->
    parse_query([{hassubject, maybe_split_list(Arg)}|Rest], Context, Result);
parse_query([{hassubject, [Id]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),
    {Arg, Result2} = add_arg(m_rsc:rid(Id, Context), Result1),
    Result3 = add_where(A ++ ".subject_id = " ++ Arg, Result2),
    parse_query(Rest, Context, Result3);

%% hassubject=[id,predicate,[alias]]
%% Give all things which have an incoming edge to Id with the given predicate
parse_query([{hassubject, [Id, Predicate]}|Rest], Context, Result) ->
    parse_query([{hassubject, [Id, Predicate, "rsc"]}|Rest], Context, Result);
parse_query([{hassubject, [Id, Predicate, Alias]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join(Alias, "object_id", Result),
    Result2 = case Id of
                  undefined -> Result1;
                  _ -> {Arg1, R} = add_arg(m_rsc:rid(Id,Context), Result1),
                       add_where(A ++ ".subject_id = " ++ Arg1, R)
              end,
    PredicateId = predicate_to_id(Predicate, Context),
    {Arg2, Result3} = add_arg(PredicateId, Result2),
    Result4 = add_where(A ++ ".predicate_id = " ++ Arg2, Result3),
    parse_query(Rest, Context, Result4);
parse_query([{hassubject, Id}|Rest], Context, Result) when is_list(Id) ->
    parse_query([{hassubject, [m_rsc:rid(Id,Context)]}|Rest], Context, Result);


%% hasobject=[id]
%% Give all things which have an outgoing edge to Id
parse_query([{hasobject, Id}|Rest], Context, Result) when is_integer(Id); is_binary(Id) ->
    parse_query([{hasobject, maybe_split_list(Id)}|Rest], Context, Result);
parse_query([{hasobject, [$[|_] = Arg}|Rest], Context, Result) ->
    parse_query([{hasobject, maybe_split_list(Arg)}|Rest], Context, Result);
parse_query([{hasobject, [Id]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("subject_id", Result),
    {Arg, Result2} = add_arg(m_rsc:rid(Id,Context), Result1),
    Result3 = add_where(A ++ ".object_id = " ++ Arg, Result2),
    parse_query(Rest, Context, Result3);

%% hasobject=[id,predicate,[alias]]
%% Give all things which have an outgoing edge to Id with the given predicate
parse_query([{hasobject, [Id, Predicate]}|Rest], Context, Result) ->
    parse_query([{hasobject, [Id, Predicate, "rsc"]}|Rest], Context, Result);
parse_query([{hasobject, [Id, Predicate, Alias]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join(Alias, "subject_id", Result),
    Result2 = case Id of
                  undefined -> Result1;
                  _ -> {Arg1, R} = add_arg(m_rsc:rid(Id,Context), Result1),
                       add_where(A ++ ".object_id = " ++ Arg1, R)
              end,
    PredicateId = predicate_to_id(Predicate, Context),
    {Arg2, Result3} = add_arg(PredicateId, Result2),
    Result4 = add_where(A ++ ".predicate_id = " ++ Arg2, Result3),
    parse_query(Rest, Context, Result4);
parse_query([{hasobject, Id}|Rest], Context, Result) when is_list(Id) ->
    parse_query([{hasobject, [m_rsc:rid(Id,Context)]}|Rest], Context, Result);

%% hasanyobject=[[id,predicate]|id, ...]
%% Give all things which have an outgoing edge to Id with any of the given object/predicate combinations
parse_query([{hasanyobject, ObjPreds}|Rest], Context, Result) ->
    OPs = expand_object_predicates(ObjPreds, Context),
    % rsc.id in (select subject_id from edge where (object_id = ... and predicate_id = ... ) or (...) or ...)
    Alias = "edge_" ++ binary_to_list(z_ids:identifier()),
    OPClauses = [ object_predicate_clause(Alias, Obj,Pred) || {Obj,Pred} <- OPs ],
    Where = lists:flatten([
                "rsc.id in (select ", Alias ,".subject_id from edge ",Alias," where (",
                    z_utils:combine(") or (", OPClauses),
                "))"
                ]),
    Result1 = add_where(Where, Result),
    parse_query(Rest, Context, Result1);

%% hasobjectpredicate=predicate
%% Give all things which have any outgoing edge with given predicate
parse_query([{hasobjectpredicate, Predicate}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("subject_id", Result),
    PredicateId = predicate_to_id(Predicate, Context),
    {Arg1, Result2} = add_arg(PredicateId, Result1),
    Result3 = add_where(A ++ ".predicate_id = " ++ Arg1, Result2),
    parse_query(Rest, Context, Result3);

%% hassubjectpredicate=predicate
%% Give all things which have any incoming edge with given predicate
parse_query([{hassubjectpredicate, Predicate}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),
    PredicateId = predicate_to_id(Predicate, Context),
    {Arg1, Result2} = add_arg(PredicateId, Result1),
    Result3 = add_where(A ++ ".predicate_id = " ++ Arg1, Result2),
    parse_query(Rest, Context, Result3);

%% is_featured or is_featured={false,true}
%% Filter on whether an item is featured or not.
parse_query([{is_featured, Boolean}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_bool(Boolean), Result),
    Result2 = add_where("rsc.is_featured = " ++ Arg, Result1),
    parse_query(Rest, Context, Result2);

%% is_published or is_published={false,true,all}
%% Filter on whether an item is published or not.
parse_query([{is_published, Boolean}|Rest], Context, Result) ->
    Result1 = Result#search_sql{extra=[no_publish_check,Result#search_sql.extra]},
    case z_convert:to_list(Boolean) of
        "all" ->
            parse_query(Rest, Context, Result1);
        _ ->
            Result2 = case z_convert:to_bool(Boolean) of
                          true ->
                              add_where("rsc.is_published and "
                                        "rsc.publication_start <= now() and "
                                        "rsc.publication_end >= now()",
                                        Result1);
                          false ->
                              add_where("(not rsc.is_published or "
                                        "rsc.publication_start > now() or "
                                        "rsc.publication_end < now())",
                                        Result1)
                      end,
            parse_query(Rest, Context, Result2)
    end;


%% upcoming
%% Filter on items whose start date lies in the future
parse_query([{upcoming, Boolean}|Rest], Context, Result) ->
    Result1 = case z_convert:to_bool(Boolean) of
                  true -> add_where("rsc.pivot_date_start >= current_date", Result);
                  false -> Result
              end,
    parse_query(Rest, Context, Result1);

%% ongoing
%% Filter on items whose date range is around the current date
parse_query([{ongoing, Boolean}|Rest], Context, Result) ->
    Result1 = case z_convert:to_bool(Boolean) of
                  true -> add_where("rsc.pivot_date_start <= now() and rsc.pivot_date_end >= now()", Result);
                  false -> Result
              end,
    parse_query(Rest, Context, Result1);

%% finished
%% Filter on items whose start date lies in the past
parse_query([{finished, Boolean}|Rest], Context, Result) ->
    Result1 = case z_convert:to_bool(Boolean) of
                  true -> add_where("rsc.pivot_date_start < current_date", Result);
                  false -> Result
              end,
    parse_query(Rest, Context, Result1);

%% Filter on items whose start date lies in the future
parse_query([{unfinished, Boolean}|Rest], Context, Result) ->
    Result1 = case z_convert:to_bool(Boolean) of
                  true -> add_where("rsc.pivot_date_end >= current_date", Result);
                  false -> Result
              end,
    parse_query(Rest, Context, Result1);

%% Filter on items whose start date lies in the future or don't have an end_date
parse_query([{unfinished_or_nodate, Boolean}|Rest], Context, Result) ->
    Result1 = case z_convert:to_bool(Boolean) of
                  true -> add_where("(rsc.pivot_date_end >= current_date or rsc.pivot_date_start is null)", Result);
                  false -> Result
              end,
    parse_query(Rest, Context, Result1);

%% authoritative={true|false}
%% Filter on items which are authoritative or not
parse_query([{is_authoritative, Boolean}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_bool(Boolean), Result),
    Result2 = add_where("rsc.is_authoritative = " ++ Arg, Result1),
    parse_query(Rest, Context, Result2);

%% creator_id=<rsc id>
%% Filter on items which are created by <rsc id>
parse_query([{creator_id, Integer}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Integer), Result),
    Result2 = add_where("rsc.creator_id = " ++ Arg, Result1),
    parse_query(Rest, Context, Result2);

%% modifier_id=<rsc id>
%% Filter on items which are last modified by <rsc id>
parse_query([{modifier_id, Integer}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Integer), Result),
    Result2 = add_where("rsc.modifier_id = " ++ Arg, Result1),
    parse_query(Rest, Context, Result2);

%% qargs
%% Add all query terms from the current query arguments
parse_query([{qargs, true}|Rest], Context, Result) ->
    Terms = parse_request_args(qargs(Context)),
    parse_query(Terms++Rest, Context, Result);

%% query_id=<rsc id>
%% Get the query terms from given resource ID, and use those terms.
parse_query([{query_id, Id}|Rest], Context, Result) ->
    case m_category:is_a(m_rsc:p(Id, category_id, Context), 'query', Context) of
        true ->
            QArgs = try
                        parse_query_text(z_html:unescape(m_rsc:p(Id, 'query', Context)))
                    catch
                        throw:{error,{unknown_query_term,Term}} ->
                            lager:error("[~p] Unknown query term in search query ~p: ~p",
                                        [z_context:site(Context), Id, Term]),
                            []
                    end,
            parse_query(QArgs ++ Rest, Context, Result);
        false ->
                                                % Fetch the id's haspart objects (assume a collection)
            parse_query([{hassubject, [Id, haspart]} | Rest], Context, Result)
    end;

%% rsc_id=<rsc id>
%% Filter to *only* include the given rsc id. Can be used for resource existence check.
parse_query([{rsc_id, Id}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(Id, Result),
    Result2 = add_where("rsc.id = " ++ Arg, Result1),
    parse_query(Rest, Context, Result2);

%% name=<name-pattern>
%% Filter on the unique name of a resource.
parse_query([{name, Name}|Rest], Context, Result) ->
    case z_string:to_lower(mod_search:trim(Name, Context)) of
        All when All =:= <<>>; All =:= <<"*">>; All =:= <<"%">> ->
            Result2 = add_where("rsc.name is not null", Result),
            parse_query(Rest, Context, Result2);
        Name1 ->
            Name2 = binary:replace(Name1, <<"*">>, <<"%">>, [global]),
            {Arg, Result1} = add_arg(Name2, Result),
            Result2 = add_where("rsc.name like " ++ Arg, Result1),
            parse_query(Rest, Context, Result2)
    end;

%% sort=fieldname
%% Order by a given field. Putting a '-' in front of the field name reverts the ordering.
parse_query([{sort, Sort}|Rest], Context, Result) ->
    parse_query(Rest, Context, add_order(Sort,Result));
parse_query([{asort, Sort}|Rest], Context, Result) ->
    parse_query(Rest, Context, add_order(Sort,Result));
parse_query([{zsort, Sort}|Rest], Context, Result) ->
    parse_query(Rest, Context, add_order(Sort,Result));

%% custompivot=tablename
%% Add a join on the given custom pivot table.
parse_query([{custompivot, Table}|Rest], Context, Result) ->
    Table1 = case is_atom(Table) of
                 true -> atom_to_list(Table);
                 false -> Table
             end,
    parse_query(Rest, Context, add_custompivot_join(Table1, Result));

%% text=...
%% Perform a fulltext search
parse_query([{text, Text}|Rest], Context, Result) ->
    case mod_search:trim(Text, Context) of
        <<>> -> parse_query(Rest, Context, Result);
        <<"id:", S/binary>> -> mod_search:find_by_id(S, Context);
        _ ->
            TsQuery = mod_search:to_tsquery(Text, Context),
            {QArg, Result1} = add_arg(TsQuery, Result),
            {BArg, Result1a} = add_arg(mod_search:rank_behaviour(Context), Result1),
            Result2 = add_where(QArg++" @@ rsc.pivot_tsv", Result1a),
            Result3 = add_order_unsafe(
                              "ts_rank_cd("
                                ++mod_search:rank_weight(Context)
                                ++", rsc.pivot_tsv, "
                                ++QArg++", "
                                ++BArg++") desc", Result2),
            parse_query(Rest, Context, Result3)
    end;

%% match_objects=<id>
%% Match on the objects of the resource, best matching return first.
%% Similar to the {match_objects id=...} query.
parse_query([{match_objects, RId}|Rest], Context, Result) ->
    case m_rsc:rid(RId, Context) of
        undefined ->
            #search_result{};
        Id ->
            ObjectIds = m_edge:objects(Id, Context),
            parse_query([{match_object_ids, ObjectIds}, {id_exclude, Id}|Rest], Context, Result)
    end;
parse_query([{match_object_ids, ObjectIds} | Rest], Context, Result) ->
    ObjectIds1 = [ m_rsc:rid(OId, Context) || OId <- ObjectIds ],
    MatchTerms = [ ["zpo",integer_to_list(ObjId)] || ObjId <- ObjectIds1, is_integer(ObjId) ],
    TsQuery = lists:flatten(z_utils:combine("|", MatchTerms)),
    case TsQuery of
        [] ->
            #search_result{};
        _ ->
            {QArg, Result1} = add_arg(TsQuery, Result),
            Result2 = Result1#search_sql{
                        from=Result1#search_sql.from ++ ", to_tsquery(" ++ QArg ++ ") matchquery"
                       },
            Result3 = add_where("matchquery @@ rsc.pivot_rtsv", Result2),
            Result4 = add_order_unsafe("ts_rank(rsc.pivot_rtsv, matchquery) desc", Result3),
            parse_query(Rest, Context, Result4)
    end;

%% date_start_after=date
%% Filter on date_start after a specific date.
parse_query([{date_start_after, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_start >= " ++ Arg, Result1));

%% date_start_after=date
%% Filter on date_start before a specific date.
parse_query([{date_start_before, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_start <= " ++ Arg, Result1));

%% date_start_year=year
%% Filter on year of start date
parse_query([{date_start_year, Year}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Year), Result),
    parse_query(Rest, Context, add_where("date_part('year', rsc.pivot_date_start) = " ++ Arg, Result1));

%% date_end_after=date
%% Filter on date_end after a specific date.
parse_query([{date_end_after, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_end >= " ++ Arg, Result1));

%% date_end_after=date
%% Filter on date_end before a specific date.
parse_query([{date_end_before, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_end <= " ++ Arg, Result1));

%% date_end_year=year
%% Filter on year of end date
parse_query([{date_end_year, Year}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Year), Result),
    parse_query(Rest, Context, add_where("date_part('year', rsc.pivot_date_end) = " ++ Arg, Result1));

%% publication_year=year
%% Filter on year of publication
parse_query([{publication_year, Year}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Year), Result),
    parse_query(Rest, Context, add_where("date_part('year', rsc.publication_start) = " ++ Arg, Result1));

%% publication_month=month
%% Filter on month of publication
parse_query([{publication_month, Month}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Month), Result),
    parse_query(Rest, Context, add_where("date_part('month', rsc.publication_start) = " ++ Arg, Result1));

parse_query([{publication_after, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.publication_start >= " ++ Arg, Result1));

parse_query([{publication_before, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_datetime:to_datetime(Date, Context), Result),
    parse_query(Rest, Context, add_where("rsc.publication_start <= " ++ Arg, Result1));

%% No match found
parse_query([Term|_], _Context, _Result) ->
    throw({error, {unknown_query_term, Term}}).

%%
%% Helper functions
%%

%% Add a value to a proplist. If it is already there, the value is
%% replaced by a list of values.
add_or_append(Key, Value, PropList) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            [{Key, Value} | PropList];
        Val when not(is_list(Val)) ->
            [{Key, [Value, Val]} | proplists:delete(Key, PropList)];
        Val ->
            case z_string:is_string(Val) of
                true ->
                    [{Key, [Value, Val]} | proplists:delete(Key, PropList)];
                false ->
                    [{Key, [Value | Val]} | proplists:delete(Key, PropList)]
            end
    end.

%% Add a join on the edge table.
add_edge_join(ObjectOrSubject, SearchSql) ->
    add_edge_join("rsc", ObjectOrSubject, SearchSql).
add_edge_join(RscTable, ObjectOrSubject, Search) ->
    Alias = "edge" ++ integer_to_list(length(Search#search_sql.tables)),
    Search1 = add_where(Alias ++ "." ++ ObjectOrSubject ++ " = " ++ RscTable ++ ".id", Search),
    {Alias,
     Search1#search_sql{
       tables=Search1#search_sql.tables ++ [{edge, Alias}],
       from=Search1#search_sql.from ++ ", edge " ++ Alias
      }
    }.


%% Add a join on a custom pivot table
add_custompivot_join(Table, SearchSql) ->
    add_custompivot_join("rsc", Table, SearchSql).
add_custompivot_join(RscTable, Table, Search) ->
    Table1 = sql_safe(Table),
    RscTable1 = sql_safe(RscTable),
    Alias = "pivot" ++ integer_to_list(length(Search#search_sql.tables)),
    JoinClause = "(" ++ RscTable1 ++ ".id = " ++ Alias ++ ".id)",
    Search#search_sql{
      tables=Search#search_sql.tables ++ [{Table, Alias}],
      from=Search#search_sql.from ++ " left join pivot_" ++ Table1 ++ " " ++ Alias ++ " on " ++ JoinClause
     }.

%% Add a join on the hierarchy table.
% add_hierarchy_join(HierarchyName, Lft, Rght, Search) ->
%     {NameArg, Search1} = add_arg(HierarchyName, Search),
%     {LftArg, Search2} = add_arg(Lft, Search1),
%     {RghtArg, Search3} = add_arg(Rght, Search2),

%     A = "h" ++ integer_to_list(length(Search#search_sql.tables)),
%     Search4 = add_where(A ++ ".name = " ++ NameArg ++ " AND " ++ A ++ ".lft >= " ++ LftArg ++ " AND " ++ A ++ ".rght <= " ++ RghtArg, Search3),

%     Search4#search_sql{
%       tables=Search1#search_sql.tables ++ [{hierarchy, A}],
%       from=Search1#search_sql.from ++ ", hierarchy " ++ A
%      }.

%% Add an AND clause to the WHERE of a #search_sql
%% Clause is already supposed to be safe.
add_where(Clause, Search) ->
    case Search#search_sql.where of
        [] ->
            Search#search_sql{where=Clause};
        C ->
            Search#search_sql{where=C ++ " AND " ++ Clause}
    end.


%% Add an ORDER clause.
add_order(Order, Search) when is_atom(Order) ->
    add_order(atom_to_list(Order), Search);
add_order(Order, Search) when is_binary(Order) ->
    add_order(binary_to_list(Order), Search);
add_order("seq", Search) ->
    add_order("+seq", Search);
add_order([C,$s,$e,$q], Search) when C =:= $-; C =:= $+ ->
    case proplists:get_value(edge, Search#search_sql.tables) of
        L when is_list(L) ->
            Search1 = add_order([C|L]++".seq", Search),
            add_order([C|L]++".id", Search1);
        undefined ->
            Search
    end;
add_order("edge."++_ = Order, Search) ->
    add_order([$+|Order], Search);
add_order([C,$e,$d,$g,$e,$.|Order], Search) when C =:= $-; C =:= $+ ->
    case proplists:get_value(edge, Search#search_sql.tables) of
        L when is_list(L) -> add_order([C|L]++[$.|Order], Search);
        undefined -> Search
    end;
add_order(Sort, Search) ->
    Clause = case Sort of
                 "random" ->
                     "random()";
                 _ ->
                     case Sort of
                         [$-|F1] -> sql_safe(F1) ++ " DESC";
                         [$+|F1] -> sql_safe(F1) ++ " ASC";
                         _ -> sql_safe(Sort) ++ " ASC"
                     end
             end,
    add_order_unsafe(Clause, Search).

%% Add an ORDER clause without checking on SQL safety.
add_order_unsafe(Clause, Search) ->
    case Search#search_sql.order of
        [] ->
            Search#search_sql{order=Clause};
        C ->
            Search#search_sql{order=C ++ ", " ++ Clause}
    end.

%% Append an argument to a #search_sql
add_arg(ArgValue, Search) ->
    Arg = [$$] ++ integer_to_list(length(Search#search_sql.args) + 1),
    {Arg, Search#search_sql{args=Search#search_sql.args ++ [ArgValue]}}.


%% Make sure that parts of the query are safe to append to the search query.
sql_safe(String) when not is_list(String) ->
    sql_safe(z_convert:to_list(String));
sql_safe(String) ->
    case re:run(String, ?SQL_SAFE_REGEXP) of
        {match, _} ->
            String;
        _ ->
            throw({error, {unsafe_expression, String}})
    end.


%% Make sure the input is a list of valid categories.
assure_categories(Name, Context) ->
    Cats = case {z_string:is_string(Name), is_binary(Name)} of
               {true, false} -> [iolist_to_binary(Name)];
               {_, true} -> [Name];
               _ -> Name
           end,
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

%% Flatten eventual lists of categories
-spec assure_cat_flatten(any() | list()) -> list().
assure_cat_flatten(Name) when not is_list(Name) ->
    assure_cat_flatten([Name]);
assure_cat_flatten(Names) when is_list(Names) ->
    lists:flatten([
                     case is_list(N) of
                         true ->
                             case z_string:is_string(N) of
                                 true -> iolist_to_binary(N);
                                 false -> assure_cat_flatten(N)
                             end;
                         false ->
                             N
                     end
                     || N <- Names]).

%% Make sure the given name is a category.
assure_category([], _) -> undefined;
assure_category(<<>>, _) -> undefined;
assure_category(undefined, _) -> undefined;
assure_category([$'|_] = Name, Context) ->
    case lists:last(Name) of
        $' -> assure_category_1(z_string:trim(Name, $'), Context);
        _ -> assure_category_1(Name, Context)
    end;
assure_category([$"|_] = Name, Context) ->
    case lists:last(Name) of
        $" -> assure_category_1(z_string:trim(Name, $"), Context);
        _ -> assure_category_1(Name, Context)
    end;
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
                    lager:warning("Query: unknown category '~p'", [Name]),
                    display_error([ ?__("Unknown category", Context), 32, $", z_html:escape(z_convert:to_binary(Name)), $" ], Context),
                    error;
                CatId ->
                    case m_category:id_to_name(CatId, Context) of
                        undefined ->
                            lager:warning("Query: '~p' is not a category", [Name]),
                            display_error([ $", z_html:escape(z_convert:to_binary(Name)), $", 32, ?__("is not a category", Context) ], Context),
                            error;
                        Name1 ->
                            {ok, Name1}
                    end
            end
    end.

%% If the current user is an administrator or editor, show an error message about this search
display_error(Msg, Context) ->
    case z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            ContextPruned = z_context:prune_for_async(Context),
            z_session_page:add_script(z_render:growl_error(Msg, ContextPruned));
        false ->
            ok
    end.


%% Add filters
add_filters([ [Column|_] | _ ] = Filters, Result) when is_list(Column); is_binary(Column); is_atom(Column) ->
    add_filters_or(Filters, Result);
add_filters({'or', Filters}, Result) ->
    add_filters_or(Filters, Result);
add_filters([Column, Value], R) ->
    add_filters([Column, eq, Value], R);
add_filters([Column, Operator, Value], Result) ->
    {Expr, Result1} = create_filter(Column, Operator, Value, Result),
    add_where(Expr, Result1).

add_filters_or(Filters, Result) ->
    {Exprs, Result1} = lists:foldr(
                         fun
                            ([C,O,V], {Es, R}) ->
                                 {E, R1} = create_filter(C, O, V, R),
                                 {[E|Es], R1};
                            ([C,V], {Es, R}) ->
                                 {E, R1} = create_filter(C, eq, V, R),
                                 {[E|Es], R1}
                         end,
                         {[], Result},
                         Filters),
    Or = "(" ++ string:join(Exprs, " OR ") ++ ")",
    add_where(Or, Result1).

create_filter(Column, Operator, null, Result) ->
    create_filter(Column, Operator, undefined, Result);
create_filter(Column, Operator, undefined, Result) ->
    Column1 = sql_safe(Column),
    Operator1 = map_filter_operator(Operator),
    {create_filter_null(Column1, Operator1), Result};
create_filter(Column, Operator, Value, Result) ->
    {Arg, Result1} = add_arg(Value, Result),
    Column1 = sql_safe(Column),
    Operator1 = map_filter_operator(Operator),
    {Column1 ++ " " ++ Operator1 ++ " " ++ Arg, Result1}.

create_filter_null(Column, "=") ->
    Column ++ " is null";
create_filter_null(Column, "<>") ->
    Column ++ " is not null";
create_filter_null(_Column, _Op) ->
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
maybe_split_list(Id) when is_integer(Id) ->
    [Id];
maybe_split_list(<<"[", Rest/binary>>) ->
    split_list(Rest);
maybe_split_list([$[|Rest]) ->
    split_list(z_convert:to_binary(Rest));
maybe_split_list(Other) ->
    [Other].

split_list(Bin) ->
    Bin1 = binary:replace(Bin, <<"]">>, <<>>, [global]),
    Parts = binary:split(Bin1, <<",">>, [global]),
    [ unquot(z_string:trim(P)) || P <- Parts ].

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
            lager:warning("Query: unknown predicate '~p'", [Pred]),
            display_error([ ?__("Unknown predicate", Context), 32, $", z_html:escape(z_convert:to_binary(Pred)), $" ], Context),
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
