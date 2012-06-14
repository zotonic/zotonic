%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009-2010 Arjan Scherpenisse
%% Date: 2009-04-12
%% @doc Handler for m.search[{query, Args..}]

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
         parse_query_text/1
]).

-include_lib("zotonic.hrl").

-define(SQL_SAFE_REGEXP, "^[0-9a-zA-Z_\.]+$").

search(Query, Context) ->
    Start = #search_sql{select="rsc.id",
                        from="rsc rsc",
                        tables=[{rsc, "rsc"}]},
    Query1 = filter_empty(Query),
    case parse_query(Query1, Context, Start) of
        [] -> #search_result{};
        #search_result{} = Result -> Result;
        Start -> #search_result{};
        R ->
            %% add any sort terms
            R1 = parse_sort(Query1, R),
            %% add default sorting
            add_order("-rsc.id", R1)
    end.



parse_request_args(Args) ->
    parse_request_args(Args, []).

parse_request_args([], Acc) ->
    Acc;
parse_request_args([{"zotonic_host",_}|Rest], Acc) ->
    parse_request_args(Rest, Acc);
parse_request_args([{"zotonic_dispatch",_}|Rest], Acc) ->
    parse_request_args(Rest, Acc);
parse_request_args([{K,V}|Rest], Acc) ->
    NewVal = V,
    parse_request_args(Rest, [{request_arg(K),NewVal}|Acc]).

%% Parses a query text. Every line is an argument; of which the first
%% '=' separates argument key from argument value.
%% @doc parse_query_text(string()) -> [{K, V}]
parse_query_text(Text) ->
    case is_binary(Text) of
        true ->
            parse_query_text(z_convert:to_list(Text));
        _ ->
            Lines = string:tokens(Text, "\n"),
            [ {request_arg(L), string:join(Rest, "=")} || [L|Rest] <- [ string:tokens(string:strip(L), "=") || L <- Lines] ]
    end.


% Convert request arguments to atom. Doing it this way avoids atom
% table overflows.
request_arg("authoritative")       -> authoritative;
request_arg("cat")                 -> cat;
request_arg("cat_exclude")         -> cat_exclude;
request_arg("creator_id")          -> creator_id;
request_arg("modifier_id")         -> modifier_id;
request_arg("custompivot")         -> custompivot;
request_arg("id_exclude")          -> id_exclude;
request_arg("hasobject")           -> hasobject;
request_arg("hasobjectpredicate")  -> hasobjectpredicate;
request_arg("hassubject")          -> hassubject;
request_arg("hassubjectpredicate") -> hassubjectpredicate;
request_arg("is_featured")         -> is_featured;
request_arg("is_published")        -> is_published;
request_arg("is_public")           -> is_public;
request_arg("date_start_after")    -> date_start_after;
request_arg("date_start_before")   -> date_start_before;
request_arg("date_start_year")     -> date_start_year;
request_arg("date_end_year")       -> date_end_year;
request_arg("publication_month")   -> publication_month;
request_arg("publication_year")    -> publication_year;
request_arg("query_id")            -> query_id;
request_arg("rsc_id")              -> rsc_id;
request_arg("sort")                -> sort;
request_arg("text")                -> text;
request_arg("upcoming")            -> upcoming;
request_arg("ongoing")             -> ongoing;
request_arg(Term)                  -> throw({error, {unknown_query_term, Term}}).


%% Private methods start here
filter_empty(Q) ->
    lists:filter(fun({_, X}) -> not(empty_term(X)) end, Q).

empty_term(X) when X =:= [] orelse X =:= undefined orelse X =:= <<>> -> true;
empty_term([X, _]) ->
    empty_term(X);
empty_term(_) ->
    false.


parse_sort([], Result) ->
    Result;
parse_sort([{sort, Sort}|Rest], Result) ->
    Sort1 = case is_atom(Sort) of
                true -> atom_to_list(Sort);
                false -> Sort
            end,
    parse_sort(Rest, add_order(Sort1, Result));
parse_sort([_|Rest], Result) ->
    parse_sort(Rest, Result).


parse_query([], _Context, Result) ->
    Result;

%% cat=categoryname
%% Filter results on a certain category.
parse_query([{cat, Cats}|Rest], Context, Result) ->
    Cats1 = assure_categories(Cats, Context),
    Cats2 = add_or_append("rsc", Cats1, Result#search_sql.cats),
    Tables1 = Result#search_sql.tables,
    parse_query(Rest, Context, Result#search_sql{cats=Cats2, tables=Tables1});

%% cat_exclude=categoryname
%% Filter results outside a certain category.
parse_query([{cat_exclude, Cats}|Rest], Context, Result) ->
    Cats1 = assure_categories(Cats, Context),
    Cats2 = add_or_append("rsc", Cats1, Result#search_sql.cats_exclude),
    Tables1 = Result#search_sql.tables,
    parse_query(Rest, Context, Result#search_sql{cats_exclude=Cats2, tables=Tables1});

%% id_exclude=resource-id
%% Exclude an id from the result
parse_query([{id_exclude, Id}|Rest], Context, Result)  when is_integer(Id) ->
    Result1 = add_where("rsc.id <> " ++ integer_to_list(Id), Result),
    parse_query(Rest, Context, Result1);

parse_query([{id_exclude, _Id}|Rest], Context, Result)  ->
    parse_query(Rest, Context, Result);


%% hassubject=[id]
%% Give all things which have an incoming edge to Id
parse_query([{hassubject, Id}|Rest], Context, Result) when is_integer(Id) ->
    parse_query([{hassubject, [Id]}|Rest], Context, Result);
parse_query([{hassubject, [Id]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),
    {Arg, Result2} = add_arg(m_rsc:rid(Id,Context), Result1),
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
    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
    {Arg2, Result3} = add_arg(PredicateId, Result2),
    Result4 = add_where(A ++ ".predicate_id = " ++ Arg2, Result3),
    parse_query(Rest, Context, Result4);
parse_query([{hassubject, Id}|Rest], Context, Result) when is_list(Id) ->
    parse_query([{hassubject, [m_rsc:rid(Id,Context)]}|Rest], Context, Result);


%% hasobject=[id]
%% Give all things which have an outgoing edge to Id
parse_query([{hasobject, Id}|Rest], Context, Result) when is_integer(Id) ->
    parse_query([{hasobject, [Id]}|Rest], Context, Result);
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
    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
    {Arg2, Result3} = add_arg(PredicateId, Result2),
    Result4 = add_where(A ++ ".predicate_id = " ++ Arg2, Result3),
    parse_query(Rest, Context, Result4);
parse_query([{hasobject, Id}|Rest], Context, Result) when is_list(Id) ->
    parse_query([{hasobject, [m_rsc:rid(Id,Context)]}|Rest], Context, Result);


%% hasobjectpredicate=predicate
%% Give all things which have any outgoing edge with given predicate
parse_query([{hasobjectpredicate, Predicate}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("subject_id", Result),
    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
    {Arg1, Result2} = add_arg(PredicateId, Result1),
    Result3 = add_where(A ++ ".predicate_id = " ++ Arg1, Result2),
    parse_query(Rest, Context, Result3);

%% hassubjectpredicate=predicate
%% Give all things which have any incoming edge with given predicate
parse_query([{hassubjectpredicate, Predicate}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),
    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
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


%% is_public or is_public={false,true,all}
%% Filter on whether an item is publicly visible or not.
parse_query([{is_public, Boolean}|Rest], Context, Result) ->
    case z_convert:to_list(Boolean) of
        "all" ->
            parse_query(Rest, Context, Result);
        _ ->
            Result2 = case z_convert:to_bool(Boolean) of
                          true ->
                              add_where("rsc.visible_for = 0", Result);
                          false ->
                              add_where("rsc.visible_for > 0", Result)
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


%% authoritative={true|false}
%% Filter on items which are authoritative or not
parse_query([{authoritative, Boolean}|Rest], Context, Result) ->
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

%% query_id=<rsc id>
%% Get the query terms from given resource ID, and use those terms.
parse_query([{query_id, Id}|Rest], Context, Result) ->
    case m_category:is_a(m_rsc:p(Id, category_id, Context), 'query', Context) of
        true ->
            Q = z_convert:to_list(m_rsc:p(Id, 'query', Context)),
            parse_query(parse_query_text(Q) ++ Rest, Context, Result);
        false ->
            % Fetch the id's haspart objects (assume a collection)
            parse_query([{hassubject, Id, haspart} | Rest], Context, Result)
    end;

%% rsc_id=<rsc id>
%% Filter to *only* include the given rsc id. Can be used for resource existence check.
parse_query([{rsc_id, Id}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(Id, Result),
     Result2 = add_where("rsc.id = " ++ Arg, Result1),
     parse_query(Rest, Context, Result2);

%% sort=fieldname
%% Order by a given field. Putting a '-' in front of the field name reverts the ordering.
parse_query([{sort, _Sort}|Rest], Context, Result) ->
    parse_query(Rest, Context, Result);

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
    case z_string:trim(Text) of 
        "id:"++ S -> mod_search:find_by_id(S, Context);
        [] -> parse_query(Rest, Context, Result);
        _ ->
            TsQuery = mod_search:to_tsquery(Text, Context),
            {QArg, Result1} = add_arg(TsQuery, Result),
            {LArg, Result2} = add_arg(z_pivot_rsc:pg_lang(Context#context.language), Result1),
            Result3 = Result2#search_sql{
                        from=Result2#search_sql.from ++ ", to_tsquery(" ++ LArg ++ ", " ++ QArg ++ ") txtquery"
                       },
            Result4 = add_where("txtquery @@ rsc.pivot_tsv", Result3),
            Result5 = add_order_unsafe("ts_rank_cd(rsc.pivot_tsv, txtquery, 32) desc", Result4),
            parse_query(Rest, Context, Result5)
    end;

%% date_start_after=date
%% Filter on date_start after a specific date.
parse_query([{date_start_after, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_datetime(Date), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_start >= " ++ Arg, Result1));

%% date_start_after=date
%% Filter on date_start before a specific date.
parse_query([{date_start_before, Date}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_datetime(Date), Result),
    parse_query(Rest, Context, add_where("rsc.pivot_date_start <= " ++ Arg, Result1));

%% date_start_year=year
%% Filter on year of start date
parse_query([{date_start_year, Year}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(z_convert:to_integer(Year), Result),
    parse_query(Rest, Context, add_where("date_part('year', rsc.pivot_date_start) = " ++ Arg, Result1));

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
add_order("seq", Search) ->
    case proplists:get_value(edge, Search#search_sql.tables) of
        L when is_list(L) -> add_order(L++".seq", Search);
        undefined -> Search
    end;
add_order(Sort, Search) ->
    Clause = case Sort of 
                 "random" ->
                     "random()";
                 _ -> 
                     [FirstChar|F1] = Sort,
                     case FirstChar of
                         $- -> sql_safe(F1) ++ " DESC";
                         $+ -> sql_safe(F1) ++ " ASC";
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
sql_safe(String) ->
    case re:run(String, ?SQL_SAFE_REGEXP) of
        {match, _} ->
            String;
        _ ->
            throw({error, {unsafe_expression, String}})
    end.


%% Make sure the input is a list of valid categories.
assure_categories(Name, Context) ->
    Cats = case z_string:is_string(Name) of
               true -> [Name];
               false -> Name
           end,
    lists:foldl(fun(C, Acc) ->
                    case assure_category(C, Context) of
                        error -> Acc;
                        {ok, N} -> [N|Acc]
                    end
                end,
                [],
                Cats).

%% Make sure the given name is a category.
assure_category([], _) -> error;
assure_category(<<>>, _) -> error;
assure_category(undefined, _) -> error;
assure_category(Name, Context) ->
    case m_category:name_to_id(Name, Context) of
        {ok, _Id} ->
            {ok, Name};
        _ -> 
            case m_rsc:rid(Name, Context) of
                undefined ->
                    lager:warning("Query: unknown category ~p", [Name]),
                    error;
                CatId ->
                    case m_category:id_to_name(CatId, Context) of
                        undefined ->
                            lager:warning("Query: ~p is not a category", [CatId]),
                            error;
                        Name1 ->
                            {ok, Name1}
                    end
            end
    end.


