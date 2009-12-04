%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-04-12
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
    search/2
]).

-include_lib("zotonic.hrl").

search(Query, Context) ->
    Start = #search_sql{select="rsc.id",
                        from="rsc rsc",
                        tables=[{rsc, "rsc"}]},
    R = parse_query(Query, Context, Start),
    %% add default sorting
    add_order("rsc.id desc", R).


parse_query([], _Context, Result) ->
    Result;

%% cat=categoryname
%% Filter results on a certain category.
parse_query([{cat, Cat}|Rest], Context, Result) ->
    Cats = add_or_append("rsc", Cat, Result#search_sql.cats),
    Tables1 = Result#search_sql.tables,
    parse_query(Rest, Context, Result#search_sql{cats=Cats, tables=Tables1});

%% hassubject=[id]
%% Give all things which have an outgoing edge to Id
parse_query([{hassubject, Id}|Rest], Context, Result) when is_integer(Id) ->
    parse_query([{hassubject, [Id]}|Rest], Context, Result);
parse_query([{hassubject, [Id]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("subject_id", Result),
    {Arg, Result2} = add_arg(Id, Result1),
    Result3 = add_where(A ++ ".object_id = " ++ Arg, Result2),
    parse_query(Rest, Context, Result3);

%% hassubject=[id,predicate]
%% Give all things which have an outgoing edge to Id with the given predicate
parse_query([{hassubject, [Id, Predicate]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("subject_id", Result),

    {Arg1, Result2} = add_arg(Id, Result1),
    Result3 = add_where(A ++ ".object_id = " ++ Arg1, Result2),

    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
    {Arg2, Result4} = add_arg(PredicateId, Result3),
    Result5 = add_where(A ++ ".predicate_id = " ++ Arg2, Result4),
    parse_query(Rest, Context, Result5);

%% hasobject=[id]
%% Give all things which have an outgoing edge to Id
parse_query([{hasobject, Id}|Rest], Context, Result) when is_integer(Id) ->
    parse_query([{hasobject, [Id]}|Rest], Context, Result);
parse_query([{hasobject, [Id]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),
    {Arg, Result2} = add_arg(Id, Result1),
    Result3 = add_where(A ++ ".subject_id = " ++ Arg, Result2),
    parse_query(Rest, Context, Result3);

%% hasobject=[id,predicate]
%% Give all things which have an outgoing edge to Id with the given predicate
parse_query([{hasobject, [Id, Predicate]}|Rest], Context, Result) ->
    {A, Result1} = add_edge_join("object_id", Result),

    {Arg1, Result2} = add_arg(Id, Result1),
    Result3 = add_where(A ++ ".subject_id = " ++ Arg1, Result2),

    PredicateId = m_predicate:name_to_id_check(Predicate, Context),
    {Arg2, Result4} = add_arg(PredicateId, Result3),
    Result5 = add_where(A ++ ".predicate_id = " ++ Arg2, Result4),
    parse_query(Rest, Context, Result5);

%% is_featured or is_featured={false,true}
%% Filter on whether an item is featured or not.
parse_query([{is_featured, Boolean}|Rest], Context, Result) ->
    {Arg, Result1} = add_arg(Boolean, Result),
     Result2 = add_where("rsc.is_featured = " ++ Arg, Result1),
     parse_query(Rest, Context, Result2);

%% upcoming
%% Filter on items whose end date lies in the future
parse_query([{upcoming, true}|Rest], Context, Result) ->
     Result1 = add_where("rsc.pivot_date_end >= current_date", Result),
     parse_query(Rest, Context, Result1);


%% sort=fieldname
%% Order by a given field. Putting a '-' in front of the field name reverts the ordering.
parse_query([{sort, Sort}|Rest], Context, Result) ->
    Sort1 = case is_atom(Sort) of
                true -> atom_to_list(Sort);
                false -> Sort
            end,
    [FirstChar|F1] = Sort1,
    Order = case FirstChar of
                $- -> F1 ++ " DESC";
                $+ -> F1 ++ " ASC";
                _ -> Sort ++ " ASC"
            end,
    parse_query(Rest, Context, add_order(Order, Result));

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

%% Add an AND clause to the WHERE of a #search_sql
add_where(Clause, Search) ->
    case Search#search_sql.where of
        [] ->
            Search#search_sql{where=Clause};
        C ->
            Search#search_sql{where=C ++ " AND " ++ Clause}
    end.

%% Add an AND clause to the WHERE of a #search_sql
add_order(Clause, Search) ->
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

