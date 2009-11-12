%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-07
%%
%% @doc Interface to database, uses database definition from Context

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

-module(z_db).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    transaction/2,
    set/3,
    get/2,
    get_parameter/2,
    assoc_row/2,
    assoc_row/3,
    assoc_props_row/2,
    assoc_props_row/3,
    assoc/2,
    assoc/3,
    assoc_props/2,
    assoc_props/3,
    q/2,
    q/3,
    q1/2,
    q1/3,
    q_row/2,
    q_row/3,
    insert/2,
    insert/3,
    update/4,
    delete/3,
    select/3,
    columns/2,
    update_sequence/3,
    table_exists/2,
    
    assert_table_name/1,
    prepare_cols/2
]).

-include_lib("zotonic.hrl").


%% @doc Perform a function inside a transaction, do a rollback on exceptions
%% @spec transaction(Function, Context) -> FunctionResult | {error, Reason}
transaction(Function, #context{dbc=undefined} = Context) ->
    Host     = Context#context.host,
    {ok, C}  = pgsql_pool:get_connection(Host),
    Context1 = Context#context{dbc=C},
    Result = try
                {ok, [], []} = pgsql:squery(C, "BEGIN"),
                R = Function(Context1),
                {ok, [], []} = pgsql:squery(C, "COMMIT"),
                R
             catch
                _:Why ->
                    pgsql:squery(C, "ROLLBACK"),
                    {rollback, {Why, erlang:get_stacktrace()}}
             end,
    pgsql_pool:return_connection(Host, C),
    Result;
transaction(Function, Context) ->
    % Nested transaction, only keep the outermost transaction
    Function(Context).


%% @doc Simple get/set functions for db property lists
set(Key, Props, Value) ->
    lists:keystore(Key, 1, Props, {Key, Value}).
get(Key, Props) ->
    proplists:get_value(Key, Props).


%% @doc Transaction handler safe function for fetching a db connection
get_connection(#context{dbc=undefined, host=Host}) ->
    {ok, C} = pgsql_pool:get_connection(Host),
    C;
get_connection(Context) ->
    Context#context.dbc.

%% @doc Transaction handler safe function for releasing a db connection
return_connection(C, #context{dbc=undefined, host=Host}) ->
    pgsql_pool:return_connection(Host, C);
return_connection(_C, _Context) -> 
    ok.


assoc_row(Sql, Context) ->
    assoc_row(Sql, [], Context).

assoc_row(Sql, Parameters, Context) ->
    case assoc(Sql, Parameters, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.

assoc_props_row(Sql, Context) ->
    assoc_props_row(Sql, [], Context).

assoc_props_row(Sql, Parameters, Context) ->
    case assoc_props(Sql, Parameters, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.
    

get_parameter(Parameter, Context) ->
    C = get_connection(Context),
    {ok, Result} = pgsql:get_parameter(C, z_convert:to_binary(Parameter)),
    return_connection(C, Context),
    Result.
    

%% @doc Return property lists of the results of a query on the database in the Context
%% @spec assoc(SqlQuery, Context) -> Rows
assoc(Sql, Context) ->
    assoc(Sql, [], Context).

assoc(Sql, Parameters, Context) ->
    C = get_connection(Context),
    {ok, Result} = pgsql:assoc(C, Sql, Parameters),
    return_connection(C, Context),
    Result.


assoc_props(Sql, Context) ->
    assoc_props(Sql, [], Context).

assoc_props(Sql, Parameters, Context) ->
    C = get_connection(Context),
    {ok, Result} = pgsql:assoc(C, Sql, Parameters),
    return_connection(C, Context),
    merge_props(Result).

q(Sql, Context) ->
    q(Sql, [], Context).

q(Sql, Parameters, Context) ->
    C = get_connection(Context),
    Result = case pgsql:equery(C, Sql, Parameters) of
                {ok, _Cols, Rows} -> Rows;
                {ok, Rows} -> Rows
              end,
    return_connection(C, Context),
    Result.

q1(Sql, Context) ->
    q1(Sql, [], Context).

q1(Sql, Parameters, Context) ->
    C = get_connection(Context),
    V = case pgsql:equery1(C, Sql, Parameters) of
            {ok, Value} -> Value;
            {error, noresult} -> undefined
        end,
    return_connection(C, Context),
    V.

q_row(Sql, Context) ->
    q_row(Sql, [], Context).

q_row(Sql, Args, Context) ->
    case q(Sql, Args, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.


%% @doc Insert a new row in a table, use only default values.
%% @spec insert(Table, Context) -> {ok, Id}
insert(Table, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Context);
insert(Table, Context) ->
    assert_table_name(Table),
    C = get_connection(Context),
    {ok, Id} = pgsql:equery1("insert into \""++Table++"\" default values returning id"),
    return_connection(C, Context),
    {ok, Id}.


%% @doc Insert a row, setting the fields to the props.  Unknown columns are serialized in the props column.
%% When the table has an 'id' column then the new id is returned.
%% @spec insert(Database, Context) -> {ok, Id} | Error
insert(Table, [], Context) ->  
    insert(Table, Context);
insert(Table, Props, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Props, Context);
insert(Table, Props, Context) ->  
    assert_table_name(Table),
    Cols = columns(Table, Context),
    InsertProps = prepare_cols(Cols, Props),
    C = get_connection(Context),

    InsertProps1 = case proplists:get_value(props, InsertProps) of
        undefined ->
            InsertProps;
        PropsCol -> 
            lists:keystore(props, 1, InsertProps, {props, cleanup_props(PropsCol)})
    end,
    
    %% Build the SQL insert statement
    {ColNames,Parameters} = lists:unzip(InsertProps1),
    Sql = "insert into \""++Table++"\" (\"" 
             ++ string:join([ atom_to_list(ColName) || ColName <- ColNames ], "\", \"")
             ++ "\") values ("
             ++ string:join([ [$$ | integer_to_list(N)] || N <- lists:seq(1, length(Parameters)) ], ", ")
             ++ ")",

    FinalSql = case proplists:is_defined(id, Cols) of
        true -> Sql ++ " returning id";
        false -> Sql
    end,

    Id = case pgsql:equery1(C, FinalSql, Parameters) of
            {ok, IdVal} -> IdVal;
            {error, noresult} -> undefined
         end,
    return_connection(C, Context),
    {ok, Id}.


%% @doc Update a row in a table, merging the props list with any new props values
%% @spec update(Table, Id, Parameters, Context) -> {ok, RowsUpdated}
update(Table, Id, Parameters, Context) when is_atom(Table) ->
    update(atom_to_list(Table), Id, Parameters, Context);
update(Table, Id, Parameters, Context) ->
    assert_table_name(Table),
    Cols         = columns(Table, Context),
    UpdateProps  = prepare_cols(Cols, Parameters),
    C            = get_connection(Context),
    UpdateProps1 = case proplists:is_defined(props, UpdateProps) of
        true ->
            % Merge the new props with the props in the database
            {ok, OldProps} = pgsql:equery1(C, "select props from \""++Table++"\" where id = $1", [Id]),
            case is_list(OldProps) of
                true ->
                    FReplace = fun ({P,_} = T, L) -> lists:keystore(P, 1, L, T) end,
                    NewProps = lists:foldl(FReplace, OldProps, proplists:get_value(props, UpdateProps)),
                    lists:keystore(props, 1, UpdateProps, {props, cleanup_props(NewProps)});
                false ->
                    UpdateProps
            end;
        false ->
            UpdateProps
    end,

    {ColNames,Params} = lists:unzip(UpdateProps1),
    ColNamesNr = lists:zip(ColNames, lists:seq(2, length(ColNames)+1)),

    Sql = "update \""++Table++"\" set " 
             ++ string:join([ "\"" ++ atom_to_list(ColName) ++ "\" = $" ++ integer_to_list(Nr) || {ColName, Nr} <- ColNamesNr ], ", ")
             ++ " where id = $1",
    {ok, RowsUpdated} = pgsql:equery1(C, Sql, [Id | Params]),
    return_connection(C, Context),
    {ok, RowsUpdated}.


%% @doc Delete a row from a table, the row must have a column with the name 'id'
%% @spec delete(Table, Id, Context) -> {ok, RowsDeleted}
delete(Table, Id, Context) when is_atom(Table) ->
    delete(atom_to_list(Table), Id, Context);
delete(Table, Id, Context) ->
    assert_table_name(Table),
    C   = get_connection(Context),
    Sql = "delete from \""++Table++"\" where id = $1", 
    {ok, RowsDeleted} = pgsql:equery1(C, Sql, [Id]),
    return_connection(C, Context),
    {ok, RowsDeleted}.



%% @doc Read a row from a table, the row must have a column with the name 'id'.  
%% The props column contents is merged with the other properties returned.
%% @spec select(Table, Id, Context) -> {ok, Row}
select(Table, Id, Context) when is_atom(Table) ->
    select(atom_to_list(Table), Id, Context);
select(Table, Id, Context) ->
    assert_table_name(Table),
    C   = get_connection(Context),
    Sql = "select * from \""++Table++"\" where id = $1 limit 1", 
    {ok, Row} = pgsql:assoc(C, Sql, [Id]),
    return_connection(C, Context),

    Props = case Row of
        [R] ->
            case proplists:get_value(props, R) of
                PropsCol when is_list(PropsCol) -> 
                    lists:keydelete(props, 1, R) ++ PropsCol;
                _ ->
                    R
            end;
        [] ->
            []
    end,
    {ok, Props}.


%% @doc Remove all undefined props, translate texts to binaries.
cleanup_props(Ps) when is_list(Ps) ->
    [ {K,to_binary_string(V)} || {K,V} <- Ps, V /= undefined ];
cleanup_props(P) -> 
    P.

    to_binary_string([]) -> [];
    to_binary_string(L) when is_list(L) ->
        case z_string:is_string(L) of
            true -> list_to_binary(L);
            false -> L
        end;
    to_binary_string({trans, Tr}) ->
        {trans, [ {Lang,to_binary(V)} || {Lang,V} <- Tr ]};
    to_binary_string(V) -> 
        V.

    to_binary(L) when is_list(L) -> list_to_binary(L);
    to_binary(V) -> V.


%% @doc Check if all cols are valid columns in the target table, move unknown properties to the props column (if exists)
prepare_cols(Cols, Props) ->
    {CProps, PProps} = split_props(Props, Cols),
    case PProps of
        [] ->
            CProps;
        _  -> 
            PPropsMerged = case proplists:is_defined(props, CProps) of
                            true ->
                                FReplace = fun ({P,_} = T, L) -> lists:keystore(P, 1, L, T) end,
                                lists:foldl(FReplace, proplists:get_value(props, CProps), PProps);
                            false -> 
                                PProps
                           end,
            [{props, PPropsMerged} | proplists:delete(props, CProps)]
    end.

split_props(Props, Cols) ->
    {CProps, PProps} = lists:partition(fun ({P,_V}) -> proplists:is_defined(P, Cols) end, Props),
    case PProps of
        [] -> ok;
        _  -> z_utils:assert(proplists:is_defined(props, Cols), {unknown_column, PProps})
    end,
    {CProps, PProps}.


%% @doc Return a property list with all columns of the table. (example: [{id,int4},...])
%% @spec columns(Table, Context) -> PropList
columns(Table, Context) when is_atom(Table) ->
    columns(atom_to_list(Table), Context);
columns(Table, Context) ->
    Db = Context#context.host,
    case z_depcache:get({columns, Db, Table}, Context) of
        {ok, Cols} -> 
            Cols;
        _ ->
            C = get_connection(Context),
            Cols = pgsql:columns(C, Table),
            return_connection(C, Context),
            z_depcache:set({columns, Db, Table}, Cols, ?YEAR, [{database, Db}], Context),
            Cols
    end.


%% @doc Update the sequence of the ids in the table. They will be renumbered according to their position in the id list.
%% @spec update_sequence(Table, IdList, Context) -> void()
update_sequence(Table, Ids, Context) when is_atom(Table) ->
    update_sequence(atom_to_list(Table), Ids, Context);
update_sequence(Table, Ids, Context) ->
    assert_table_name(Table),
    Args = lists:zip(Ids, lists:seq(1, length(Ids))),
    Updater = fun(Ctx) -> 
                C = get_connection(Ctx),
                [ {ok, _} = pgsql:equery1(C, "update \""++Table++"\" set seq = $2 where id = $1", Arg) || Arg <- Args ],
                return_connection(C, Ctx)
            end,
    ok = transaction(Updater, Context).


%% @doc Check the information schema if a certain table exists in the context database.
%% @spec table_exists(TableName, Context) -> bool()
table_exists(Table, Context) ->
    {ok, Db} = pgsql_pool:get_database(?HOST(Context)),
    case q1("   select count(*) 
                from information_schema.tables 
                where table_catalog = $1 
                  and table_name = $2 
                  and table_type = 'BASE TABLE'", [Db, Table], Context) of
        1 -> true;
        0 -> false
    end.
    

%% @doc Check if a name is a valid SQL table name. Crashes when invalid
%% @spec check_table_name(String) -> true
assert_table_name([H|T]) when (H >= $a andalso H =< $z) orelse H == $_ ->
    assert_table_name1(T).
assert_table_name1([]) ->
    true;
assert_table_name1([H|T]) when (H >= $a andalso H =< $z) orelse (H >= $0 andalso H =< $9) orelse H == $_ ->
    assert_table_name1(T).



%% @doc Merge the contents of the props column into the result rows
%% @spec merge_props(list()) -> list()
merge_props(undefined) ->
    undefined;
merge_props(List) ->
    merge_props(List, []).
    
merge_props([], Acc) ->
    lists:reverse(Acc);
merge_props([R|Rest], Acc) ->
    case proplists:get_value(props, R) of
        undefined ->
            merge_props(Rest, [R|Acc]);
        <<>> ->
            merge_props(Rest, [R|Acc]);
        List ->
            merge_props(Rest, [lists:keydelete(props, 1, R)++List|Acc])
    end.
