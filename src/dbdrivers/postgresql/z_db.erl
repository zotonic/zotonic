%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-07
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
    has_connection/1,
    transaction/2,
    transaction/3,
    transaction_clear/1,
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
    equery/2,
    equery/3,
    insert/2,
    insert/3,
    update/4,
    delete/3,
    select/3,
    columns/2,
    column_names/2,
    update_sequence/3,
    table_exists/2,
    create_table/3,
    drop_table/2,
    flush/1,
    
    assert_table_name/1,
    prepare_cols/2
]).


-include_lib("pgsql.hrl").
-include_lib("zotonic.hrl").


%% @doc Perform a function inside a transaction, do a rollback on exceptions
%% @spec transaction(Function, Context) -> FunctionResult | {error, Reason}
transaction(Function, Context) ->
    transaction(Function, [], Context).

% @doc Perform a transaction with extra options. Default retry on deadlock
transaction(Function, Options, Context) ->
    Result = case transaction1(Function, Context) of
                {rollback, {{error, {error, error, <<"40P01">>, _, _}}, Trace1}} ->
                    {rollback, {deadlock, Trace1}};
                {rollback, {{case_clause, {error, {error, error,<<"40P01">>, _, _}}}, Trace2}} ->
                    {rollback, {deadlock, Trace2}};
                {rollback, {{badmatch, {error, {error, error,<<"40P01">>, _, _}}}, Trace2}} ->
                    {rollback, {deadlock, Trace2}};
                Other -> 
                    Other
            end,
    case Result of
        {rollback, {deadlock, Trace}} = DeadlockError ->
            case proplists:get_value(noretry_on_deadlock, Options) of
                true ->
                    ?zWarning(
                        io_lib:format("DEADLOCK on database transaction, NO RETRY '~p'", [Trace]),
                        Context
                    ),
                    DeadlockError;
                _False ->
                    ?zWarning(
                        io_lib:format("DEADLOCK on database transaction, will retry '~p'", [Trace]),
                        Context
                    ),
                    % Sleep random time, then retry transaction
                    timer:sleep(z_ids:number(100)),
                    transaction(Function, Options, Context)
            end;
        R ->
            R
    end.


% @doc Perform the transaction, return error when the transaction function crashed
transaction1(Function, #context{dbc=undefined} = Context) ->
    case has_connection(Context) of
        true ->
            Host     = Context#context.host,
            {ok, C}  = pgsql_pool:get_connection(Host),
            Context1 = Context#context{dbc=C},
            Result = try
                        case pgsql:squery(C, "BEGIN") of
                            {ok, [], []} -> ok;
                            {error, _} = ErrorBegin -> throw(ErrorBegin)
                        end,
                        R = Function(Context1),
                        case pgsql:squery(C, "COMMIT") of
                            {ok, [], []} -> ok;
                            {error, _} = ErrorCommit -> throw(ErrorCommit)
                        end,
                        R
                     catch
                        _:Why ->
                            pgsql:squery(C, "ROLLBACK"),
                            {rollback, {Why, erlang:get_stacktrace()}}
                     end,
            pgsql_pool:return_connection(Host, C),
            Result;
        false ->
            {rollback, {no_database_connection, erlang:get_stacktrace()}}
    end;
transaction1(Function, Context) ->
    % Nested transaction, only keep the outermost transaction
    Function(Context).
    
    

%% @doc Clear any transaction in the context, useful when starting a thread with this context.
transaction_clear(#context{dbc=undefined} = Context) ->
    Context;
transaction_clear(Context) ->
    Context#context{dbc=undefined}.


%% @doc Simple get/set functions for db property lists
set(Key, Props, Value) ->
    lists:keystore(Key, 1, Props, {Key, Value}).
get(Key, Props) ->
    proplists:get_value(Key, Props).


%% @doc Check if we have database connection
has_connection(#context{host=Host}) ->
    is_pid(erlang:whereis(Host)).


%% @doc Transaction handler safe function for fetching a db connection
get_connection(#context{dbc=undefined, host=Host} = Context) ->
    case has_connection(Context) of
        true ->
            {ok, C} = pgsql_pool:get_connection(Host),
            C;
        false ->
            none
    end;
get_connection(Context) ->
    Context#context.dbc.

%% @doc Transaction handler safe function for releasing a db connection
return_connection(C, #context{dbc=undefined, host=Host}) ->
    pgsql_pool:return_connection(Host, C);
return_connection(_C, _Context) -> 
    ok.

%% @doc Apply function F with a connection as parameter. Make sure the
%% connection is returned after usage.
with_connection(F, Context) ->
    with_connection(F, get_connection(Context), Context).

    with_connection(F, none, _Context) -> 
        F(none);
    with_connection(F, Connection, Context) when is_pid(Connection) -> 
        try
            F(Connection)
        after
            return_connection(Connection, Context)
	end.


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
    F = fun(C) ->
		{ok, Result} = pgsql:get_parameter(C, z_convert:to_binary(Parameter)),
		Result
	end,
    with_connection(F, Context).
    

%% @doc Return property lists of the results of a query on the database in the Context
%% @spec assoc(SqlQuery, Context) -> Rows
assoc(Sql, Context) ->
    assoc(Sql, [], Context).

assoc(Sql, Parameters, Context) ->
    F = fun(C) when C =:= none -> [];
	   (C) ->
                {ok, Result} = pgsql:assoc(C, Sql, Parameters),
                Result
	end,
    with_connection(F, Context).


assoc_props(Sql, Context) ->
    assoc_props(Sql, [], Context).

assoc_props(Sql, Parameters, Context) ->
    F = fun(C) when C =:= none -> [];
	   (C) ->
                {ok, Result} = pgsql:assoc(C, Sql, Parameters),
                merge_props(Result)
	end,
    with_connection(F, Context).


q(Sql, Context) ->
    q(Sql, [], Context).

q(Sql, Parameters, Context) ->
    F = fun(C) when C =:= none -> [];
	   (C) ->
                case pgsql:equery(C, Sql, Parameters) of
                    {ok, _Affected, _Cols, Rows} -> Rows;
                    {ok, _Cols, Rows} -> Rows;
                    {ok, Rows} -> Rows
                end
	end,
    with_connection(F, Context).

q1(Sql, Context) ->
    q1(Sql, [], Context).

q1(Sql, Parameters, Context) ->
    F = fun(C) when C =:= none -> undefined;
	   (C) ->
                case pgsql:equery1(C, Sql, Parameters) of
                    {ok, Value} -> Value;
                    {error, noresult} -> undefined
                end
	end,
    with_connection(F, Context).
	

q_row(Sql, Context) ->
    q_row(Sql, [], Context).

q_row(Sql, Args, Context) ->
    case q(Sql, Args, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.


equery(Sql, Context) ->
    equery(Sql, [], Context).
    
equery(Sql, Parameters, Context) ->
    F = fun(C) when C =:= none -> {error, noresult};
	   (C) ->
                pgsql:equery(C, Sql, Parameters)
	end,
    with_connection(F, Context).


%% @doc Insert a new row in a table, use only default values.
%% @spec insert(Table, Context) -> {ok, Id}
insert(Table, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Context);
insert(Table, Context) ->
    assert_table_name(Table),
    F = fun(C) ->
		pgsql:equery1(C, "insert into \""++Table++"\" default values returning id")
	end,
    with_connection(F, Context).


%% @doc Insert a row, setting the fields to the props.  Unknown columns are serialized in the props column.
%% When the table has an 'id' column then the new id is returned.
%% @spec insert(Table::atom(), Props::proplist(), Context) -> {ok, Id} | Error
insert(Table, [], Context) ->  
    insert(Table, Context);
insert(Table, Props, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Props, Context);
insert(Table, Props, Context) ->  
    assert_table_name(Table),
    Cols = column_names(Table, Context),
    InsertProps = prepare_cols(Cols, Props),

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

    FinalSql = case lists:member(id, Cols) of
        true -> Sql ++ " returning id";
        false -> Sql
    end,

    F = fun(C) ->
		Id = case pgsql:equery1(C, FinalSql, Parameters) of
			 {ok, IdVal} -> IdVal;
			 {error, noresult} -> undefined
		     end,
		{ok, Id}
	end,
    with_connection(F, Context).


%% @doc Update a row in a table, merging the props list with any new props values
%% @spec update(Table, Id, Parameters, Context) -> {ok, RowsUpdated}
update(Table, Id, Parameters, Context) when is_atom(Table) ->
    update(atom_to_list(Table), Id, Parameters, Context);
update(Table, Id, Parameters, Context) ->
    assert_table_name(Table),
    Cols         = column_names(Table, Context),
    UpdateProps  = prepare_cols(Cols, Parameters),
    F = fun(C) ->
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
        {ok, RowsUpdated}
    end,
    with_connection(F, Context).


%% @doc Delete a row from a table, the row must have a column with the name 'id'
%% @spec delete(Table, Id, Context) -> {ok, RowsDeleted}
delete(Table, Id, Context) when is_atom(Table) ->
    delete(atom_to_list(Table), Id, Context);
delete(Table, Id, Context) ->
    assert_table_name(Table),
    F = fun(C) ->
        Sql = "delete from \""++Table++"\" where id = $1", 
        {ok, RowsDeleted} = pgsql:equery1(C, Sql, [Id]),
        {ok, RowsDeleted}
	end,
    with_connection(F, Context).



%% @doc Read a row from a table, the row must have a column with the name 'id'.  
%% The props column contents is merged with the other properties returned.
%% @spec select(Table, Id, Context) -> {ok, Row}
select(Table, Id, Context) when is_atom(Table) ->
    select(atom_to_list(Table), Id, Context);
select(Table, Id, Context) ->
    assert_table_name(Table),
    F = fun(C) ->
		Sql = "select * from \""++Table++"\" where id = $1 limit 1", 
		pgsql:assoc(C, Sql, [Id])
	end,
    {ok, Row} = with_connection(F, Context),
    
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
    {CProps, PProps} = lists:partition(fun ({P,_V}) -> lists:member(P, Cols) end, Props),
    case PProps of
        [] -> ok;
        _  -> z_utils:assert(lists:member(props, Cols), {unknown_column, PProps})
    end,
    {CProps, PProps}.


%% @doc Return a property list with all columns of the table. (example: [{id,int4,modifier},...])
%% @spec columns(Table, Context) -> [ #column_def{} ]
columns(Table, Context) when is_atom(Table) ->
    columns(atom_to_list(Table), Context);
columns(Table, Context) ->
    {ok, Db} = pgsql_pool:get_database(?HOST(Context)),
    {ok, Schema} = pgsql_pool:get_database_opt(schema, ?HOST(Context)),
    case z_depcache:get({columns, Db, Schema, Table}, Context) of
        {ok, Cols} -> 
            Cols;
        _ ->
            Cols = q("  select column_name, data_type, character_maximum_length, is_nullable, column_default
                        from information_schema.columns
                        where table_catalog = $1
                          and table_schema = $2
                          and table_name = $3
                        order by ordinal_position", [Db, Schema, Table], Context),
            Cols1 = [ columns1(Col) || Col <- Cols ],
            z_depcache:set({columns, Db, Schema, Table}, Cols1, ?YEAR, [{database, Db}], Context),
            Cols1
    end.
    

    columns1({<<"id">>, <<"integer">>, undefined, Nullable, <<"nextval(", _/binary>>}) ->
        #column_def{
            name = id,
            type = "serial",
            length = undefined,
            is_nullable = z_convert:to_bool(Nullable),
            default = undefined
        };
    columns1({Name,Type,MaxLength,Nullable,Default}) ->
        #column_def{
            name = z_convert:to_atom(Name),
            type = z_convert:to_list(Type),
            length = MaxLength,
            is_nullable = z_convert:to_bool(Nullable),
            default = column_default(Default)
        }.
    
    column_default(undefined) -> undefined;
    column_default(<<"nextval(", _/binary>>) -> undefined;
    column_default(Default) -> binary_to_list(Default).


%% @doc Return a list with the column names of a table.  The names are sorted.
%% @spec column_names(Table, Context) -> [ atom() ]
column_names(Table, Context) ->
    Names = [ C#column_def.name || C <- columns(Table, Context)],
    lists:sort(Names).


%% @doc Flush all cached information about the database.
flush(Context) ->
    {ok, Db} = pgsql_pool:get_database(?HOST(Context)),
    z_depcache:flush({database, Db}, Context).


%% @doc Update the sequence of the ids in the table. They will be renumbered according to their position in the id list.
%% @spec update_sequence(Table, IdList, Context) -> void()
%% @todo Make the steps of the sequence bigger, and try to keep the old sequence numbers in tact (needs a diff routine)
update_sequence(Table, Ids, Context) when is_atom(Table) ->
    update_sequence(atom_to_list(Table), Ids, Context);
update_sequence(Table, Ids, Context) ->
    assert_table_name(Table),
    Args = lists:zip(Ids, lists:seq(1, length(Ids))),
    F = fun(C) when C =:= none -> 
		[];
	   (C) -> 
		[ {ok, _} = pgsql:equery1(C, "update \""++Table++"\" set seq = $2 where id = $1", Arg) || Arg <- Args ]
	   end,
    with_connection(F, Context).



%% @doc Check the information schema if a certain table exists in the context database.
%% @spec table_exists(TableName, Context) -> bool()
table_exists(Table, Context) ->
    {ok, Db} = pgsql_pool:get_database(?HOST(Context)),
    {ok, Schema} = pgsql_pool:get_database_opt(schema, ?HOST(Context)),
    case q1("   select count(*) 
                from information_schema.tables 
                where table_catalog = $1 
                  and table_name = $2 
                  and table_schema = $3
                  and table_type = 'BASE TABLE'", [Db, Table, Schema], Context) of
        1 -> true;
        0 -> false
    end.


%% @doc Make sure that a table is dropped, only when the table exists
drop_table(Name, Context) when is_atom(Name) ->
    drop_table(atom_to_list(Name), Context);
drop_table(Name, Context) ->
    case table_exists(Name, Context) of
        true -> q("drop table \""++Name++"\"", Context);
        false -> ok
    end.


%% @doc Ensure that a table with the given columns exists, alter any existing table
%% to add, modify or drop columns.  The 'id' (with type serial) column _must_ be defined
%% when creating the table.
create_table(Table, Cols, Context) when is_atom(Table)->
    create_table(atom_to_list(Table), Cols, Context);
create_table(Table, Cols, Context) ->
    assert_table_name(Table),
    ColsSQL = ensure_table_create_cols(Cols, []),
    z_db:q("CREATE TABLE \""++Table++"\" ("++string:join(ColsSQL, ",") ++ table_create_primary_key(Cols) ++ ")", Context),
    ok.


table_create_primary_key([]) -> [];
table_create_primary_key([#column_def{name=id, type="serial"}|_]) -> ", primary key(id)";
table_create_primary_key([#column_def{name=N, primary_key=true}|_]) -> ", primary key(" ++ z_convert:to_list(N) ++ ")";
table_create_primary_key([_|Cols]) -> table_create_primary_key(Cols).

ensure_table_create_cols([], Acc) ->
    lists:reverse(Acc);
ensure_table_create_cols([C|Cols], Acc) ->
    M = lists:flatten([$", atom_to_list(C#column_def.name), $", 32, column_spec(C)]),
    ensure_table_create_cols(Cols, [M|Acc]).

column_spec(#column_def{type=Type, length=Length, is_nullable=Nullable, default=Default}) ->
    L = case Length of
            undefined -> [];
            _ -> [$(, integer_to_list(Length), $)]
        end,
    N = column_spec_nullable(Nullable),
    D = column_spec_default(Default),
    lists:flatten([Type, L, N, D]).

column_spec_nullable(true) -> "";
column_spec_nullable(false) -> " not null".

column_spec_default(undefined) -> "";
column_spec_default(Default) -> [" DEFAULT ", Default].


%% @doc Check if a name is a valid SQL table name. Crashes when invalid
%% @spec assert_table_name(String) -> true
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
