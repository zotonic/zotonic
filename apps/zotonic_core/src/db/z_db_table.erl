%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Functions to create, update or drop database tables.
%% @end

%% Copyright 2009-2024 Marc Worrell
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
-module(z_db_table).

-export([
    create_table/3,
    alter_table/3,
    drop_table/2,
    table_exists/2,
    table_exists/3,
    columns/2,
    columns/3,
    column/3,
    column/4,
    assert_table_name/1,
    assert_database_name/1,
    assert_schema_name/1,
    quoted_table_name/1
    ]).

-include_lib("zotonic.hrl").
% -include_lib("epgsql/include/epgsql.hrl").


%% @doc Ensure that a table with the given columns exists, if the table exists then
%% add, modify or drop columns.  The 'id' (with type serial) column _must_ be defined
%% when creating the table.
-spec create_table(Name, Cols, Context) -> ok | {error, Reason} when
    Name :: z_db:table_name(),
    Cols :: [ #column_def{} ],
    Context :: z:context(),
    Reason :: term().
create_table(Table, Cols, Context) ->
    Cols1 = lists:map(fun norm/1, Cols),
    {_Schema, _Tab, QTab} = quoted_table_name(Table),
    ColsSQL = ensure_table_create_cols(Cols1, []),
    case z_db:squery(iolist_to_binary([
            "CREATE TABLE ", QTab, " (",
                lists:join($,, ColsSQL),
                table_create_primary_key(Cols1),
            ")"
        ]), Context)
    of
        {ok, _, _} ->
            z_db:flush(Context),
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Error creating database table">>,
                result => error,
                reason => Reason,
                table => Table
            }),
            Error
    end.

%% @doc Alter a table so that it matches the given column definitions. If the table doesn't
%% exist then it is created. RESTRICTIONS: does NOT change the primary key and unique
%% constraint of existing columns. If the table doesn't exist then it is created.
%% Be careful when adding columns that are not nullable, if the table contains data then
%% adding those columns will fail.
-spec alter_table(Name, Cols, Context) -> ok | {error, Reason} when
    Name :: z_db:table_name(),
    Cols :: [ #column_def{} ],
    Context :: z:context(),
    Reason :: term().
alter_table(Table, Cols, Context) ->
    case table_exists(Table, Context) of
        true ->
            alter_table_1(Table, Cols, Context);
        false ->
            create_table(Table, Cols, Context)
    end.

%% @doc Make sure that a table is dropped, only when the table exists
-spec drop_table(z_db:table_name(), z:context()) -> ok.
drop_table(Table, Context) ->
    {_Schema, _Tab, QTab} = quoted_table_name(Table),
    case table_exists(Table, Context) of
        true ->
            z_db:q("drop table if exists " ++ QTab, Context),
            z_db:flush(Context),
            ok;
        false ->
            ok
    end.

%% @doc Check the information schema if a certain table exists in the context database.
-spec table_exists(z_db:table_name(), z:context()) -> boolean().
table_exists(Table, Context) ->
    {Schema, Tab, _QTab} = quoted_table_name(Table),
    table_exists(Schema, Tab, Context).

-spec table_exists( z_db:schema_name(), z_db:table_name(), z:context() ) -> boolean().
table_exists(Schema, Table, Context) ->
    Options = z_db_pool:get_database_options(Context),
    Db = proplists:get_value(dbdatabase, Options),
    Schema1 = case Schema of
        default -> proplists:get_value(dbschema, Options);
        S -> S
    end,
    case z_db:q1("select count(*)
                from information_schema.tables
                where table_catalog = $1
                  and table_name = $2
                  and table_schema = $3
                  and table_type = 'BASE TABLE'", [Db, Table, Schema1], Context) of
        1 -> true;
        0 -> false
    end.


%% @doc Return a property list with all columns of the table. (example: [{id,int4,modifier},...])
-spec columns(z_db:table_name(), z:context()) -> list( #column_def{} ).
columns(Table, Context) ->
    {Schema, Table1, _QTab} = quoted_table_name(Table),
    columns(Schema, Table1, Context).

-spec columns(z_db:schema_name(), z_db:table_name(), z:context()) -> list( #column_def{} ).
columns(Schema, Table, Context) when is_binary(Table) ->
    columns(Schema, binary_to_list(Table), Context);
columns(Schema, Table, Context) when is_atom(Table) ->
    columns(Schema, atom_to_list(Table), Context);
columns(Schema, Table, Context) when is_list(Table) ->
    assert_table_name(Table),
    Options = z_db_pool:get_database_options(Context),
    Db = proplists:get_value(dbdatabase, Options),
    Schema1 = case Schema of
        default -> proplists:get_value(dbschema, Options);
        S when is_list(S) -> S;
        S when is_binary(S) -> S
    end,
    case z_depcache:get({columns, Db, Schema1, Table}, Context) of
        {ok, Cols} ->
            Cols;
        _ ->
            Cols = z_db:q("
                        select column_name, data_type, character_maximum_length,
                               is_nullable, column_default, udt_name
                        from information_schema.columns
                        where table_catalog = $1
                          and table_schema = $2
                          and table_name = $3
                        order by ordinal_position", [Db, Schema1, Table], Context),
            Cols1 = [ columns1(Col) || Col <- Cols ],
            z_depcache:set({columns, Db, Schema1, Table}, Cols1, ?YEAR, [{database, Db}], Context),
            Cols1
    end.

%% @doc Fetch the column definition for the given column.
-spec column( z_db:table_name(), z_db:column_name(), z:context()) ->
        {ok, #column_def{}} | {error, enoent}.
column(Table, Column, Context) ->
    {Schema, Table1, _QTab} = quoted_table_name(Table),
    column(Schema, Table1, Column, Context).

%% @doc Fetch the column definition for the given column.
-spec column( z_db:schema_name(), z_db:table_name(), z_db:column_name(), z:context()) ->
        {ok, #column_def{}} | {error, enoent}.
column(Schema, Table, Column0, Context) ->
    Columns = columns(Schema, Table, Context),
    Column = to_existing_atom(Column0),
    case lists:filter(
        fun
            (#column_def{ name = Name }) when Name =:= Column -> true;
            (_) -> false
        end,
        Columns)
    of
        [] -> {error, enoent};
        [ #column_def{} = Col ] -> {ok, Col}
    end.


%% @doc Quote a table name so that it is safe to use in SQL queries.
-spec quoted_table_name( z_db:table_name() ) -> {default | string(), string(), string()}.
quoted_table_name(TableName) ->
    assert_table_name(TableName),
    case binary:split(z_convert:to_binary(TableName), <<".">>, [ global ]) of
        [ Schema, Table ] ->
            QTab = binary_to_list(
                iolist_to_binary([
                    $", Schema, $", $., $", Table, $"
                ])),
            {binary_to_list(Schema), binary_to_list(Table), QTab};
        [ Table ] ->
            QTab = binary_to_list(
                iolist_to_binary([
                    $", Table, $"
                ])),
            {default, binary_to_list(Table), QTab}
    end.


%% @doc Check if a name is a valid SQL table name. Crashes when invalid
-spec assert_table_name( z_db:table_name() ) -> true.
assert_table_name(A) when is_atom(A) ->
    assert_table_name1(atom_to_list(A));
assert_table_name([ C | _ ] = Table) when C =/= $. ->
    assert_table_name1(Table);
assert_table_name(<< C,  _/binary>> = Table) when C =/= $. ->
    assert_table_name1b(Table).

assert_table_name1([]) -> true;
assert_table_name1([$_|T]) -> assert_table_name1(T);
assert_table_name1([$.|T]) -> assert_table_name1(T);
assert_table_name1([H|T]) when (H >= $a andalso H =< $z) ->
    assert_table_name1(T);
assert_table_name1([H|T]) when (H >= $0 andalso H =< $9) ->
    assert_table_name1(T).

assert_table_name1b(<<>>) -> true;
assert_table_name1b(<<$_, T/binary>>) -> assert_table_name1b(T);
assert_table_name1b(<<$., T/binary>>) -> assert_table_name1b(T);
assert_table_name1b(<<H, T/binary>>) when (H >= $a andalso H =< $z) ->
    assert_table_name1b(T);
assert_table_name1b(<<H, T/binary>>) when (H >= $0 andalso H =< $9) ->
    assert_table_name1b(T).

%% @doc Check if a name is a valid SQL database name. Crashes when invalid
-spec assert_database_name( string() ) -> true.
assert_database_name([]) -> true;
assert_database_name([$.|T]) -> assert_database_name(T);
assert_database_name([$_|T]) -> assert_database_name(T);
assert_database_name([H|T]) when (H >= $a andalso H =< $z) ->
    assert_database_name(T);
assert_database_name([H|T]) when (H >= $0 andalso H =< $9) ->
    assert_database_name(T).

%% @doc Check if a name is a valid SQL schema name. Crashes when invalid
-spec assert_schema_name( string() ) -> true.
assert_schema_name([]) -> true;
assert_schema_name([$_|T]) -> assert_schema_name(T);
assert_schema_name([H|T]) when (H >= $a andalso H =< $z) ->
    assert_schema_name(T);
assert_schema_name([H|T]) when (H >= $0 andalso H =< $9) ->
    assert_schema_name(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alter_table_1(Table, NewCols0, Context) ->
    NewCols = lists:map(fun norm/1, NewCols0),
    CurrentCols = lists:map(fun norm/1, columns(Table, Context)),
    DropCols = lists:filter(
        fun(Col) ->
            not has_column(Col, NewCols)
        end,
        CurrentCols),
    RemainingColumns = CurrentCols -- DropCols,
    AlterCols = lists:filter(
        fun(Col) ->
            changed_column(Col, NewCols)
        end,
        RemainingColumns),
    AddCols = lists:filter(
        fun(Col) ->
            find_column(Col#column_def.name, RemainingColumns) =:= false
        end,
        NewCols),
    DropSql = drop_col_statements(DropCols),
    AlterSql = alter_col_statements(AlterCols, NewCols),
    AddSql = add_col_statements(AddCols),
    case iolist_to_binary(lists:join($,, DropSql ++ AlterSql ++ AddSql)) of
        <<>> ->
            ok;
        Alter ->
            Sql = iolist_to_binary([
                "ALTER TABLE \"", z_convert:to_binary(Table), "\" ", Alter
            ]),
            case z_db:squery(Sql, Context) of
                {ok, _, _} ->
                    z_db:flush(Context),
                    ok;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        in => zotonic_core,
                        text => <<"Error altering database table">>,
                        result => error,
                        reason => Reason,
                        table => Table
                    }),
                    Error
            end
    end.

drop_col_statements(DropCols) ->
    lists:map(
        fun(#column_def{ name = N }) ->
            ["DROP COLUMN \"", atom_to_list(N), "\" CASCADE"]
        end,
        DropCols).

alter_col_statements(AlterCols, NewCols) ->
    lists:map(fun(C) -> alter_col_statement(C, find_column(C#column_def.name, NewCols)) end, AlterCols).

alter_col_statement(
        #column_def{ type = OldT, length = OldLen, is_nullable = OldNullable, default = OldDefault },
        #column_def{ type = NewT, length = NewLen, is_nullable = NewNullable, is_array = NewArray, default = NewDefault, name = Name }) ->
    NewT1 = case NewT of
        <<"serial">> -> <<"integer">>;
        <<"bigserial">> -> <<"bigint">>;
        T -> T
    end,
    Qs = [
        if
            NewT =/= OldT orelse NewLen =/= OldLen ->
                [
                    "ALTER COLUMN \"", atom_to_list(Name), "\" TYPE ",
                    NewT1,
                    if
                        is_integer(NewLen)  -> [ "(", integer_to_list(NewLen), ")" ];
                        true -> []
                    end,
                    column_spec_array(NewArray)
                ];
            true ->
                []
        end,
        if
            not NewNullable, OldNullable ->
                [ "ALTER COLUMN \"", atom_to_list(Name), "\" SET NOT NULL" ];
            NewNullable, not OldNullable ->
                [ "ALTER COLUMN \"", atom_to_list(Name), "\" DROP NOT NULL" ];
            true ->
                []
        end,
        if
            NewDefault =/= OldDefault, NewDefault =:= undefined ->
                [ "ALTER COLUMN \"", atom_to_list(Name), "\" SET DEFAULT NULL" ];
            NewDefault =/= OldDefault, NewDefault =/= undefined ->
                [ "ALTER COLUMN \"", atom_to_list(Name), "\" SET DEFAULT ", NewDefault ];
            true ->
                []
        end
    ],
    lists:join($,, [ Q || Q <- Qs, Q =/= [] ]).

add_col_statements(AddCols) ->
    lists:map(fun(C) -> add_col_statement(C) end, AddCols).

add_col_statement(#column_def{ name = N } = C) ->
    [ "ADD COLUMN \"", atom_to_list(N), "\" ", column_spec(C) ].

find_column(_, []) -> false;
find_column(A, [ #column_def{ name = B } = Col | _ ]) when A =:= B -> Col;
find_column(A, [ _ | Cs ]) -> find_column(A, Cs).

has_column(#column_def{}, []) ->
    false;
has_column(#column_def{ name = A, is_array = AA },[ #column_def{ name = B, is_array = BA } | _ ]) when A =:= B, AA =/= BA ->
    false;
has_column(#column_def{ name = A, type = X }, [ #column_def{ name = B, type = Y } | _ ]) when A =:= B ->
    is_compat_type(X, Y);
has_column(#column_def{} = Col, [ _ | Defs ]) ->
    has_column(Col, Defs).

changed_column(#column_def{}, []) ->
    false;
changed_column(#column_def{ name = A } = CA, [ #column_def{ name = B } = CB | _ ]) when A =:= B ->
    CA#column_def.type =/= CB#column_def.type
    orelse CA#column_def.length =/= CB#column_def.length
    orelse CA#column_def.is_nullable =/= CB#column_def.is_nullable
    orelse CA#column_def.default =/= CB#column_def.default;
changed_column(#column_def{} = Col, [ _ | Defs ]) ->
    changed_column(Col, Defs).

is_compat_type(A, B) -> base_type(A) =:= base_type(B).

base_type(<<"character varying">>) -> text;
base_type(<<"character">>) -> text;
base_type(<<"text">>) -> text;
base_type(<<"bytea">>) -> binary;
base_type(<<"integer">>) -> numeric;
base_type(<<"smallint">>) -> numeric;
base_type(<<"bigint">>) -> numeric;
base_type(<<"serial">>) -> serial;
base_type(<<"bigserial">>) -> serial;
base_type(<<"smallserial">>) -> serial;
base_type(<<"float">>) -> numeric;
base_type(<<"real">>) -> numeric;
base_type(<<"double", _/binary>>) -> numeric;
base_type(<<"decimal">>) -> numeric;
base_type(<<"money">>) -> numeric;
base_type(<<"timestamp", _/binary>>) -> date;
base_type(<<"date", _/binary>>) -> date;
base_type(T) -> T.


norm(#column_def{ type = Type, default = Default } = Col) ->
    Col#column_def{
        type = norm_type(z_string:to_lower(z_convert:to_binary(Type))),
        default = maybe_binary(Default)
    }.

norm_type(<<"varchar">>) -> <<"character varying">>;
norm_type(<<"char">>) -> <<"character">>;
norm_type(<<"bpchar">>) -> <<"character">>;
norm_type(<<"int">>) -> <<"integer">>;
norm_type(<<"float">>) -> <<"real">>;
norm_type(<<"double">>) -> <<"double precision">>;
norm_type(<<"timestamp">>) -> <<"timestamp with time zone">>;
norm_type(T) -> T.

maybe_binary(undefined) -> undefined;
maybe_binary(V) -> z_convert:to_binary(V).

columns1({<<"id">>, <<"integer">>, undefined, Nullable, <<"nextval(", _/binary>>, _UdtName}) ->
    #column_def{
        name = id,
        type = <<"serial">>,
        length = undefined,
        is_nullable = z_convert:to_bool(Nullable),
        default = undefined
    };
columns1({<<"id">>, <<"bigint">>, undefined, Nullable, <<"nextval(", _/binary>>, _UdtName}) ->
    #column_def{
        name = id,
        type = <<"bigserial">>,
        length = undefined,
        is_nullable = z_convert:to_bool(Nullable),
        default = undefined
    };
columns1({Name, <<"ARRAY">>, _MaxLength, Nullable, Default, UdtName}) ->
    % @todo Better method to derive the type of the array elements.
    #column_def{
        name = z_convert:to_atom(Name),
        type = case UdtName of
            <<"_text">> -> <<"text">>;
            <<"_varchar">> -> <<"character varying">>;
            <<"_int", _/binary>> -> <<"integer">>;
            _ -> UdtName
        end,
        length = undefined,
        is_array = true,
        is_nullable = z_convert:to_bool(Nullable),
        default = column_default(Default)
    };
columns1({Name, Type, MaxLength, Nullable, Default, _UdtName}) ->
    #column_def{
        name = z_convert:to_atom(Name),
        type = z_convert:to_binary(Type),
        length = MaxLength,
        is_nullable = z_convert:to_bool(Nullable),
        default = column_default(Default)
    }.

column_default(undefined) -> undefined;
column_default(<<"nextval(", _/binary>>) -> undefined;
column_default(Default) -> z_convert:to_binary(Default).

table_create_primary_key([]) -> [];
table_create_primary_key([#column_def{ name = id, type = <<"serial">> }|_]) -> ", primary key(id)";
table_create_primary_key([#column_def{ name = id, type = <<"bigserial">> }|_]) -> ", primary key(id)";
table_create_primary_key([#column_def{ name = N, primary_key = true }|_]) -> [ ", primary key(", z_convert:to_list(N), ")" ];
table_create_primary_key([_|Cols]) -> table_create_primary_key(Cols).

ensure_table_create_cols([], Acc) ->
    lists:reverse(Acc);
ensure_table_create_cols([C|Cols], Acc) ->
    M = [ $", z_convert:to_binary(C#column_def.name), $", 32, column_spec(C) ],
    ensure_table_create_cols(Cols, [M|Acc]).

column_spec(#column_def{
        type = Type,
        length = Length,
        is_nullable = Nullable,
        is_array = IsArray,
        default = Default,
        unique = Unique
    }) ->
    L = case Length of
            undefined -> [];
            _ -> [$(, integer_to_binary(Length), $)]
        end,
    A = column_spec_array(IsArray),
    N = column_spec_nullable(Nullable),
    D = column_spec_default(Default),
    U = column_spec_unique(Unique),
    [Type, L, A, N, D, U].

column_spec_array(true) -> "[]";
column_spec_array(false) -> "".

column_spec_nullable(true) -> "";
column_spec_nullable(false) -> " not null".

column_spec_default(undefined) -> "";
column_spec_default(Default) -> [" DEFAULT ", z_convert:to_binary(Default) ].

column_spec_unique(false) -> "";
column_spec_unique(true) -> " UNIQUE".


to_existing_atom(C) ->
    try
        to_existing_atom_1(C)
    catch
        error:badarg ->
            'ERROR'
    end.

to_existing_atom_1(C) when is_binary(C) ->
    binary_to_existing_atom(C, utf8);
to_existing_atom_1(C) when is_list(C) ->
    list_to_existing_atom(C);
to_existing_atom_1(C) when is_atom(C) ->
    C.
