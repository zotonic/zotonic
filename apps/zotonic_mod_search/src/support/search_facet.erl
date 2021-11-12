%% @copyright 2021 Driebit BV
%% @doc Faceted search using a facet.tpl for definition and a
%% postgresql table for searches.

%% Copyright 2021 Driebit BV
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

-module(search_facet).

-record(facet_def, {
    name :: binary(),
    block :: atom(),
    type :: facet_type(),
    is_range :: boolean()
    }).

-type facet_type() :: text
                    | ft
                    | int
                    | float
                    | dt
                    | id
                    | list.
-type facet_def() :: #facet_def{}.


-export([
    pivot_rsc/2,
    ensure_table/1,
    is_table_ok/1,
    template_facets/1,
    facet_table/1,
    create_table/1,
    recreate_table/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Pivot a resource, fill the facet table.
-spec pivot_rsc( m_rsc:resource_id(), z:context() ) -> ok | {error, term()}.
pivot_rsc(Id, Context) ->
    % 0. Fetch blocks in pivot.tpl
    % 1. Render all blocks
    % 2. Prepare possible ft columns
    % 3. Map types for columns
    % 4. Insert or update facet table
    ok.


%% @doc Ensure that the facet table is correct, if not then drop the existing
%% table and request a pivot of all resources to fill the table.
-spec ensure_table(z:context()) -> ok | {error, term()}.
ensure_table(Context) ->
    case is_table_ok(Context) of
        true ->
            ok;
        false ->
            case recreate_table(Context) of
                ok ->
                    z_pivot_rsc:queue_all(Context),
                    ok;
                {error, _} = Error ->
                    Error
            end
    end.


%% @doc Check if the current table is compatible with the facets in pivot.tpl
-spec is_table_ok(z:context()) -> boolean().
is_table_ok(Context) ->
    DbCols = lists:filter(
        fun
            (#column_def{ name = id }) -> false;
            (#column_def{ name = _ }) -> true
        end,
        z_db:columns(search_facet, Context)),
    case facet_table(Context) of
        {ok, {TplCols, _}} when length(DbCols) =:= length(TplCols) ->
            lists:all(
                fun(#column_def{ name = Name, type = Type }) ->
                    is_type(DbCols, Name, Type)
                end,
                TplCols);
        {ok, _} ->
            false;
        {error, _} ->
            false
    end.

is_type([], _Name, _Type) ->
    false;
is_type([ #column_def{ name = Name, type = T } | _ ], Name, Type) ->
    z_convert:to_binary(T) =:= Type;
is_type([ _ | Cols ], Name, Type) ->
    is_type(Cols, Name, Type).


%% @doc Recreate the facet table by first dropping it.
-spec recreate_table( z:context() ) -> ok | {error, term()}.
recreate_table(Context) ->
    z_db:q("drop table if exists search_facet cascade", Context),
    z_db:flush(Context),
    create_table(Context).


%% @doc Generate the table for the facet pivoting. The definition of the
%% columns is derived from the facet.tpl template.
-spec create_table( z:context() ) -> ok | {error, term()}.
create_table(Context) ->
    case facet_table(Context) of
        {ok, {Cols, Idxs}} ->
            Cols1 = [
                #column_def{
                    name = id,
                    type = "integer",
                    length = undefined,
                    is_nullable = false,
                    default = undefined,
                    primary_key = true,
                    unique = true
                }
                | Cols
            ],
            case z_db:create_table(search_facet, Cols1, Context) of
                ok ->
                    [] = z_db:q(
                        "ALTER TABLE search_facet ADD CONSTRAINT fk_facet_id FOREIGN KEY (id)
                         REFERENCES rsc (id)
                         ON UPDATE CASCADE ON DELETE CASCADE",
                        Context),
                    lists:foreach(
                        fun(Idx) ->
                            [] = z_db:q(Idx, Context)
                        end,
                        Idxs),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


%% @doc Generate the face table definition from the facets.
-spec facet_table(z:context()) -> {ok, term()} | {error, term()}.
facet_table(Context) ->
    case template_facets(Context) of
        {ok, Facets} ->
            {Cs, Is} = lists:foldl(
                fun(Facet, {ColAcc, IdxAcc}) ->
                    Col = facet_to_column(Facet),
                    Idx = facet_to_index(Facet),
                    {[Col|ColAcc], [Idx|IdxAcc]}
                end,
                {[], []},
                Facets),
            Cs1 = lists:sort( lists:flatten(Cs) ),
            Is1 = lists:sort( lists:flatten(Is) ),
            {ok, {Cs1, Is1}};
        {error, _} = Error ->
            Error
    end.

facet_to_column(#facet_def{
        name = Name,
        type = ft
    }) ->
    [
        #column_def{
            name = binary_to_atom(<<"f_", Name/binary>>, utf8),
            type = col_type(text),
            length = col_length(text),
            is_nullable = true,
            default = undefined,
            primary_key = false,
            unique = false
        },
        #column_def{
            name = binary_to_atom(<<"ft_", Name/binary>>, utf8),
            type = <<"text">>,
            length = undefined,
            is_nullable = true,
            default = undefined,
            primary_key = false,
            unique = false
        }
    ];
facet_to_column(#facet_def{
        name = Name,
        type = list
    }) ->
    #column_def{
        name = binary_to_atom(<<"f_", Name/binary>>, utf8),
        type = <<"text">>,
        length = undefined,
        is_nullable = true,
        is_array = true,
        default = undefined,
        primary_key = false,
        unique = false
    };
facet_to_column(#facet_def{
        name = Name,
        type = Type
    }) ->
    #column_def{
        name = binary_to_atom(<<"f_", Name/binary>>, utf8),
        type = col_type(Type),
        length = col_length(Type),
        is_nullable = true,
        default = undefined,
        primary_key = false,
        unique = false
    }.


col_type(text) -> <<"character varying">>;
col_type(int) -> <<"integer">>;
col_type(float) -> <<"float">>;
col_type(boolean) -> <<"boolean">>;
col_type(dt) -> <<"timestamp with time zone">>;
col_type(id) -> <<"integer">>.

col_length(text) -> 80;
col_length(int) -> undefined;
col_length(float) -> undefined;
col_length(boolean) -> undefined;
col_length(dt) -> undefined;
col_length(id) -> undefined.


facet_to_index(#facet_def{
        name = Name,
        type = ft
    }) ->
    [
        <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
          "ON search_facet(f_", Name/binary, ")">>,

        <<"CREATE INDEX search_facet_ft_", Name/binary, "_key ",
           "ON search_facet USING gin (ft_", Name/binary, " public.gin_trgm_ops)">>
    ];
facet_to_index(#facet_def{
        name = Name,
        type = list
    }) ->
    <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
       "ON search_facet USING gin (f_", Name/binary, ")">>;
facet_to_index(#facet_def{
        name = Name
    }) ->
    <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
      "ON search_facet(f_", Name/binary, ")">>.


%% @doc Fetch all facet definitions from the current facet template.
-spec template_facets( z:context() ) -> {ok, [ facet_def() ]}.
template_facets(Context) ->
    case z_template:blocks(<<"pivot/facet.tpl">>, #{}, Context) of
        {ok, Blocks} ->
            Facets = lists:map(fun block_to_facet/1, Blocks),
            case find_duplicate_names(Facets) of
                [] -> {ok, Facets};
                Names ->
                    lager:error("Blocks with duplicate basenames in facet.tpl: ~p", [ Names ]),
                    {error, duplicate_blocks}
            end;
        {error, _} = Error ->
            Error
    end.

find_duplicate_names(Facets) ->
    Counts = lists:foldl(
        fun(#facet_def{ name = N }, Acc) ->
            Acc#{ N => maps:get(N, Acc, 0) + 1 }
        end,
        #{},
        Facets),
    L = maps:to_list(Counts),
    lists:filtermap(
        fun
            ({_, 1}) -> false;
            ({N, _}) -> {true, N}
        end,
        L).

block_to_facet(Block) ->
    case atom_to_binary(Block, utf8) of
        <<"is_", _/binary>> = Name ->
            #facet_def{
                name = Name,
                block = Block,
                type = boolean,
                is_range = false
            };
        B ->
            {Type, Name, IsRange} = block_type(B),
            #facet_def{
                name = Name,
                block = Block,
                type = Type,
                is_range = IsRange
            }
    end.

block_type(B) ->
    case lists:reverse(binary:split(B, <<"_">>, [ global ])) of

        [ <<"id">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {id, n(Rs), true};
        [ <<"int">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {int, n(Rs), true};
        [ <<"float">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {float, n(Rs), true};
        [ <<"dt">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {dt, n(Rs), true};

        [ <<"id">> | Rs ] when length(Rs) >= 1 ->
            {id, n(Rs), false};
        [ <<"int">> | Rs ] when length(Rs) >= 1 ->
            {int, n(Rs), false};
        [ <<"float">> | Rs ] when length(Rs) >= 1 ->
            {float, n(Rs), false};
        [ <<"dt">> | Rs ] when length(Rs) >= 1 ->
            {dt, n(Rs), false};

        [ <<"ft">> | Rs ] when length(Rs) >= 1 ->
            {ft, n(Rs), false};

        [ <<"list">> | Rs ] when length(Rs) >= 1 ->
            {list, n(Rs), false};

        _ ->
            {text, B, false}
    end.

n(Rs) ->
    iolist_to_binary(lists:join($_, lists:reverse(Rs))).
