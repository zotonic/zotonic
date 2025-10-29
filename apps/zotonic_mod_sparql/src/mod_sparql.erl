%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc SPARQL support for Zotonic.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(mod_sparql).
-moduledoc("
The mod_sparql module adds support to use the SPARQL query language for accessing Zotonic data.
").

-mod_title("SPARQL").
-mod_description("SPARQL for Zotonic data.").
-mod_provides([]).
-mod_depends([]).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_sparql_mapping/2
]).

% For testing
-export([
    find_column/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/sparql.hrl").


observe_sparql_mapping(#sparql_mapping{ ns_prefix = Prefix, ns = NS, predicate = Predicate }, Context) ->
    % Could be a predicate or a property (of some table)
    case find_predicate(NS, Predicate, Context) of
        undefined ->
            % Check known rsc and pivot properties - use z_rdf_props
            case find_column(Prefix, NS, Predicate, Context) of
                {ok, _} = Ok -> Ok;
                undefined -> undefined
            end;
        RId ->
            % Known predicate - will be an edge
            IsReversed = z_convert:to_bool(m_rsc:p_no_acl(RId, <<"reversed">>, Context)),
            {ok, {edge, RId, IsReversed}}
    end.

find_column(<<"rdfs">>, _NS, <<"subClassOf">>, _Context) ->
    {ok, subclass};
find_column(Prefix, NS, Predicate, Context) ->
    PredicateName = predicate_name(Prefix, NS, Predicate),
    case z_rdf_props:mapping(PredicateName) of
        <<"category_id">> ->
            {ok, category};
        undefined ->
            % Just check if we know the basename, irrespective of namespace
            find_table_column(camelcase_to_underscore(Predicate), Context);
        Column ->
            find_table_column(camelcase_to_underscore(Column), Context)
    end.

find_table_column(Predicate, Context) ->
    case is_protected(Predicate) of
        true -> undefined;
        false ->
            case z_db:column(<<"rsc">>, Predicate, Context) of
                {ok, #column_def{} = Def} -> {ok, {column, <<"rsc">>, Predicate, column_type(Def)}};
                {error, enoent} -> find_table_column_1(Predicate, Context)
            end
    end.

%% @doc Map a facet or pivot property to its database column. If the column
%% is not defined then assume it as a path in the resource JSON properties.
find_table_column_1(<<"facet.", Facet/binary>>, Context) ->
    case search_facet:lookup_facet(Facet, Context) of
        {ok, #{
            table := Table,
            column := Column,
            type := Type
        }} ->
            {ok, {column, Table, Column, Type}};
        {error, _} ->
            undefined
    end;
find_table_column_1(<<"pivot.", Pivot/binary>> = Predicate, Context) ->
    case binary:split(Pivot, <<".">>, [global]) of
        [ Name ] ->
            find_table_column_2(<<"rsc">>, <<"pivot_", Name/binary>>, Predicate, Context);
        [ PivotTable, Name ] ->
            find_table_column_2(<<"pivot_", PivotTable/binary>>, Name, Predicate, Context);
        _ ->
            json_property(Predicate)
    end;
find_table_column_1(Predicate, _Context) ->
    json_property(Predicate).

find_table_column_2(Table, Column, Predicate, Context) ->
    case z_db:column(Table, Column, Context) of
        {ok, #column_def{} = Def} -> {ok, {column, Table, Column, column_type(Def)}};
        {error, enoent} -> json_property(Predicate)
    end.

column_type(#column_def{ name = Name, is_array = true }) ->
    case maybe_id(Name) of
        id -> ids;
        _ -> list
    end;
column_type(#column_def{ type = Type, name = Name }) ->
    case Type of
        <<"text">> -> text;
        <<"character varying">> -> text;
        <<"integer">> -> maybe_id(Name);
        <<"bigint">> -> integer;
        <<"smallint">> -> integer;
        <<"serial">> -> integer;
        <<"bigserial">> -> integer;
        <<"smallserial">> -> integer;
        <<"numeric">> -> float;
        <<"decimal">> -> float;
        <<"float">> -> float;
        <<"double">> -> float;
        <<"boolean">> -> boolean;
        <<"timestamp", _/binary>> -> datetime;
        <<"datetime", _/binary>> -> datetime;
        <<"date", _/binary>> -> datetime;
        <<"tsvector">> -> fts;
        <<"ARRAY">> -> list;
        <<"array">> -> list;
        _ -> text
    end.

%% @doc Expand a dot-separated property name to a JSON path.
json_property(Predicate) ->
    Path = binary:split(Predicate, <<".">>, [global]),
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, Path, type_from_name(lists:last(Path))}}.

maybe_id(Name) ->
    case z_props:property_name_type_hint(Name) of
        id -> id;
        _ -> integer
    end.

type_from_name(Name) ->
    case z_props:property_name_type_hint(Name) of
        undefined -> text;
        Type -> Type
    end.

%% @doc Expand something like FooBar to the usual zotonic lowercased
%% property name foo_bar.
camelcase_to_underscore(Name) ->
    camelcase_to_underscore(Name, <<>>).

camelcase_to_underscore(<<C, Rest/binary>>, <<>>) when C >= $A, C =< $Z ->
    camelcase_to_underscore(Rest, <<(C + 32)>>);
camelcase_to_underscore(<<$., C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    camelcase_to_underscore(Rest, <<Acc/binary, $., (C + 32)>>);
camelcase_to_underscore(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    camelcase_to_underscore(Rest, <<Acc/binary, $_, (C + 32)>>);
camelcase_to_underscore(<<C/utf8, Rest/binary>>, Acc) ->
    camelcase_to_underscore(Rest, <<Acc/binary, C/utf8>>);
camelcase_to_underscore(<<>>, Acc) ->
    Acc.

predicate_name(<<>>, _NS, Predicate) ->
    Predicate;
predicate_name(NS, NS, Predicate) ->
    <<NS/binary, Predicate/binary>>;
predicate_name(Prefix, _NS, Predicate) ->
    <<Prefix/binary, $:, Predicate/binary>>.

is_protected(<<"props">>) -> true;
is_protected(<<"props_json">>) -> true;
is_protected(_) -> false.

find_predicate(NS, Predicate, Context) ->
    case find_id(NS, Predicate, Context) of
        undefined ->
            undefined;
        RId ->
            case m_rsc:is_a(RId, predicate, Context) of
                true -> RId;
                false -> undefined
            end
    end.

find_id(NS, Predicate, Context) ->
    URI = <<NS/binary, Predicate/binary>>,
    case m_rsc:rid(URI, Context) of
        undefined ->
            m_rsc:rid(Predicate, Context);
        RId ->
            RId
    end.
