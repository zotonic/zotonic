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
-mod_provides([ sparql ]).
-mod_depends([ mod_search, mod_rdf ]).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_sparql_mapping/2,
    observe_search_query/2,
    observe_search_query_parse/2
]).

% For testing
-export([
    find_column/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/sparql.hrl").


observe_search_query(#search_query{ name = <<"sparql">>, args = Args }, Context) ->
    case sparql_args(Args) of
        {ok, Query, Arguments} ->
            case compile_sparql(Query, Arguments, Context) of
                {ok, SearchTerms} -> SearchTerms;
                {error, Reason} -> throw(Reason)
            end;
        {error, Reason} ->
            throw(Reason)
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_search_query_parse(#search_query_parse{
        query = Query,
        query_type = undefined,
        arguments = Arguments
    }, Context) ->
    case is_sparql_query(Query) of
        true -> parse_query_resource(Query, Arguments, Context);
        false -> undefined
    end;
observe_search_query_parse(#search_query_parse{
        query = Query,
        query_type = <<"sparql">>,
        arguments = Arguments
    }, Context) ->
    parse_query_resource(Query, Arguments, Context);
observe_search_query_parse(#search_query_parse{}, _Context) ->
    undefined.

sparql_args(Args) when is_map(Args) ->
    case search_arg(<<"query">>, Args, undefined) of
        Query when is_binary(Query); is_list(Query) ->
            case search_arg(<<"args">>, Args, #{}) of
                Arguments when is_map(Arguments) -> {ok, Query, Arguments};
                _ -> {error, invalid_arguments}
            end;
        undefined ->
            {error, missing_query};
        _ ->
            {error, invalid_query}
    end;
sparql_args(_Args) ->
    {error, invalid_arguments}.

search_arg(Key, Args, Default) ->
    case maps:find(Key, Args) of
        {ok, Value} -> Value;
        error -> z_search:lookup_qarg_value(Key, Args, Default)
    end.

compile_sparql(Query, Arguments, Context) ->
    case z_sparql:parse(Query) of
        {ok, ParsedQuery} ->
            case z_sparql_sql:to_sql_term(ParsedQuery, Arguments, Context) of
                {ok, SqlTerms} -> {ok, #search_sql_terms{ terms = SqlTerms }};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

parse_query_resource(Query, Arguments, Context) ->
    case z_sparql:parse(Query) of
        {ok, ParsedQuery} ->
            parse_query_resource_1(ParsedQuery, Arguments, Context);
        {error, Reason} ->
            query_parse_error(Reason, Context)
    end.

parse_query_resource_1(ParsedQuery, Arguments, Context) ->
    case z_sparql_plan:to_query_plan(ParsedQuery, Arguments, Context) of
        {ok, #{ root := Root, select := [Root] } = Plan} ->
            case z_sparql_sql:query_plan_to_sql(Plan, Context) of
                {ok, SqlTerms} ->
                    {ok, (query_descriptor(Context))#{
                        parsed => Plan,
                        search_terms => #search_sql_terms{ terms = SqlTerms }
                    }};
                {error, Reason} ->
                    query_parse_error(Reason, Context)
            end;
        {ok, _Plan} ->
            query_parse_error(single_resource_projection_required, Context);
        {error, Reason} ->
            query_parse_error(Reason, Context)
    end.

query_parse_error(Reason, Context) ->
    {error, {query_parse, maps:merge(
        query_descriptor(Context),
        sparql_error(Reason))}}.

sparql_error(Reason) when is_binary(Reason) ->
    case re:run(
        Reason,
        <<"^undefined:([0-9]+):([0-9]+):\\s*(.*)$">>,
        [{capture, [1, 2, 3], binary}, dotall])
    of
        {match, [Line, Column, Message]} ->
            #{
                reason => Reason,
                message => Message,
                line => binary_to_integer(Line),
                column => binary_to_integer(Column)
            };
        nomatch ->
            #{ reason => Reason, message => Reason }
    end;
sparql_error({{_Source, Line, Column}, z_sparql_parser, Message} = Reason) ->
    #{
        reason => Reason,
        message => parser_error_message(Message),
        line => Line,
        column => Column
    };
sparql_error(single_resource_projection_required = Reason) ->
    #{
        reason => Reason,
        message => <<"The query must select exactly one resource variable.">>
    };
sparql_error(Reason) ->
    #{
        reason => Reason,
        message => unicode:characters_to_binary(io_lib:format("~tp", [Reason]))
    }.

parser_error_message(Message) ->
    MessageBin = iolist_to_binary(z_sparql_parser:format_error(Message)),
    case re:run(
        MessageBin,
        <<"^syntax error before: <<\\\"(.*)\\\">>$">>,
        [{capture, [1], binary}])
    of
        {match, [Token]} ->
            <<"Syntax error before \"", Token/binary, "\".">>;
        nomatch ->
            MessageBin
    end.

query_descriptor(Context) ->
    #{
        query_type => <<"sparql">>,
        query_type_label => ?__("SPARQL query", Context),
        is_live => false,
        show_parsed => false
    }.

is_sparql_query(Query) ->
    Pattern = <<
        "^(?:\\s|#[^\\r\\n]*(?:\\r?\\n|$))*"
        "(?:BASE\\s|PREFIX\\s|SELECT(?=\\s|\\*|\\?|\\())"
    >>,
    re:run(Query, Pattern, [caseless, {capture, none}]) =:= match.


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
            search_column := SearchColumn,
            type := Type
        }} when Type =:= fulltext; Type =:= fts ->
            {ok, {search_column, Table, Column, SearchColumn, Type}};
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

maybe_id(Name) when is_atom(Name) ->
    maybe_id(atom_to_binary(Name, utf8));
maybe_id(Name) when is_binary(Name) ->
    case z_props:property_name_type_hint(Name) of
        id -> id;
        _ -> integer
    end.

type_from_name(Name) when is_atom(Name) ->
    type_from_name(atom_to_binary(Name, utf8));
type_from_name(Name) ->
    case z_props:property_name_type_hint(Name) of
        undefined -> text;
        Type -> property_type(Type)
    end.

property_type(id) -> id;
property_type(int) -> integer;
property_type(float) -> float;
property_type(bool) -> boolean;
property_type(datetime) -> datetime;
property_type(list) -> list;
property_type(uri) -> uri;
property_type(binary) -> text;
property_type(text) -> text;
property_type(html) -> text;
property_type(language) -> text;
property_type(email) -> text;
property_type(unsafe) -> text.

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
