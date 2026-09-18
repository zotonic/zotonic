%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc SPARQL parser and query routines.
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

-module(z_sparql).

-export([
    parse/1,
    search/2,
    search/3,
    search/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_rdf/include/zotonic_rdf.hrl").

%% @doc Parse a SPARQL query, return the parse tree or an error.
%% The parse tree can be used to generate a SQL query.
-spec parse(Sparql) -> {ok, ParsedQuery} | {error, Reason} when
    Sparql :: string() | binary(),
    ParsedQuery :: term(),
    Reason :: term().
parse(Sparql) ->
    case z_sparql_scanner:scan(Sparql) of
        {ok, Tokens} ->
            z_sparql_parser:parse(Tokens);
        {error, _} = Error ->
            Error
    end.

%% @doc Do a SPARQL SELECT query with default page length.
-spec search(Sparql, Context) -> {ok, Result} | {error, Reason} when
    Sparql :: string() | binary(),
    Context :: z:context(),
    Result :: #search_result{},
    Reason :: term().
search(Sparql, Context) ->
    OffsetLimit = {1, z_search:default_pagelen(Context)},
    search(Sparql, OffsetLimit, Context).

%% @doc Do a SPARQL SELECT query with arguments, or with a defined offset and limit.
-spec search(Sparql, ArgumentsOrOffsetLimit, Context) -> {ok, Result} | {error, Reason} when
    Sparql :: string() | binary(),
    ArgumentsOrOffsetLimit :: map() | z_search:search_offset(),
    Context :: z:context(),
    Result :: #search_result{},
    Reason :: term().
search(Sparql, Arguments, Context) when is_map(Arguments) ->
    OffsetLimit = {1, z_search:default_pagelen(Context)},
    search(Sparql, Arguments, OffsetLimit, Context);
search(Sparql, OffsetLimit, Context) ->
    search(Sparql, #{}, OffsetLimit, Context).

%% @doc Do a SPARQL SELECT query with pre-bound arguments and pagination.
-spec search(Sparql, Arguments, OffsetLimit, Context) -> {ok, Result} | {error, Reason} when
    Sparql :: string() | binary(),
    Arguments :: map(),
    OffsetLimit :: z_search:search_offset(),
    Context :: z:context(),
    Result :: #search_result{},
    Reason :: term().
search(Sparql, Arguments, OffsetLimit, Context) ->
    case parse(Sparql) of
        {ok, ParsedQuery} ->
            search_parsed(ParsedQuery, Arguments, OffsetLimit, Context);
        {error, _} = Error ->
            Error
    end.

search_parsed(ParsedQuery, Arguments, OffsetLimit, Context) ->
    case z_sparql_sql:to_sql_term(ParsedQuery, Arguments, Context) of
        {ok, SqlTerms} ->
            SearchSql = z_search_terms:combine(SqlTerms, Context),
            {ok, z_search:search_result(SearchSql, OffsetLimit, Context)};
        {error, _} = Error ->
            Error
    end.
