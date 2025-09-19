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
    parse/1
]).

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
