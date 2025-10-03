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

-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/sparql.hrl").


observe_sparql_mapping(#sparql_mapping{ ns_prefix = Prefix, ns = NS, predicate = Predicate }, Context) ->
    % Could be a predicate or a property (of some table)
    case find_predicate(NS, Predicate, Context) of
        undefined ->
            % Check known rsc and pivot properties - use z_rdf_props
            case find_column(Prefix, Predicate, Context) of
                {ok, _} = Ok -> Ok;
                undefined -> undefined
            end;
        RId ->
            % Known predicate - will be an edge
            {ok, {edge, RId}}
    end.

find_column(<<"zotonic">>, Predicate, Context) ->
    case is_protected(Predicate) of
        true -> undefined;
        false ->
            case z_db:column(<<"rsc">>, Predicate, Context) of
                {ok, _Column} -> {ok, {column, <<"rsc">>, Predicate}};
                {error, enoent} ->
                    % TODO:
                    % 1. Check facet
                    % 2. Check pivots
                    % 3. JSON selector in props_json
                    undefined
            end
    end;
find_column(_NS, _Predicate, _Context) ->
    % TODO: check known property mappings from z_rdf_props.
    undefined.


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
