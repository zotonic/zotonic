%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc SPARQL support for Zotonic.  See also z_rdf_props in code and
%% the zotonic_rdf app.
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

-module(mod_rdf).
-moduledoc("
Support for mapping between Zotonic and RDF data.
").

-mod_title("RDF").
-mod_description("RDF mappings.").
-mod_provides([]).
-mod_depends([]).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_rdf_ns/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @todo Check this and if ok then move to zotonic_rdf app.
-define(NS_ZOTONIC, <<"http://zotonic.net/predicate/">>).

observe_rdf_ns(#rdf_ns{ ns = ?NS_ZOTONIC }, _Context) ->
    {ok, <<"zotonic">>};
observe_rdf_ns(#rdf_ns{ ns = NS }, Context) ->
    case NS =:= m_rsc:uri_prefix(Context) of
        true ->
            {ok, <<"site">>};
        false ->
            observe_rdf_ns_1(NS)
    end.

observe_rdf_ns_1(NS) ->
    case zotonic_rdf:ns_compact(NS) of
        Prefix when Prefix =:= NS ->
            undefined;
        Prefix ->
            case binary:last(Prefix) of
                $: -> {ok, binary:part(Prefix, 0, byte_size(Prefix) - 1)};
                _ -> {ok, Prefix}
            end
    end.
