%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc RDF namespace resolution for Zotonic data mappings.
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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "module", "rdf_and_linked_data",
        "semantic_web", "namespace", "mapping", "resource",
        "sparql", "structured_data"
    ]
}).
-moduledoc("
Resolve RDF namespaces used when mapping Zotonic data to RDF vocabulary terms.

The module observes the `rdf_ns` notification and returns a compact prefix for
a known namespace. It recognizes these mappings:

| Namespace | Prefix |
| --- | --- |
| `http://zotonic.net/predicate/` | `zotonic` |
| The local resource URI namespace from `m_rsc:uri_prefix/1` | `site` |

The Zotonic prefix and namespace are defined centrally in `zotonic_rdf`.
Other namespaces are resolved through `zotonic_rdf:ns_compact/1`. A recognized
namespace returns `{ok, Prefix}` without a trailing colon. An unknown namespace
returns `undefined`, allowing other notification observers to provide a mapping.
The local namespace follows the site's resource URI configuration rather than a
hard-coded host or path.

## Related APIs

`mod_sparql` depends on this module when resolving namespaces in SELECT queries.
It handles predicate-to-property and predicate-to-edge mappings through the
`sparql_mapping` notification, using `z_rdf_props` for known property mappings.

The core `m_rdf` model provides JSON-LD summary maps for resources, including
Schema.org properties and translated variants. The `zotonic_rdf` library supplies
shared vocabulary and namespace utilities. The `zotonic_jsonld` and
`zotonic_turtle` applications parse and generate documents with those standard
prefixes; JSON-LD contexts are expanded before normalization. See those APIs for
serialization options and supported formats. Enabling this module does not install
an RDF import service or a SPARQL protocol endpoint.
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
