%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Model API for SPARQL SELECT queries.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_sparql).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model", "search_and_discovery",
        "query", "sparql", "rdf_and_linked_data", "pagination",
        "resource", "api"
    ]
}).
-moduledoc("
Run SPARQL SELECT queries through Zotonic's normal search pipeline.

This model delegates to `m_search` using the `sparql` query type provided by
`mod_sparql`. Paging, search result handling, and resource visibility checks use
the calling context, just as for other searches.

## Query payload

The payload is a map with these fields:

| Field | Description |
| --- | --- |
| `query` | Required SPARQL SELECT text. |
| `args` | Optional map of named pre-bound variables, without the leading `?`. |
| `page` | Search result page. |
| `pagelen` | Number of results per page. |

For example, from Erlang:

```erlang
Args = #{
    <<\"query\">> => <<\"SELECT ?article WHERE { ?article :author ?author }\">>,
    <<\"args\">> => #{<<\"author\">> => {rsc, AuthorId}},
    <<\"page\">> => 1,
    <<\"pagelen\">> => 20
},
{ok, Result} = m_sparql:search(Args, Context).
```

`m_search:search(<<\"sparql\">>, Args, Context)` is the equivalent search call.
`search/2` returns `{ok, #search_result{}}` or `{error, Reason}`. Selecting only
the root resource returns resource IDs. Additional projected values use search
result rows, with the root resource ID included for ungrouped queries.

## Named arguments

Arguments accept text, booleans, integers, floats, dates, datetimes, and
`undefined`. Use `{rsc, ResourceReference}` for a resource or `{iri, Iri}` for an
IRI. Resource references can be IDs, names, or resolvable resource URIs.

JSON callers can send explicitly typed values in the nested `args` map:

```json
{
  \"author\": {\"type\": \"resource\", \"value\": 123},
  \"homepage\": {\"type\": \"iri\", \"value\": \"https://example.test/\"},
  \"since\": {\"type\": \"datetime\", \"value\": \"2026-08-20T10:00:00Z\"},
  \"day\": {\"type\": \"date\", \"value\": \"2026-08-20\"}
}
```

Use these arguments instead of inserting caller-supplied values into query text.
See `mod_sparql` for default namespaces, predicate mappings, and query limits.

## Available Model API Paths

| Method | Path | Description |
| --- | --- | --- |
| `get` | `/` | Run a query and return a normal search result. |
| `get` | `/paged` | Run a query and return a paged search result. |
| `get` | `/count` | Run a query and return the result count. |

The HTTP model API accepts a JSON payload on `/api/model/sparql/get`; POST can
be used with the model `get` topic to send structured arguments. The paged and
count paths are `/api/model/sparql/get/paged` and `/api/model/sparql/get/count`.
These are Zotonic model endpoints, not the SPARQL protocol endpoint.

Use `page` and `pagelen` for paging; query LIMIT and OFFSET are not supported.
Stored query resources have the additional restriction that they select exactly
the root resource variable. Direct model queries may project other expressions.
").

-behaviour(zotonic_model).

-export([
    m_get/3,
    search/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


-spec m_get(list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([], Msg, Context) ->
    m_search:m_get([ <<"sparql">> ], Msg, Context);
m_get([ <<"paged">> | Rest ], Msg, Context) ->
    m_search:m_get([ <<"paged">>, <<"sparql">> | Rest ], Msg, Context);
m_get([ <<"count">> | Rest ], Msg, Context) ->
    m_search:m_get([ <<"count">>, <<"sparql">> | Rest ], Msg, Context);
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

-spec search(Args, Context) -> {ok, #search_result{}} | {error, Reason}
    when
        Args :: map(),
        Context :: z:context(),
        Reason :: term().
search(Args, Context) when is_map(Args) ->
    m_search:search(<<"sparql">>, Args, Context);
search(_Args, _Context) ->
    {error, invalid_arguments}.
