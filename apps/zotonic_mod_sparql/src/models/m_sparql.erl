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
-moduledoc("
Run SPARQL SELECT queries through Zotonic's normal search pipeline.

The payload must contain `query` and can contain a map of named pre-bound
variables in `args`, plus normal search paging fields. Results are filtered by
the resource ACLs of the calling context. This model is intentionally readable
where normal searches are readable; it does not grant access beyond `m.search`.

Available Model API Paths
-------------------------

| Method | Path | Description |
| --- | --- | --- |
| `get` | `/` | Run a SPARQL query and return a normal search result. |
| `get` | `/paged` | Run a SPARQL query and return a paged search result. |
| `get` | `/count` | Run a SPARQL query and return the result count. |

The HTTP API accepts a JSON payload on `/api/model/sparql/get`. POST can be
used with the model `get` topic to send structured JSON arguments.
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
