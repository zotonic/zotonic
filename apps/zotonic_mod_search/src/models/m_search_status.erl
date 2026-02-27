%% @doc Search model, used as an interface to the search functions of modules etc.
%% @copyright 2024-2026 Marc Worrell
%% @end

%% Copyright 2024-2026 Marc Worrell
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

-module(m_search_status).
-moduledoc("
Model for search subsystem status, including facet table health checks and facet definition metadata.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/facets/is_table_ok/...` | Return whether facet storage tables/indexes are present and healthy. |
| `get` | `/facets/definition/...` | Return configured/template facet definition map used by search faceting. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

-export([
    m_get/3
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"facets">>, <<"is_table_ok">> | Rest ], _Msg, Context) ->
    {ok, {search_facet:is_table_ok(Context), Rest}};
m_get([ <<"facets">>, <<"definition">> | Rest ], _Msg, Context) ->
    case search_facet:template_facets_map(Context) of
        {ok, L} ->
            {ok, {L, Rest}};
        {error, _} = Error ->
            Error
    end.
