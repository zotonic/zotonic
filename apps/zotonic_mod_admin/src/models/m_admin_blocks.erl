%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Zotonic: admin blocks model and interface
%% @end

%% Copyright 2013-2026 Marc Worrell
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

-module(m_admin_blocks).
-moduledoc("
Model for listing editable content blocks for a resource in the admin interface.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/list/+id/...` | Return the sorted admin edit block definitions available for resource `+id`, collected via `#admin_edit_blocks` notifications. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    m_get/3
]).


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"list">>, Id | Rest ], _Msg, Context) ->
    RscId = m_rsc:rid(Id, Context),
    List = lists:sort( z_notifier:foldr(#admin_edit_blocks{ id = RscId }, [], Context) ),
    {ok, {List, Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.
