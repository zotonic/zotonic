%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-20
%%
%% @doc Model for accessing the persistent variables from a template.

%% Copyright 2009 Marc Worrell
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

-module(m_persistent).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    get/2,
    get_props/2,
    put/3
]).

-include_lib("zotonic.hrl").

-type id() :: binary() | string().
-define(T_PERSISTENT, "persistent").

%% @doc Fetch the value for the key from a model source
-spec m_find_value(Key :: id(), Source :: #m{}, #context{}) -> term().
m_find_value(persistent_id, #m{value=undefined}, Context) ->
    z_context:persistent_id( Context);
m_find_value(Key, #m{value=undefined}, Context) ->
    z_context:get_persistent(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
-spec m_to_list(Source :: #m{}, #context{}) -> list().
m_to_list(#m{value=undefined}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
-spec m_value(Source :: #m{}, #context{}) -> term().
m_value(#m{value=undefined}, _Context) ->
    undefined.


%% @doc Select full row by persistent id.
-spec get(Id :: id(), #context{}) -> Props :: list().
get(Id, Context) ->
    z_db:q1("SELECT props FROM " ++ ?T_PERSISTENT ++ " WHERE id = $1", [Id], Context).


%% @doc Get only stored (persistent) props for session by id.
-spec get_props(Id :: id(), Context :: #context{}) -> Props :: list() | 'undefined'.
get_props(Id, Context) ->
    z_db:q1("SELECT props FROM " ++ ?T_PERSISTENT ++ " WHERE id = $1", [Id], Context).


%% @doc Save new persistent session data.
-spec put(Id :: id(), Props :: list(), #context{}) -> ok.
put(Id, Props, Context) ->
    z_db:q("UPDATE " ++ ?T_PERSISTENT ++ " SET props = $2, modified = now() WHERE id = $1", [Id, Props], Context) == 1
    orelse z_db:q("INSERT INTO " ++ ?T_PERSISTENT ++ " (id, props) VALUES ($1, $2)", [Id, Props], Context),
    ok.

