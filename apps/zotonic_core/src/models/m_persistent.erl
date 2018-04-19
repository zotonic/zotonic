%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Model for accessing the persistent variables from a template.

%% Copyright 2009-2017 Marc Worrell
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

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/2,

    get/2,
    get_props/2,
    put/3
]).

-include_lib("zotonic.hrl").

-type id() :: binary() | string().
-define(T_PERSISTENT, "persistent").


%% @doc Fetch the value for the key from a model source
-spec m_get( list(), z:context() ) -> {term(), list()}.
m_get([ persistent_id | Rest ], Context) ->
    {z_context:persistent_id(Context), Rest};
m_get([ Key | Rest ], Context) ->
    {z_context:get_persistent(Key, Context), Rest};
m_get(Vs, _Context) ->
    lager:error("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {undefined, []}.


%% @doc Select full row by persistent id.
-spec get(Id :: undefined | id(), #context{}) -> Props :: list().
get(undefined, _Context) ->
    [];
get(Id, Context) ->
    z_db:q1("SELECT props FROM " ++ ?T_PERSISTENT ++ " WHERE id = $1", [Id], Context).


%% @doc Get only stored (persistent) props for session by id.
-spec get_props(Id :: undefined | id(), Context :: #context{}) -> Props :: list() | 'undefined'.
get_props(undefined, _Context) ->
    undefined;
get_props(Id, Context) ->
    z_db:q1("SELECT props FROM " ++ ?T_PERSISTENT ++ " WHERE id = $1", [Id], Context).


%% @doc Save new persistent session data.
-spec put(Id :: undefined | id(), Props :: list(), #context{}) -> ok | {error, undefined}.
put(undefined, _Props, _Context) ->
    {error, undefined};
put(Id, Props, Context) ->
    z_db:q(
        "UPDATE " ++ ?T_PERSISTENT ++ " SET props = $2, modified = now() WHERE id = $1",
        [Id, {term, Props}],
        Context
    ) == 1
    orelse z_db:q(
        "INSERT INTO " ++ ?T_PERSISTENT ++ " (id, props) VALUES ($1, $2)",
        [Id, ?DB_PROPS(Props)],
        Context
    ),
    ok.

