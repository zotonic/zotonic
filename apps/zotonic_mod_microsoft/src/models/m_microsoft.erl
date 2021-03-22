%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc Model for Microsoft services

%% Copyright 2011 Maas-Maarten Zeeman
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

-module(m_microsoft).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3,
    is_useauth/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"useauth">> | Rest ], _OptMsg, Context) ->
    {ok, {is_useauth(Context), Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.


-spec is_useauth( z:context() ) -> boolean().
is_useauth(Context) ->
    case m_config:get_value(mod_microsoft, appid, Context) of
        undefined -> false;
        <<>> -> false;
        _ -> m_config:get_boolean(mod_microsoft, useauth, Context)
    end.
