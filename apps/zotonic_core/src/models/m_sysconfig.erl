%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%%
%% @doc Get system configurarion keys.

%% Copyright 2016 Marc Worrell
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

-module(m_sysconfig).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Key | Rest ], _Msg, Context) ->
    try
        KeyAtom = erlang:binary_to_existing_atom(Key, utf8),
        case z_acl:is_admin(Context) of
            true -> {ok, {z_config:get(KeyAtom), Rest}};
            false -> {error, eacces}
        end
    catch
        error:badarg ->
            {ok, {undefined, Rest}}
    end;
m_get(Vs, _Msg, _Context) ->
    lager:info("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.

