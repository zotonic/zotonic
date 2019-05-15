%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Model for mod_admin_identity

%% Copyright 2017 Marc Worrell
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

-module(m_admin_identity).

-behaviour (zotonic_model).

-export([
    m_get/3
]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ password_regex | Rest ], _Msg, Context) ->
    {ok, {m_config:get_value(mod_admin_identity, password_regex, Context), Rest}};
m_get([ new_user_category | Rest ], _Msg, Context) ->
    Cat = case m_config:get_value(mod_admin_identity, new_user_category, Context) of
        undefined -> person;
        <<>> -> person;
        C -> C
    end,
    {ok, {Cat, Rest}};
m_get(Vs, _Msg, _Context) ->
    lager:debug("Unknown ~p lookup: ~p", [?MODULE, Vs]),
    {error, unknown_path}.
