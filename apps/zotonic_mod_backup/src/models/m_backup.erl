%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2022 Marc Worrell
%% @doc Model for database and files backups

%% Copyright 2015-2022 Marc Worrell
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

-module(m_backup).

-behaviour(zotonic_model).

-export([
    m_get/3
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"admin_panel">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_backup, admin_panel, Context), Rest}};
m_get([ <<"daily_dump">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_backup, daily_dump, Context), Rest}};
m_get([ <<"encrypt_backups">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_backup, encrypt_backups, Context), Rest}};
m_get([ <<"has_encrypt_password">> | Rest ], _Msg, Context) ->
    PW = m_config:get_value(mod_backup, backup_encrypt_password, Context),
    {ok, {is_binary(PW) andalso size(PW) > 0, Rest}};
m_get([ <<"list_backups">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true -> {ok, {mod_backup:list_backups(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"is_backup_in_progress">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true -> {ok, {mod_backup:backup_in_progress(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"directory">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true -> {ok, {mod_backup:dir(Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

