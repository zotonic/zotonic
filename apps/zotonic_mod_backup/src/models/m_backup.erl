%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2026 Marc Worrell
%% @doc Model for database and files backups
%% @end

%% Copyright 2015-2026 Marc Worrell
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
-moduledoc("
Model for backup configuration and operational status, including backup permissions, listing, directory, encryption settings, and active backup state.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/admin_panel/...` | Return backup admin-panel configuration/state map as provided by `backup_config:admin_panel/1`. |
| `get` | `/allow_manual_backup/...` | Return whether the current site/user is allowed to start manual backups (`backup_config:allow_manual_backup/1`; requires `use` on `mod_backup`). |
| `get` | `/allow_backup_download/...` | Return whether backup file download is allowed (`backup_config:allow_backup_download/1`; requires `use` on `mod_backup`). |
| `get` | `/daily_dump/...` | Return configured daily dump setting as binary value (`backup_config:daily_dump/1`; requires `use` on `mod_backup`). |
| `get` | `/encrypt_backups/...` | Return whether backup archives are configured to be encrypted (`backup_config:encrypt_backups/1`; requires `use` on `mod_backup`). |
| `get` | `/has_encrypt_password/...` | Return whether a non-empty backup encryption password is configured (`backup_config:encrypt_password/1`; requires `use` on `mod_backup`). |
| `get` | `/list_backups/...` | Return available backup files/entries from `mod_backup:list_backups/1` (requires `use` on `mod_backup`). |
| `get` | `/is_backup_in_progress/...` | Return whether a backup job is currently running (`mod_backup:backup_in_progress/1`; requires `use` on `mod_backup`). |
| `get` | `/directory/...` | Return the resolved backup directory path used for backup files (`backup_create:dir/1`; requires `use` on `mod_backup`). |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-behaviour(zotonic_model).

-export([
    m_get/3
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"admin_panel">> | Rest ], _Msg, Context) ->
    {ok, {backup_config:admin_panel(Context), Rest}};
m_get([ <<"allow_manual_backup">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            {ok, {backup_config:allow_manual_backup(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"allow_backup_download">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            {ok, {backup_config:allow_backup_download(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"daily_dump">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            DailyDump = z_convert:to_binary(backup_config:daily_dump(Context)),
            {ok, {DailyDump, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"encrypt_backups">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true -> {ok, {backup_config:encrypt_backups(Context), Rest}};
        false -> {error, eacces}
    end;
m_get([ <<"has_encrypt_password">> | Rest ], _Msg, Context) ->
    case z_acl:is_allowed(use, mod_backup, Context) of
        true ->
            PW = backup_config:encrypt_password(Context),
            {ok, {is_binary(PW) andalso size(PW) > 0, Rest}};
        false ->
            {error, eacces}
    end;
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
        true -> {ok, {backup_create:dir(Context), Rest}};
        false -> {error, eacces}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.
