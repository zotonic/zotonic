%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Fetch the config for the backup module. The configuration
%% can be done (and locked) in the Zotonic system config or per site.
%% @end

%% Copyright 2025 Marc Worrell
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

-module(backup_config).

-export([
    is_config_locked/0,
    is_filestore_enabled/1,
    allow_manual_backup/1,
    allow_backup_download/1,
    daily_dump/1,
    admin_panel/1,
    encrypt_password/1,
    encrypt_backups/1,
    retention_months/1,
    user_retention_days/1,
    deleted_user_retention_days/1
]).

-include_lib("zotonic_core/include/zotonic_file.hrl").
-include("../backup.hrl").


% Number of months we keep revisions
-define(BACKUP_REVISION_RETENTION_MONTHS, 18).
% Number of days we keep user revisions
-define(BACKUP_USER_REVISION_RETENTION_DAYS, 90).
% Number of days we keep user deletions
-define(BACKUP_USER_DELETION_RETENTION_DAYS, 30).


%% @doc Check if the site config is locked, and we should only follow the
%% global config for some of the configurations.
is_config_locked() ->
    z_convert:to_bool(get_app_config(is_config_locked)).

%% @doc Check if the cloud storage of (backup) files is enabled.
-spec is_filestore_enabled(z:context()) -> boolean().
is_filestore_enabled(Context) ->
    TestPath = <<"backup/backup.json">>,
    case z_notifier:first(#filestore_credentials_lookup{ path = TestPath}, Context) of
        {ok, #filestore_credentials{}} -> true;
        {error, _} -> false;
        undefined -> false
    end.

%% @doc Check if a daily backup needs to be made. If so it could include the files
%% or be only a backup of the database.
-spec daily_dump(Context) -> ?BACKUP_DB | ?BACKUP_ALL | ?BACKUP_NONE when
    Context :: z:context().
daily_dump(Context) ->
    case get_binary(daily_dump, Context) of
        <<"1">> ->
            case is_filestore_enabled(Context) of
                true -> ?BACKUP_DB;
                false -> ?BACKUP_ALL
            end;
        <<"2">> -> ?BACKUP_DB;
        _ -> ?BACKUP_NONE
    end.

%% @doc Check if the admin panel for the revisions should be shown. If the site
%% config is not set then a fallback is done to the Zotonic config.
-spec admin_panel(Context) -> boolean() when
    Context :: z:context().
admin_panel(Context) ->
    case m_config:get_value(mod_filestore, admin_panel, Context) of
        undefined ->
            case application:get_env(zotonic_mod_backup, admin_panel) of
                {ok, V} -> z_convert:to_bool(V);
                undefined -> false
            end;
        V -> z_convert:to_bool(V)
    end.

%% @doc Check if a manually initiated backup is allowed. Defaults to true.
-spec allow_manual_backup(Context) -> boolean() when
    Context :: z:context().
allow_manual_backup(Context) ->
    case get_value(allow_manual_backup, Context) of
        undefined -> true;
        <<>> -> true;
        "" -> true;
        V -> z_convert:to_bool(V)
    end.

%% @doc Check if a manually initiated backup is allowed. Defaults to true.
-spec allow_backup_download(Context) -> boolean() when
    Context :: z:context().
allow_backup_download(Context) ->
    case get_value(allow_backup_download, Context) of
        undefined -> true;
        <<>> -> true;
        "" -> true;
        V -> z_convert:to_bool(V)
    end.

%% @doc Check if backups should be encrypted. The password should also be set.
-spec encrypt_backups(Context) -> boolean() when
    Context :: z:context().
encrypt_backups(Context) ->
    get_boolean(encrypt_backups, Context).

%% @doc Return the password used to encrypt backups. If a backup is encrypted
%% is regulated by encrypt_backups/1
-spec encrypt_password(Context) -> binary() when
    Context :: z:context().
encrypt_password(Context) ->
    get_binary(backup_encrypt_password, Context).


%% @doc Return the number of months we keep revisions. Defaults to 18 months.
%% Uses the configuration mod_backup.revision_retention_months
-spec retention_months(Context) -> Months when
    Context :: z:context(),
    Months :: pos_integer().
retention_months(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, revision_retention_months, Context)) of
        undefined ->
            ?BACKUP_REVISION_RETENTION_MONTHS;
        N ->
            max(N, 1)
    end.

%% @doc Return the number of days we keep user resource's revisions.
%% Defaults to 3 months (90 days).
%% Uses the configuration mod_backup.user_revision_retention_days
-spec user_retention_days(Context) -> Days when
    Context :: z:context(),
    Days :: pos_integer().
user_retention_days(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, user_revision_retention_days, Context)) of
        undefined ->
            ?BACKUP_USER_REVISION_RETENTION_DAYS;
        N ->
            max(N, 1)
    end.

%% @doc Return the number of days we keep user resource's revisions of deleted users.
%% Defaults to 1 month (30 days), which is also the maximum allowed.
%% Uses the configuration mod_backup.user_deletion_retention_days
-spec deleted_user_retention_days(Context) -> Days when
    Context :: z:context(),
    Days :: pos_integer().
deleted_user_retention_days(Context) ->
    case z_convert:to_integer(m_config:get_value(mod_backup, user_deletion_retention_days, Context)) of
        undefined ->
            ?BACKUP_USER_DELETION_RETENTION_DAYS;
        N when N =< 0 ->
            ?BACKUP_USER_DELETION_RETENTION_DAYS;
        N ->
            min(30, N)
    end.



get_binary(Key, Context) ->
    z_convert:to_binary(get_value(Key, Context)).

get_boolean(Key, Context) ->
    z_convert:to_bool(get_value(Key, Context)).

get_value(Key, Context) ->
    case is_config_locked() of
        true ->
            get_app_config(Key);
        false ->
            case m_config:get_value(mod_backup, Key, Context) of
                undefined -> get_app_config(Key);
                <<>> -> get_app_config(Key);
                Config -> Config
            end
    end.

get_app_config(Key) ->
    case application:get_env(zotonic_mod_backup, Key) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.
