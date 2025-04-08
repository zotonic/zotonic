%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Fetch the config of the filestore from the global config or
%% the site config. It is possible to set a system wide config, which
%% is then used for all sites. This enables backup of all sites running
%% on a single Zotonic installation.
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

-module(filestore_config).

-export([
    is_config_locked/0,
    service/1,
    s3url/1,
    s3key/1,
    s3secret/1,
    is_upload_enabled/1,
    is_local_keep/1,
    delete_interval/1
]).

%% Default delete interval matches with the default number of weekly backups
%% by mod_backup.
-define(DEFAULT_DELETE_INTERVAL, <<"5 weeks">>).

%% @doc Check if the site config is locked, and we should only follow the
%% global config.
is_config_locked() ->
    z_convert:to_bool(get_app_config(is_config_locked)).

%% @doc Get the service for the file storage. If the config is saved then this
%% is determined by the protocol of the s3url.
%% Possible values: "s3", "ftp", "webdav" or "" when disabled.
-spec service(z:context()) -> binary().
service(Context) ->
    case get_binary(service, Context) of
        <<>> -> <<"s3">>;
        Service -> Service
    end.

%% @doc Get the S3 URL from the config. The protocol defines if s3, ftp, or
%% webdav is used.
%%
%% Possible protocols for the URL:
%% - "http:" or "https:" for s3
%% - "ftp:" or "ftps:" for ftp
%% - "webdav:" or "webdavs:" for WebDAV
%% - "dav:" or "davs:" for WebDAV
-spec s3url(z:context()) -> binary().
s3url(Context) ->
    get_binary(s3url, Context).

%% @doc Get the S3 key (or FTP/WebDAV username) from the config.
-spec s3key(z:context()) -> binary().
s3key(Context) ->
    get_binary(s3key, Context).

%% @doc Get the S3 secret (or FTP/WebDAV password) from the config.
-spec s3secret(z:context()) -> binary().
s3secret(Context) ->
    get_binary(s3secret, Context).

%% @doc Check if the uploading files is enabled. If set then any newly
%% uploaded or created (preview) files is queued for uploading to the
%% remote filestore.
-spec is_upload_enabled(z:context()) -> boolean().
is_upload_enabled(Context) ->
    get_boolean(is_upload_enabled, Context).

%% @doc Check if after upload a local file should be kept or deleted.
%% If set to "true" then the remote filestore is acting as a backup
%% of all local files.
-spec is_local_keep(z:context()) -> boolean().
is_local_keep(Context) ->
    get_boolean(is_local_keep, Context).

%% @doc Get the delay interval after we delete files from the remote
%% filestore.
%%
%% Possible values:
%% - "false" no deletion, keep all files on the remote
%% - an integer: the number of seconds to wait before deleting the file
%% - "N days" the number of days to wait before deleting the file
%% - "N weeks" the number of weeks to wait before deleting the file
%% - "N months" the number of months to wait before deleting the file
%%
%% Defaults "0", which means immediate deletion.
-spec delete_interval(Context) -> binary() when
    Context :: z:context().
delete_interval(Context) ->
    case z_convert:to_binary(get_value(delete_interval, Context)) of
        <<>> -> ?DEFAULT_DELETE_INTERVAL;
        Interval -> Interval
    end.


get_boolean(Key, Context) ->
    z_convert:to_bool(get_value(Key, Context)).

get_binary(Key, Context) ->
    z_convert:to_binary(get_value(Key, Context)).

get_value(Key, Context) ->
    case is_config_locked() of
        true ->
            get_app_config(Key, Context);
        false ->
            case m_config:get_value(mod_filestore, Key, Context) of
                undefined -> get_app_config(Key, Context);
                <<>> -> get_app_config(Key, Context);
                Config -> Config
            end
    end.

get_app_config(s3url, Context) ->
    Url = get_app_config(s3url),
    case z_convert:to_binary(Url) of
        <<>> -> <<>>;
        UrlBin ->
            Site = z_convert:to_binary(z_context:site(Context)),
            case binary:match(<<"{{site}}">>, UrlBin) of
                nomatch ->
                    case binary:last(UrlBin) of
                        $/ -> <<UrlBin/binary, Site/binary, $/>>;
                        _ -> <<UrlBin/binary, $/, Site/binary, $/>>
                    end;
                {_, _} ->
                    binary:replace(UrlBin, <<"{{site}}">>, Site)
            end
    end;
get_app_config(Key, _Context) ->
    get_app_config(Key).

get_app_config(Key) ->
    case application:get_env(zotonic_mod_filestore, Key) of
        {ok, Value} -> Value;
        undefined -> undefined
    end.
