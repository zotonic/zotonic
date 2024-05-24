%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Synchronous low level routines for accessing the filestore.
%% @end

%% Copyright 2022 Marc Worrell
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

-module(filestore_request).

-export([
    upload/4,
    download/3,
    delete/2,

    do_upload/3,
    do_download/2,
    do_delete/1,

    filezmod/1,

    download_cb/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

upload(LocalFile, RemoteFile, undefined, Context) ->
    Mime = z_media_identify:guess_mime(LocalFile),
    upload(LocalFile, RemoteFile, Mime, Context);
upload(LocalFile, RemoteFile, Mime, Context) ->
    case z_notifier:first(#filestore_credentials_lookup{path=RemoteFile}, Context) of
        {ok, #filestore_credentials{} = Cred} ->
            handle_upload(Cred, LocalFile, Mime);
        undefined ->
            {error, no_credentials}
    end.

download(LocalFile, RemoteFile, Context) ->
    case z_notifier:first(#filestore_credentials_lookup{path=RemoteFile}, Context) of
        {ok, #filestore_credentials{} = Cred} ->
            do_download(Cred, LocalFile);
        undefined ->
            {error, no_credentials}
    end.

delete(RemoteFile, Context) ->
    case z_notifier:first(#filestore_credentials_lookup{path=RemoteFile}, Context) of
        {ok, #filestore_credentials{} = Cred} ->
            do_delete(Cred);
        undefined ->
            {error, no_credentials}
    end.


handle_upload(Cred, LocalFile, Mime) ->
    case file:read_file_info(LocalFile) of
        {ok, #file_info{type=regular, size=0}} ->
            ?LOG_NOTICE(#{
                text => <<"Not uploading empty file">>,
                in => zotonic_mod_filestore,
                src => LocalFile
            }),
            {error, empty};
        {ok, #file_info{type=regular, size=Size}} ->
            do_upload(Cred, {filename, Size, LocalFile}, Mime);
        {ok, #file_info{type=Type}} ->
            ?LOG_ERROR(#{
                text => <<"Not uploading file because of file type">>,
                in => zotonic_mod_filestore,
                result => error,
                reason => filetype,
                src => LocalFile,
                type => Type
            }),
            {error, filetype};
        {error, enoent} ->
            ?LOG_INFO(#{
                text => <<"Not uploading file because it is not found">>,
                in => zotonic_mod_filestore,
                result => error,
                reason => enoent,
                src => LocalFile
            }),
            {error, enoent}
    end.

-spec do_upload(CredLoc, Data, Mime) -> Result when
    CredLoc :: #filestore_credentials{},
    Data :: ftpfilez:put_data(),
    Mime :: binary(),
    Result :: ftpfilez:sync_reply().
do_upload(#filestore_credentials{service=Service, location=Location, credentials=Cred}, Data, undefined) ->
    Mod = filezmod(Service),
    Mod:put(Cred, Location, Data, []);
do_upload(#filestore_credentials{service=Service, location=Location, credentials=Cred}, Data, Mime) ->
    Mod = filezmod(Service),
    Mod:put(Cred, Location, Data, [ {content_type, Mime} ]).

-spec do_download(CredLoc, LocalFile) -> Result when
    CredLoc :: #filestore_credentials{},
    LocalFile :: file:filename_all(),
    Result :: ok | {error, term()}.
do_download(#filestore_credentials{service=Service, location=Location, credentials=Cred}, LocalFile) ->
    Mod = filezmod(Service),
    download_stream(Mod, Cred, Location, LocalFile).

-spec do_delete(CredLoc) -> Result when
    CredLoc :: #filestore_credentials{},
    Result :: ftpfilez:sync_reply().
do_delete(#filestore_credentials{service=Service, location=Location, credentials=Cred}) ->
    Mod = filezmod(Service),
    Mod:delete(Cred, Location).

-spec filezmod(binary()) -> module().
filezmod(<<"s3">>) -> s3filez;
filezmod(<<"ftp">>) -> ftpfilez;
filezmod(<<"webdav">>) -> webdavfilez.

download_stream(Mod, Cred, Location, LocalFile) ->
    try
        case file:open(LocalFile, [ write, binary ]) of
            {ok, FD} ->
                case Mod:stream(Cred, Location, {?MODULE, download_cb, [FD]}) of
                    ok ->
                        ok;
                    {error, Reason} = Error ->
                        ?LOG_ERROR(#{
                            text => <<"Filestore error downloading to file">>,
                            in => zotonic_mod_filestore,
                            result => error,
                            reason => Reason,
                            location => Location,
                            local_file => LocalFile
                        }),
                        file:delete(LocalFile),
                        Error
                end;
            {error, Reason} = Error ->
                ?LOG_ERROR(#{
                    text => <<"Filestore error downloading to file">>,
                    in => zotonic_mod_filestore,
                    result => error,
                    reason => Reason,
                    location => Location,
                    local_file => LocalFile
                }),
                file:delete(LocalFile),
                Error
        end
    catch
        Err:Rsn:Stack ->
            ?LOG_ERROR(#{
                text => <<"Filestore error downloading to file">>,
                in => zotonic_mod_filestore,
                result => Err,
                reason => Rsn,
                stack => Stack,
                location => Location,
                local_file => LocalFile
            }),
            file:delete(LocalFile),
            {error, Rsn}
    end.

download_cb(FD, {error, _}) ->
    file:close(FD);
download_cb(_FD, stream_start) ->
    ok;
download_cb(_FD, {headers, _}) ->
    ok;
download_cb(_FD, {content_type, _}) ->
    ok;
download_cb(FD, Data) when is_binary(Data) ->
    file:write(FD, Data);
download_cb(FD, eof) ->
    file:close(FD).

