%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2026 Marc Worrell
%% @doc Start a process to upload a file to a remote storage.
%% @end

%% Copyright 2014-2026 Marc Worrell
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

-module(filestore_uploader).

-export([
    is_upload_running/2,
    upload/5,
    upload_job/5,
    stale_file_entry/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").


is_upload_running(Path, Context) ->
    Name = {?MODULE, Path},
    is_pid(z_proc:whereis(Name, Context)).

upload(_QueueId, Path, {error, Reason}, _MediaInfo, _Context) when Reason =/= enoent ->
    ?LOG_ERROR(#{
        text => <<"Filestore upload error reading file">>,
        in => zotonic_mod_filestore,
        path => Path,
        result => error,
        reason => Reason
    }),
    {error, Reason};
upload(QueueId, Path, Status, MediaInfo, Context) ->
    case is_upload_running(Path, Context) of
        false ->
            case z_sidejob:space() of
                N when N > 0 ->
                    z_sidejob:start({?MODULE, upload_job, [QueueId, Path, Status, MediaInfo, Context]});
                _ ->
                    {error, busy}
            end;
        true ->
            {error, running}
    end.

upload_job(QueueId, Path, {ok, Entry}, MediaInfo, Context) ->
    z_context:logger_md(Context),
    Name = {?MODULE, Path},
    case z_proc:register(Name, self(), Context) of
        ok ->
            ?LOG_DEBUG(#{
                text => <<"Started uploader">>,
                in => zotonic_mod_filestore,
                src => Path,
                queue_id => QueueId
            }),
            try_upload(Entry, QueueId, Path, MediaInfo, Context);
        {error, duplicate} ->
            ok
    end;
upload_job(QueueId, Path, {error, enoent}, MediaInfo, Context) ->
    z_context:logger_md(Context),
    Name = {?MODULE, Path},
    case z_proc:register(Name, self(), Context) of
        ok ->
            ?LOG_DEBUG(#{
                text => <<"Started uploader">>,
                in => zotonic_mod_filestore,
                src => Path,
                queue_id => QueueId
            }),
            try_upload(undefined, QueueId, Path, MediaInfo, Context);
        {error, duplicate} ->
            ok
    end.

%%% ------ Support routines --------

try_upload(MaybeEntry, QueueId, Path, MInfo, Context) ->
    case m_filestore:is_upload_ok(MaybeEntry) of
        true ->
            RscId = maps:get(<<"id">>, MInfo, undefined),
            case z_notifier:first(#filestore_credentials_lookup{id=RscId, path=Path}, Context) of
                {ok, #filestore_credentials{} = Cred} ->
                    Mime = maps:get(<<"mime">>, MInfo, undefined),
                    case handle_upload(Path, Mime, Cred, Context) of
                        ok ->
                            % Uploaded, signal backoff to up the batch size.
                            m_filestore:dequeue(QueueId, Context),
                            mod_filestore:update_backoff(success, Context);
                        skip ->
                            % Nothing to do, remove entry, no backoff change.
                            m_filestore:dequeue(QueueId, Context);
                        retry ->
                            % Failure uploading, backoff.
                            mod_filestore:update_backoff(fail, Context)
                    end;
                undefined ->
                    ?LOG_WARNING(#{
                        text => <<"Filestore no credentials, ignoring queued file">>,
                        in => zotonic_mod_filestore,
                        result => error,
                        reason => no_credentials,
                        src =>  Path,
                        queue_id => QueueId
                    }),
                    m_filestore:dequeue(QueueId, Context),
                    {error, no_credentials}
            end;
        false ->
            case m_filestore:is_download_ok(MaybeEntry) of
                true ->
                    % Already uploaded - we can safely delete the file.
                    case filestore_config:is_local_keep(Context) of
                        true ->
                            ok;
                        false ->
                            AbsPath = z_path:abspath(Path, Context),
                            file:delete(AbsPath)
                    end;
                false ->
                    ok
            end,
            m_filestore:dequeue(QueueId, Context),
            ok
    end.

handle_upload(Path, Mime, Cred, Context) ->
    AbsPath = z_path:abspath(Path, Context),
    case file:read_file_info(AbsPath, [raw, {time, universal}]) of
        {ok, #file_info{type=regular, size=0}} ->
            ?LOG_NOTICE(#{
                text => <<"Not uploading empty file">>,
                in => zotonic_mod_filestore,
                src => Path
            }),
            ok;
        {ok, #file_info{type=regular, size=Size}} ->
            Result = filestore_request:do_upload(Cred, {filename, Size, AbsPath}, Mime),
            finish_upload(Result, Path, AbsPath, Size, Cred, Context);
        {ok, #file_info{type=Type}} ->
            ?LOG_ERROR(#{
                text => <<"Not uploading file because of file type">>,
                in => zotonic_mod_filestore,
                result => error,
                reason => filetype,
                src => Path,
                type => Type
            }),
            skip;
        {error, enoent} ->
            ?LOG_INFO(#{
                text => <<"Not uploading file because it is not found">>,
                in => zotonic_mod_filestore,
                src => Path
            }),
            skip
    end.

%% @doc Remember the new location in the m_filestore, move the file to the filezcache and delete the file
finish_upload(ok, Path, AbsPath, Size, #filestore_credentials{service=Service, location=Location}, Context) ->
    ?LOG_INFO(#{
        text => <<"Filestore upload done">>,
        in => zotonic_mod_filestore,
        src => Path,
        dst => Location,
        service => Service,
        result => ok
    }),
    IsLocalKeep = filestore_config:is_local_keep(Context),
    case IsLocalKeep of
        true ->
            {ok, _} = m_filestore:store(Path, Size, Service, Location, IsLocalKeep, Context),
            ok;
        false ->
            FzCache = start_empty_cache_entry(Location),
            {ok, _} = m_filestore:store(Path, Size, Service, Location, IsLocalKeep, Context),
            % Make sure that the file entry is not serving the relocated file from the file system.
            pause_file_entry(Path, Context),
            AbsPathTmp = <<AbsPath/binary, "~">>,
            _ = file:rename(AbsPath, AbsPathTmp),
            % After the file entry is marked stale it will do a new lookup, and will find
            % the file stored in the filecache.
            stale_file_entry(Path, Context),
            % The file entries are waiting for the cache process, which is monitoring the current process
            case FzCache of
                {ok, Pid} ->
                    ok = filezcache_entry:store(Pid, {tmpfile, AbsPathTmp});
                {error, Reason} ->
                    ?LOG_WARNING(#{
                        text => <<"Filestore error moving to cache entry">>,
                        in => zotonic_mod_filestore,
                        result => error,
                        reason => Reason,
                        location => Location,
                        src => Path
                    }),
                    file:delete(AbsPathTmp),
                    ok
            end
    end;
finish_upload({error, Reason}, Path, _AbsPath, _Size, #filestore_credentials{service=Service, location=Location}, _Context) ->
    ?LOG_ERROR(#{
        text => <<"Filestore upload error, will retry">>,
        in => zotonic_mod_filestore,
        result => error,
        reason => Reason,
        src => Path,
        dst => Location,
        service => Service
    }),
    retry.

start_empty_cache_entry(Location) ->
    case filezcache:insert_wait(Location) of
        {ok, _Pid} = OK ->
            OK;
        {error, {already_started, Pid}} ->
            ?LOG_NOTICE(#{
                text => <<"Duplicate cache entry (will stop & restart)">>,
                in => zotonic_mod_filestore,
                location => Location
            }),
            ok = filezcache_entry:delete(Pid),
            start_empty_cache_entry(Location);
        {error, _} = Error ->
            Error
    end.

pause_file_entry(Path, Context) ->
    z_file_request:pause(file_entry_path(Path), Context).

stale_file_entry(Path, Context) ->
    z_file_request:force_stale(file_entry_path(Path), Context).

file_entry_path(<<"archive/", Path/binary>>) ->
    Path;
file_entry_path(<<"preview/", Path/binary>>) ->
    Path;
file_entry_path(Path) ->
    Path.
