%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2020 Marc Worrell
%% @doc Process uploading a file to a remote storage.

%% Copyright 2014-2020 Marc Worrell
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

-behaviour(gen_server).

-export([
    start_link/4,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2,

    stale_file_entry/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_core/include/zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

-define(RETRY_DELAY, 1800000).   % Retry after 30 minutes

-record(state, {
        id,
        path,
        media_info,
        lookup,
        context
    }).

start_link(Id, Path, MediaInfo, Context) ->
    Path1 = z_convert:to_binary(Path),
    PathLookup = m_filestore:lookup(Path1, Context),
    gen_server:start_link(
        {via, z_proc, {{upload, Path1}, Context}},
        ?MODULE,
        [Id, Path1, MediaInfo, PathLookup, Context],
        []).

init([Id, Path, MediaInfo, PathLookup, Context]) ->
    z_context:lager_md(Context),
    lager:debug("Started uploader for ~p", [Path]),
    gen_server:cast(self(), start),
    {ok, #state{
            id = Id,
            path = Path,
            media_info = MediaInfo,
            lookup = PathLookup,
            context = Context
        }}.

handle_call(Msg, _From, State) ->
    lager:error("Unknown call: ~p", [Msg]),
    {reply, {error, unknown_msg}, State}.

handle_cast(start, #state{lookup = {ok, Entry}} = State) ->
    try_upload(Entry, State);
handle_cast(start, #state{lookup = {error, enoent}} = State) ->
    try_upload(undefined, State);
handle_cast(start, #state{path = Path, lookup = {error, _} = Error} = State) ->
    lager:error("Filestore upload error reading for path ~p: ~p", [ Path, Error ]),
    {stop, normal, State};

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    lager:error("Unknown cast: ~p", [Msg]),
    {noreply, State}.

handle_info(restart, State) ->
    gen_server:cast(self(), start),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% ------ Support routines --------

try_upload(MaybeEntry, #state{id=Id, path=Path, context=Context, media_info=MInfo} = State) ->
    case m_filestore:is_upload_ok(MaybeEntry) of
        true ->
            RscId = maps:get(<<"id">>, MInfo, undefined),
            case z_notifier:first(#filestore_credentials_lookup{id=RscId, path=Path}, Context) of
                {ok, #filestore_credentials{} = Cred} ->
                    case handle_upload(Path, Cred, Context) of
                        ok ->
                            m_filestore:dequeue(Id, Context),
                            {stop, normal, State};
                        fatal ->
                            m_filestore:dequeue(Id, Context),
                            {stop, normal, State};
                        retry ->
                            lager:info("Filestore upload of ~p, sleeping 30m for retry.", [Path]),
                            timer:send_after(?RETRY_DELAY, restart),
                            {noreply, State, hibernate}
                    end;
                undefined ->
                    lager:info("Filestore no credentials found for ~p", [Path]),
                    m_filestore:dequeue(Id, Context),
                    {stop, normal, State}
            end;
        false ->
            case m_filestore:is_download_ok(MaybeEntry) of
                true ->
                    % Already uploaded - we can safely delete the file.
                    AbsPath = z_path:abspath(Path, Context),
                    file:delete(AbsPath);
                false ->
                    ok
            end,
            m_filestore:dequeue(Id, Context),
            {stop, normal, State}
    end.

handle_upload(Path, Cred, Context) ->
    AbsPath = z_path:abspath(Path, Context),
    case file:read_file_info(AbsPath) of
        {ok, #file_info{type=regular, size=0}} ->
            lager:info("Not uploading ~p because it is empty", [Path]),
            ok;
        {ok, #file_info{type=regular, size=Size}} ->
            Result = do_upload(Cred, {filename, Size, AbsPath}),
            finish_upload(Result, Path, AbsPath, Size, Cred, Context);
        {ok, #file_info{type=Type}} ->
            lager:error("Not uploading ~p because it is a ~p", [Path, Type]),
            fatal;
        {error, enoent} ->
            lager:error("Not uploading ~p because it is not found", [Path]),
            fatal
    end.

do_upload(#filestore_credentials{service= <<"s3">>, location=Location, credentials=Cred}, Data) ->
    s3filez:put(Cred, Location, Data).

%% @doc Remember the new location in the m_filestore, move the file to the filezcache and delete the file
finish_upload(ok, Path, AbsPath, Size, #filestore_credentials{service=Service, location=Location}, Context) ->
    lager:info("Filestore moved ~p to ~p : ~p", [Path, Service, Location]),
    FzCache = start_empty_cache_entry(Location),
    {ok, _} = m_filestore:store(Path, Size, Service, Location, Context),
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
        {error, _} = Error ->
            lager:warning("Filestore error moving to cache entry ~p (moving ~p): ~p", [Location, Path, Error]),
            file:delete(AbsPathTmp),
            ok
    end;
finish_upload({error, _} = Error, Path, _AbsPath, _Size, #filestore_credentials{service=Service, location=Location}, _Context) ->
    lager:error("Filestore upload error to ~p : ~p of ~p error ~p", [Service, Location, Path, Error]),
    retry.

start_empty_cache_entry(Location) ->
    case filezcache:insert_wait(Location) of
        {ok, _Pid} = OK ->
            OK;
        {error, {already_started, Pid}} ->
            lager:warning("Duplicate cache entry ~p (will stop & restart)", [Location]),
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
