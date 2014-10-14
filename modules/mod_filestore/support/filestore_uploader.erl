%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Process uploading a file to a remote storage.

%% Copyright 2014 Marc Worrell
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

    force_stale/2
    ]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/file.hrl").

-define(RETRY_DELAY, 1800000).   % Retry after 30 minutes

-record(state, {
        id,
        path,
        props,
        context
    }).

start_link(Id, Path, Props, Context) ->
    Path1 = z_convert:to_binary(Path), 
    gen_server:start_link({via, z_proc, {{upload, Path1}, Context}}, ?MODULE, [Id, Path1, Props, Context], []).

init([Id, Path, Props, Context]) ->
    z_context:lager_md(Context),
    lager:debug("[~p] Started uploader for ~p", [z_context:site(Context), Path]),
    gen_server:cast(self(), start),
    {ok, #state{
            id = Id,
            path = Path,
            props = Props,
            context = Context
        }}.

handle_call(Msg, _From, State) ->
    lager:error("Unknown call: ~p", [Msg]),
    {reply, {error, unknown_msg}, State}.

handle_cast(start, #state{id=Id, path=Path, context=Context, props=Props} = State) ->
    case m_filestore:lookup(Path, Context) of
        undefined ->
            RscId = proplists:get_value(id, Props),
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
                            lager:debug("[~p] Filestore upload of ~p, sleeping 30m for retry.", [z_context:site(Context), Path]),
                            timer:send_after(?RETRY_DELAY, restart),
                            {noreply, State, hibernate}
                    end;
                undefined ->
                    lager:debug("[~p] Filestore no credentials found for ~p", [z_context:site(Context), Path]),
                    m_filestore:dequeue(Id, Context),
                    {stop, normal, State}
            end;
        _Entry ->
            m_filestore:dequeue(Id, Context),
            {stop, normal, State}
    end;
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

handle_upload(Path, Cred, Context) ->
    AbsPath = z_path:abspath(Path, Context),
    case file:read_file_info(AbsPath) of
        {ok, #file_info{type=regular, size=0}} ->
            lager:info("[~p] Not uploading ~p because it is empty", [z_context:site(Context), Path]),
            ok;
        {ok, #file_info{type=regular, size=Size}} ->
            Result = do_upload(Cred, {filename, Size, AbsPath}),
            finish_upload(Result, Path, AbsPath, Size, Cred, Context);
        {ok, #file_info{type=Type}} ->
            lager:error("[~p] Not uploading ~p because it is a ~p", [z_context:site(Context), Path, Type]),
            fatal;
        {error, enoent} ->
            lager:error("[~p] Not uploading ~p because it is not found", [z_context:site(Context), Path]),
            fatal
    end.

do_upload(#filestore_credentials{service= <<"s3">>, location=Location, credentials=Cred}, Data) ->
    s3filez:put(Cred, Location, Data).

%% @doc Remember the new location in the m_filestore, move the file to the filezcache and delete the archived file
finish_upload(ok, Path, AbsPath, Size, #filestore_credentials{service=Service, location=Location}, Context) ->
    lager:debug("[~p] Moved ~p to ~p : ~p", [z_context:site(Context), Path, Service, Location]),
    FzCache = start_empty_cache_entry(Location),
    {ok, _} = m_filestore:store(Path, Size, Service, Location, Context),
    case FzCache of
        {ok, Pid} ->
            force_stale(Path, Context),
            filezcache_entry:store(Pid, {tmpfile, AbsPath});
        {error, _} = Error ->
            lager:warning("[~p] Error moving to cache entry ~p (moving ~p): ~p", [z_context:site(Context), Location, Path, Error]),
            file:delete(AbsPath),
            ok
    end;
finish_upload({error, _} = Error, Path, _AbsPath, _Size, #filestore_credentials{service=Service, location=Location}, Context) ->
    lager:error("[~p] Filestore upload error to ~p : ~p of ~p error ~p", [z_context:site(Context), Service, Location, Path, Error]),
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

force_stale(Path, Context) ->
    z_file_request:force_stale(file_entry_path(Path), Context).

file_entry_path(<<"archive/", Path/binary>>) ->
    Path;
file_entry_path(<<"preview/", Path/binary>>) ->
    Path;
file_entry_path(Path) ->
    Path.
