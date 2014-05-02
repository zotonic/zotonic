%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Module managing the storage of files on remote servers.

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

-module(mod_filestore).

-author("Marc Worrell <marc@worrell.nl>").
-mod_title("File Storage").
-mod_description("Store files on cloud storage services like Amazon S3 and GreenQloud").
-mod_prio(500).
-mod_schema(1).
-mod_provides([filestore]).

-behaviour(supervisor).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    observe_filestore/2,
    observe_media_update_done/2,
    observe_filestore_credentials_lookup/2,
    observe_filestore_credentials_revlookup/2,
    observe_admin_menu/3,

    pid_observe_tick_1m/3,

    queue_all/1,
    lookup/2,

    delete_ready/5,
    download_stream/5,
    manage_schema/2
    ]).

-export([
    start_link/1,
    init/1
    ]).


observe_media_update_done(#media_update_done{action=insert, post_props=Props}, Context) ->
    queue_medium(Props, Context);
observe_media_update_done(#media_update_done{}, _Context) ->
    ok.

observe_filestore(#filestore{action=lookup, path=Path}, Context) ->
    lookup(Path, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=undefined} = Upload, Context) ->
    Mime = z_media_identify:guess_mime(Path), 
    observe_filestore(Upload#filestore{mime=Mime}, Context);
observe_filestore(#filestore{action=upload, path=Path, mime=Mime}, Context) ->
    maybe_queue_file(<<>>, Path, true, [{mime,Mime}], Context),
    ok;
observe_filestore(#filestore{action=delete, path=Path}, Context) ->
    Count = m_filestore:mark_deleted(Path, Context),
    lager:debug("filestore: marked ~p entries as deleted for ~p (~p)", [Count, Path, z_context:site(Context)]),
    ok.

observe_filestore_credentials_lookup(#filestore_credentials_lookup{path=Path}, Context) ->
    S3Key = m_config:get_value(?MODULE, s3key, Context),
    S3Secret = m_config:get_value(?MODULE, s3secret, Context),
    S3Url = m_config:get_value(?MODULE, s3url, Context),
    case is_defined(S3Key) andalso is_defined(S3Secret) andalso is_defined(S3Url) of
        true ->
            Url = make_url(S3Url, Path),
            {ok, #filestore_credentials{
                    service= <<"s3">>,
                    location=Url,
                    credentials={S3Key,S3Secret}
            }};
        false ->
            undefined
    end.

observe_filestore_credentials_revlookup(#filestore_credentials_revlookup{service= <<"s3">>, location=Location}, Context) ->
    S3Key = m_config:get_value(?MODULE, s3key, Context),
    S3Secret = m_config:get_value(?MODULE, s3secret, Context),
    case is_defined(S3Key) andalso is_defined(S3Secret) of
        true ->
            {ok, #filestore_credentials{
                    service= <<"s3">>,
                    location=Location,
                    credentials={S3Key,S3Secret}
            }};
        false ->
            undefined
    end.

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_filestore,
                parent=admin_system,
                label=?__("Cloud File Store", Context),
                url={admin_filestore},
                visiblecheck={acl, use, mod_config}}
     
     |Acc].



make_url(S3Url, <<$/, _/binary>> = Path) ->
    <<S3Url/binary, Path/binary>>;
make_url(S3Url, Path) ->
    <<S3Url/binary, $/, Path/binary>>.

is_defined(undefined) -> false;
is_defined(<<>>) -> false;
is_defined(_) -> true.

pid_observe_tick_1m(Pid, tick_1m, Context) ->
    case z_convert:to_bool(m_config:get_value(?MODULE, is_upload_enabled, Context)) of
        true ->
            start_uploaders(Pid, m_filestore:fetch_queue(Context), Context);
        false ->
            nop
    end,
    start_downloaders(m_filestore:fetch_move_to_local(Context), Context),
    start_deleters(m_filestore:fetch_deleted(Context), Context).


manage_schema(What, Context) ->
    m_filestore:install(What, Context).

lookup(Path, Context) ->
    case m_filestore:lookup(Path, Context) of
        undefined ->
            undefined;
        Props when is_list(Props) ->
            Location = proplists:get_value(location, Props),
            case filezcache:where(Location) of
                Pid when is_pid(Pid) ->
                    {ok, {filezcache, Pid, Props}};
                undefined ->
                    load_cache(Props, Context)
            end
    end.

load_cache(Props, Context) ->          
    Service = proplists:get_value(service, Props),
    Location = proplists:get_value(location, Props),
    Size = proplists:get_value(size, Props),
    Id = proplists:get_value(id, Props),
    case z_notifier:first(#filestore_credentials_revlookup{service=Service, location=Location}, Context) of
        {ok, #filestore_credentials{service= <<"s3">>, location=Location1, credentials=Cred}} ->
            Ctx = z_context:prune_for_async(Context), 
            StreamFun = fun(CachePid) ->
                            s3filez:stream(Cred, 
                                           Location1,
                                           fun({error, enoent}) ->
                                                    lager:error("File store remote file is gone ~p (~p)", [Location, z_context:site(Ctx)]),
                                                    ok = m_filestore:mark_error(Id, enoent, Ctx),
                                                    exit(normal);
                                              ({error, _} = Error) ->
                                                    % Abnormal exit when receiving an error.
                                                    % This takes down the cache entry.
                                                    lager:error("Error ~p on cache load of ~p (~p)", [Error, Location, z_context:site(Ctx)]),
                                                    exit(Error);
                                              (T) when is_tuple(T) ->
                                                    nop;
                                              (B) when is_binary(B) ->
                                                    filezcache:append_stream(CachePid, B);
                                              (eof) ->
                                                    filezcache:finish_stream(CachePid)
                                           end)
                        end,
            case filezcache:insert_stream(Location1, Size, StreamFun, []) of
                {ok, Pid} ->
                    {ok, {filezcache, Pid, Props}};
                {error, {already_started, Pid}} ->
                    {ok, {filezcache, Pid, Props}}
            end;
        undefined ->
            undefined
    end.

queue_all(Context) ->
    Media = z_db:assoc_props("select * from medium order by id asc", Context),
    {ok, length([ queue_medium(M, Context) || M <- Media ])}.

queue_medium(Props, Context) ->
    maybe_queue_file(<<"archive/">>, proplists:get_value(filename, Props), proplists:get_value(is_deletable_file, Props), Props, Context),
    Props1 = [
        {id, proplists:get_value(id, Props)}
    ],
    maybe_queue_file(<<"archive/">>, proplists:get_value(preview_filename, Props), proplists:get_value(is_deletable_preview, Props), Props1, Context).


maybe_queue_file(_Prefix, undefined, _IsDeletable, _Props, _Context) ->
    nop;
maybe_queue_file(_Prefix, <<>>, _IsDeletable, _Props, _Context) ->
    nop;
maybe_queue_file(_Prefix, _Path, false, _Props, _Context) ->
    nop;
maybe_queue_file(Prefix, Filename, true, Props, Context) ->
    FilenameBin = z_convert:to_binary(Filename), 
    case m_filestore:queue(<<Prefix/binary, FilenameBin/binary>>, Props, Context) of
        ok -> ok;
        {error, duplicate} -> ok
    end.



%%% ------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%% ------------------------------------------------------------------------------------

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(_Args) ->
    {ok,{{simple_one_for_one, 20, 10},
         [
          {undefined, {filestore_uploader, start_link, []},
           transient, brutal_kill, worker, [filestore_uploader]}
         ]}}.


%%% ------------------------------------------------------------------------------------
%%% Support routines
%%% ------------------------------------------------------------------------------------

start_uploaders(Pid, Rs, Context) ->
    [ start_uploader(Pid, R, Context) || R <- Rs ].

start_uploader(Pid, R, Context) ->
    {id, Id} = proplists:lookup(id, R),
    {path, Path} = proplists:lookup(path, R),
    {props, Props} = proplists:lookup(props, R),
    supervisor:start_child(Pid, [Id, Path, Props, Context]).

start_deleters(Rs, Context) ->
    [ start_deleter(R, Context) || R <- Rs ].

start_deleter(R, Context) ->
    {id, Id} = proplists:lookup(id, R),
    {path, Path} = proplists:lookup(path, R),
    {service, Service} = proplists:lookup(service, R),
    {location, Location} = proplists:lookup(location, R),
    case z_notifier:first(#filestore_credentials_revlookup{service=Service, location=Location}, Context) of
        {ok, #filestore_credentials{service= <<"s3">>, location=Location1, credentials=Cred}} ->
            lager:debug("Queue delete for ~p", [Location1]),
            ContextAsync = z_context:prune_for_async(Context), 
            _ = s3filez:queue_delete_id({?MODULE, delete, Id}, Cred, Location1, {?MODULE, delete_ready, [Id, Path, ContextAsync]});
        undefined ->
            lager:debug("No credentials for ~p", [R])
    end.

delete_ready(Id, Path, Context, _Ref, ok) ->
    lager:debug("Delete remote file for ~p (~p)", [Path, z_context:site(Context)]),
    m_filestore:purge_deleted(Id, Context);
delete_ready(Id, Path, Context, _Ref, {error, enoent}) ->
    lager:debug("Delete remote file for ~p was not found (~p)", [Path, z_context:site(Context)]),
    m_filestore:purge_deleted(Id, Context);
delete_ready(_Id, Path, Context, _Ref, {error, _} = Error) ->
    lager:error("Could not delete remote file. Path ~p (~p) error ~p", [Path, z_context:site(Context), Error]).


start_downloaders(Rs, Context) ->
    [ start_downloader(R, Context) || R <- Rs ].

start_downloader(R, Context) ->
    {id, Id} = proplists:lookup(id, R),
    {path, Path} = proplists:lookup(path, R),
    {service, Service} = proplists:lookup(service, R),
    {location, Location} = proplists:lookup(location, R),
    case z_notifier:first(#filestore_credentials_revlookup{service=Service, location=Location}, Context) of
        {ok, #filestore_credentials{service= <<"s3">>, location=Location1, credentials=Cred}} ->
            LocalPath = z_path:files_subdir(Path, Context),
            ok = filelib:ensure_dir(LocalPath), 
            lager:debug("Queue move to local for ~p", [Location1]),
            ContextAsync = z_context:prune_for_async(Context), 
            _ = s3filez:queue_stream_id({?MODULE, stream, Id}, Cred, Location1, {?MODULE, download_stream, [Id, Path, LocalPath, ContextAsync]});
        undefined ->
            lager:debug("No credentials for ~p", [R])
    end.

download_stream(_Id, _Path, LocalPath, Context, {content_type, _}) ->
    lager:debug("Download remote file ~p started (~p)", [LocalPath, z_context:site(Context)]),
    file:delete(temp_path(LocalPath)); 
download_stream(_Id, _Path, LocalPath, _Context, Data) when is_binary(Data) ->
    file:write_file(temp_path(LocalPath), Data, [append,raw,binary]);
download_stream(Id, Path, LocalPath, Context, eof) ->
    lager:debug("Download remote file ~p ready (~p)", [LocalPath, z_context:site(Context)]),
    ok = file:rename(temp_path(LocalPath), LocalPath), 
    m_filestore:purge_move_to_local(Id, Context),
    filezcache:delete({z_context:site(Context), Path}), 
    filestore_uploader:force_stale(Path, Context);
download_stream(Id, _Path, LocalPath, Context, {error, _} = Error) ->
    lager:debug("Download error ~p file ~p (~p)", [Error, LocalPath, z_context:site(Context)]),
    file:delete(temp_path(LocalPath)),
    m_filestore:unmark_move_to_local(Id, Context);
download_stream(_Id, _Path, _LocalPath, _Context, _Other) ->
    ok.

temp_path(F) when is_list(F) ->
    F ++ ".downloading";
temp_path(F) when is_binary(F) ->
    <<F/binary, ".downloading">>.
