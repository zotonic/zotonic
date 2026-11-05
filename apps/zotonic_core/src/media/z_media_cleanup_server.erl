%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2026 Marc Worrell
%% @doc Delete medium files that were attached to deleted resources.
%% Files are deleted after 5 weeks, which is the maximum retention period
%% of backups and the default retention period of the filestore.
%% This allows for a recover of deleted media items.
%% @end

%% Copyright 2013-2026 Marc Worrell
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

-module(z_media_cleanup_server).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    cleanup/1,
    install/1,
    delete_preview/2
]).

-include_lib("zotonic_file.hrl").
-include_lib("kernel/include/logger.hrl").

% Check every 10 minutes if we have anything to delete.
% Check every 10 seconds when working through a backlog.
-define(CLEANUP_TIMEOUT_LONG, 600000).
-define(CLEANUP_TIMEOUT_SHORT, 10000).

-record(state, {site :: atom(), temp_scan = undefined}).


%% @doc Force a cleanup - useful after mass deletes, or when disk space is getting low.
cleanup(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:cast(Name, cleanup).


%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link( atom() ) -> {ok, pid()} | ignore | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Site) ->
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    {ok, #state{site=Site}, ?CLEANUP_TIMEOUT_LONG}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(cleanup, State0) ->
    Scan = z_media_temp:sweep(State0#state.temp_scan, z_context:new(State0#state.site)),
    State = State0#state{temp_scan=case Scan of [] -> undefined; _ -> Scan end},
    case do_cleanup(State#state.site) of
        {ok, 0} ->
            {noreply, State, ?CLEANUP_TIMEOUT_LONG};
        {ok, _} ->
            {noreply, State, ?CLEANUP_TIMEOUT_SHORT}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(timeout, State) ->
    handle_cast(cleanup, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_cleanup(Site) ->
    Context = z_context:new(Site),
    case m_cafs:is_enabled(Context) of
        false -> {ok, 0};
        true -> do_cleanup_db(Context)
    end.

do_cleanup_db(Context) ->
    ok = m_cafs:maintenance(Context),
    do_cleanup_1(z_db:q("
                    select id, filename, deleted
                    from medium_deleted
                    where deleted < now() - interval '5 weeks' and retry_after <= now()
                    order by id
                    limit 100",
                    Context),
                 Context).

do_cleanup_1(Rows, Context) ->
    Done = lists:filter(fun({Id, Filename, _Date}) ->
        Result = z_db:transaction(fun(Ctx) ->
            %% Follow the writer's CAFS-before-medium lock order. NOWAIT on the
            %% table also avoids waiting on a writer that acquired it first.
            case m_cafs:lookup(Filename, Ctx) of
                {ok, Hash} -> z_db:q1("select hash from cafs where hash=$1 for update", [Hash], Ctx);
                {error, enoent} -> ok
            end,
            %% Serialize the reference check and filesystem deletion against
            %% medium writers, including reuse of a previously deleted filename.
            z_db:q("lock table medium in share mode nowait", Ctx),
            case z_db:q1("select exists(select 1 from medium where filename=$1 or preview_filename=$1)",
                    [Filename], Ctx) of
                true -> {error, referenced};
                false -> do_cleanup_file(Filename, Ctx)
            end
        end, Context),
        case Result of
            ok ->
                z_db:q("delete from medium_deleted where id=$1", [Id], Context),
                true;
            Error ->
                z_db:q("update medium_deleted set retry_after=now()+interval '1 hour' where id=$1", [Id], Context),
                ?LOG_DEBUG(#{
                    text => <<"Media cleanup deferred">>,
                    in => zotonic_core,
                    filename => Filename,
                    result => error,
                    reason => Error
                }),
                false
        end
    end, Rows),
    {ok, length(Done)}.

do_cleanup_file(Filename, Context) ->
    ok = m_cafs:retire_filename(Filename, Context),
    PreviewPath = z_path:media_preview(Context),
    ArchivePath = z_path:media_archive(Context),
    PreviewFilePath = filename:join(PreviewPath, Filename),
    Dir = filename:dirname(PreviewFilePath),
    Prefix = <<(z_convert:to_binary(filename:basename(PreviewFilePath)))/binary, "(">>,
    %% Literal matching: filenames may contain wildcard characters.
    case file:list_dir(Dir) of
        {ok, Names} ->
            Files = [
                filename:join(Dir, N)
                || N <- Names,
                   lists:prefix(binary_to_list(Prefix), z_convert:to_list(N))
            ],
            cleanup_files(Files ++ [filename:join(ArchivePath, Filename)], Filename, Context);
        {error, enoent} ->
            cleanup_files([filename:join(ArchivePath, Filename)], Filename, Context);
        {error, _} = Error ->
            Error
    end.

cleanup_files(Files, Filename, Context) ->
    case delete_files(Files) of
        ok ->
            case delete_remote({prefix, <<"preview/", Filename/binary, "(">>}, Context) of
                ok -> delete_remote(<<"archive/", Filename/binary>>, Context);
                Error -> Error
            end;
        Error -> Error
    end.

delete_files([]) -> ok;
delete_files([File | Rest]) ->
    case file:delete(File) of
        ok -> delete_files(Rest);
        {error, enoent} -> delete_files(Rest);
        {error, _} = Error -> Error
    end.

%% Exact legacy preview cleanup after proven obsolescence. The usage tombstone
%% remains queued by the preview checker if a local or remote deletion fails.
-spec delete_preview(Path, Context) -> ok | {error, term()}
    when Path :: binary(), Context :: z:context().
delete_preview(Path, Context) ->
    case delete_files([filename:join(z_path:media_preview(Context), Path)]) of
        ok -> delete_remote(<<"preview/", Path/binary>>, Context);
        Error -> Error
    end.

delete_remote(Path, Context) ->
    case z_file_request:delete_remote(Path, Context) of
        absent -> ok;
        pending -> {error, filestore_pending};
        {error, _} = Error -> Error
    end.

-spec install(Context) -> ok when Context :: z:context().
install(Context) ->
    case z_db:column(medium_deleted, retry_after, Context) of
        {ok, _} -> ok;
        {error, enoent} ->
            [] = z_db:q("alter table medium_deleted add column retry_after timestamptz not null default now()", Context),
            z_db:flush(Context)
    end.
