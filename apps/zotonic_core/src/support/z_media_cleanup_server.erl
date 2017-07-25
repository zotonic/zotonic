%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Delete medium files that were attached to deleted resources.

%% Copyright 2013 Marc Worrell
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
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    cleanup/1
]).

-include_lib("zotonic_file.hrl").

% Check every 10 minutes if we have anything to delete.
% Check every 10 seconds when working through a backlog.
-define(CLEANUP_TIMEOUT_LONG, 600000).
-define(CLEANUP_TIMEOUT_SHORT, 10000).

-record(state, {site :: atom()}).


%% @doc Force a cleanup - useful after mass deletes, or when disk space is getting low.
cleanup(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:cast(Name, cleanup).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    start_link([]).
start_link(Args) when is_list(Args) ->
    {site, Site} = proplists:lookup(site, Args),
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {site, Site} = proplists:lookup(site, Args),
    lager:md([
        {site, Site},
        {module, ?MODULE}
      ]),
    {ok, #state{site=Site}, ?CLEANUP_TIMEOUT_LONG}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(cleanup, State) ->
    case do_cleanup(State#state.site) of
        {ok, 0} ->
            {noreply, State, ?CLEANUP_TIMEOUT_LONG};
        {ok, _} ->
            {noreply, State, ?CLEANUP_TIMEOUT_SHORT};
        {error, _} ->
            {noreply, State, ?CLEANUP_TIMEOUT_LONG}
    end;

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(timeout, State) ->
    handle_cast(cleanup, State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_cleanup(Site) ->
    Context = z_context:new(Site),
    do_cleanup_1(z_db:q("
                    select id, filename, deleted
                    from medium_deleted
                    order by id
                    limit 100",
                    Context),
                 Context).

do_cleanup_1([], _Context) ->
    {ok, 0};
do_cleanup_1(Rs, Context) ->
    lists:foreach(fun(R) ->
                    do_cleanup_file(R, Context)
                  end, Rs),
    Ranges = z_utils:ranges([ Id || {Id, _, _} <- Rs ]),
    z_db:transaction(
            fun(Ctx) ->
                lists:foreach(fun
                                ({A,A}) ->
                                    z_db:q("delete from medium_deleted where id = $1", [A], Ctx);
                                ({A,B}) ->
                                    z_db:q("delete from medium_deleted where id >= $1 and id <= $2", [A,B], Ctx)
                              end,
                              Ranges)
             end,
             Context),
    {ok, length(Rs)}.

do_cleanup_file({_Id, Filename, Date}, Context) ->
    PreviewPath = z_path:media_preview(Context),
    ArchivePath = z_path:media_archive(Context),
    % Remove from the file system
    BasePreview = filename:join(PreviewPath, Filename),
    Previews = z_utils:wildcard(binary_to_list(iolist_to_binary([BasePreview, "(*"]))),
    [ file:delete(Preview) || Preview <- Previews ],
    file:delete(filename:join(ArchivePath, Filename)),
    % Remove from the file store
    PreviewStore = iolist_to_binary([filename:basename(PreviewPath), $/, Filename, $( ]),
    ArchiveStore = iolist_to_binary([filename:basename(ArchivePath), $/, Filename ]),
    z_notifier:first(#filestore{action=delete, path=PreviewStore}, Context),
    z_notifier:first(#filestore{action=delete, path=ArchiveStore}, Context),
    lager:debug("Medium cleanup: ~p (from ~p)", [Filename, Date]),
    ok.
