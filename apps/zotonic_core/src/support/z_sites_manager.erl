%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Server managing all sites running inside Zotonic. Starts the sites
%% according to the config files in the sites subdirectories. Handles scanning
%% of all site directories for config files.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(z_sites_manager).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% API exports
-export([
    upgrade/0,
    get_sites/0,
    get_site_status/1,
    set_site_status/2,
    is_sites_running/0,
    get_site_contexts/0,
    get_site_config/1,
    get_fallback_site/0,
    get_builtin_sites/0,
    get_sites_hosts/0,
    module_loaded/1,
    info/0,
    foreach/1,

    stop/1,
    start/1,
    restart/1,
    await_startup/1,

    wait_for_running/1,
    wait_for_running/2,

    get_site_config_overrides/1,
    put_site_config_overrides/2,

    filechanged_observer/2
]).

%% Testing
-export([
    do_scan_sites/0,
    do_scan_sites/1
]).

-include("../../include/zotonic.hrl").
-include_lib("zotonic_filehandler/include/zotonic_filehandler.hrl").
-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").

-type site_status() :: new
                     | starting
                     | running
                     | stopping
                     | retrying
                     | failed
                     | stopped
                     | removing.

-record(state, {
    sites :: map(),
    site_monitors :: map()
}).

-record(site_status, {
    site :: atom(),
    is_enabled = true,
    status = new :: site_status(),
    pid = undefined :: undefined | pid(),
    start_time = undefined :: undefined | z_datetime:timestamp(),
    stop_time = undefined :: undefined | erlang:timestamp(),
    stop_count = 0 :: integer(),
    crash_time = undefined :: undefined | erlang:timestamp(),
    crash_count = 0 :: integer(),
    config = [] :: list()
}).

-export_type([site_status/0]).


% Backoff periods (in seconds) for restarting failed sites
-define(BACKOFF_SHORT, 2).
-define(BACKOFF_LONG, 60).

% Every minute check, check for new sites and removed sites
-define(PERIODIC_UPGRADE, 60000).

% Every second, check if any site needs a (re)start
-define(PERIODIC_START, 1000).

% Seconds after we declare a site as non-crashed and clear the backoff
% (Defaults to 5 minutes)
-define(PERIOD_CLEAR_CRASH, 300).

% Number of sites that can be started in parallel
-define(MAX_PARALLEL_START, 5).

% Ets table holding a quick lookup of a site's status
-define(SITES_STATUS_TABLE, sites_manager_sites_status).

% Timeout when waiting for a site to become available
-define(MAX_WAIT_FOR_RUNNING, 30).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Sync the supervised sites with the sites in the sites directory.
%% Removes and stops deleted sites, adds (but does not start) new sites.
-spec upgrade() -> ok.
upgrade() ->
    gen_server:cast(?MODULE, upgrade).

%% @doc Return a list of all sites and their current running status.
%%      This is coming from the ets table, so might be a bit delayed.
-spec get_sites() -> #{ atom() => site_status() }.
get_sites() ->
    List = ets:tab2list(?SITES_STATUS_TABLE),
    maps:from_list(List).

%% @doc Return a list of all sites, their current running status and their hosts configs
-spec get_sites_hosts() -> {ok, #{ atom() => {site_status(), [ {Host::binary(), Prio :: pos_integer()} ]} }}.
get_sites_hosts() ->
    gen_server:call(?MODULE, get_sites_hosts, infinity).

%% @doc Get the status of a particular site
-spec get_site_status(z:context()|atom()) -> {ok, site_status()} | {error, bad_name}.
get_site_status(#context{site=Site}) ->
    get_site_status(Site);
get_site_status(Site) when is_atom(Site) ->
    case ets:lookup(?SITES_STATUS_TABLE, Site) of
        [] -> {error, bad_name};
        [{Site, Status}] -> {ok, Status}
    end.

%% @doc Set the status of a site, called by the site supervisor
-spec set_site_status(atom(), site_status()) -> ok.
set_site_status(Site, Status) when is_atom(Site) ->
    gen_server:cast(?MODULE, {set_site_status, Site, Status}).

%% @doc Return information on all running sites.
-spec info() -> {ok, #{ atom() => #site_status{}} }.
info() ->
    gen_server:call(?MODULE, info).

%% @doc Do something for all sites that are currently running.
-spec foreach( fun((z:context()) -> any()) ) -> ok.
foreach(Fun) ->
    maps:fold(
        fun
            (Site, running, _) ->
                try
                    Fun( z_context:new(Site) ),
                    ok
                catch
                    _:_ -> ok
                end;
            (_Site, _Status, _) ->
                ok
        end,
        ok,
        get_sites()).


%% @doc Return true iff all sites are running. Don't count sites manually stopped.
-spec is_sites_running() -> boolean().
is_sites_running() ->
    maps:fold(
        fun
            (_Site, _Status, false) -> false;
            (_Site, running, true) -> true;
            (_Site, stopped, true) -> true;
            (_Site, stopping, true) -> true;
            (_Site, _Status, _Acc) -> false
        end,
        true,
        get_sites()).

%% @doc Return a list of contexts for all running sites.
-spec get_site_contexts() -> [ z:context() ].
get_site_contexts() ->
    maps:fold(
        fun
            (Site, running, Acc) ->
                try
                    [ z_context:new(Site) | Acc ]
                catch
                    _:_ -> Acc
                end;
            (_Site, _Status, Acc) ->
                Acc
        end,
        [],
        get_sites()).

%% @doc Fetch the configuration of a specific site.
-spec get_site_config(atom()) -> {ok, list()} | {error, bad_name|term()}.
get_site_config(Site) ->
    gen_server:call(?MODULE, {get_site_config, Site}, infinity).

%% @doc Return the name of the site to handle unknown Host requests
-spec get_fallback_site() -> atom() | undefined.
get_fallback_site() ->
    gen_server:call(?MODULE, get_fallback_site).

%% @doc The list of builtin sites, they are located in the zotonic/apps/ directory.
-spec get_builtin_sites() -> [ atom() ].
get_builtin_sites() ->
    [ zotonic_site_status, zotonic_site_testsandbox ].

%% @doc Stop a site or multiple sites.
stop([Node, Site]) ->
    rpc:call(Node, ?MODULE, stop, [Site]);
stop(Site) when is_atom(Site) ->
    gen_server:call(?MODULE, {stop, Site}).

%% @doc Start a site or multiple sites.
start([Node, Site]) ->
    rpc:call(Node, ?MODULE, start, [Site]);
start(Site) when is_atom(Site) ->
    gen_server:call(?MODULE, {start, Site}).

%% @doc Restart a site or multiple sites.
restart([Node, Site]) ->
    rpc:call(Node, ?MODULE, restart, [Site]);
restart(Site) when is_atom(Site) ->
    case get_site_status(Site) of
        {ok, running} ->
            gen_server:call(?MODULE, {stop, Site}),
            restart(Site);
        {ok, stopping} ->
            timer:sleep(100),
            restart(Site);
        {ok, starting} ->
            await_startup(Site);
        {ok, S} when S =:= failed; S =:= new; S =:= stopped ->
            start(Site),
            await_startup(Site);
        {ok, Status} ->
            {error, Status};
        {error, _} = Error ->
            Error
    end.

%% @doc Tell the sites manager that a module was loaded, check
%%      changes to observers, schema.
module_loaded(Module) ->
    gen_server:cast(?MODULE, {module_loaded, Module}).


%% @doc Wait for a site to complete its startup sequence.
-spec await_startup( z:context() | atom() ) -> ok | {error, bad_name | failed | removing | stopped | stopping}.
await_startup(Context = #context{}) ->
    await_startup(z_context:site(Context));
await_startup(Site) when is_atom(Site) ->
    case get_site_status(Site) of
        {ok, running} ->
            % Now wait for the sites modules to be started
            Context = z_context:new(Site),
            z_module_manager:upgrade_await(Context);
        {ok, starting} ->
            timer:sleep(1000),
            await_startup(Site);
        {ok, new} ->
            timer:sleep(1000),
            await_startup(Site);
        {ok, retrying} ->
            timer:sleep(1000),
            await_startup(Site);
        {ok, Other} ->
            {error, Other};
        {error, _} = Error ->
            Error
    end.


%% @doc Wait for a site to be running, max 30 secs.
-spec wait_for_running(atom()) -> ok | {error, bad_name | timeout | stopped | removing | term()}.
wait_for_running(Site) when is_atom(Site) ->
    wait_for_running(Site, ?MAX_WAIT_FOR_RUNNING).

% running -> ok
% new -> request start
% starting -> wait max 30 secs, otherwise 503
% retrying -> wait max 30 secs, otherwise 503
% failing -> wait max 30 secs, otherwise 503
% failed -> check scheduled retry time, maybe wait, otherwise 503
% stopping -> status site
% stopped -> status site
% removing -> status site

-spec wait_for_running(atom(), Secs::integer()) -> ok | {error, bad_name | timeout | stopped | removing | term()}.
wait_for_running(Site, Timeout) when is_atom(Site) ->
    case ets:lookup(?SITES_STATUS_TABLE, Site) of
        [] -> {error, bad_name};
        [{Site, running}] -> ok;
        [{Site, stopped}] -> {error, stopped};
        [{Site, stopping}] -> {error, stopping};
        [{Site, removing}] -> {error, removing};
        [{Site, Status}] -> wait_for_running_1(Site, Status, Timeout)
    end.

wait_for_running_1(Site, new, Timeout) when Timeout >= 0 ->
    start(Site),
    timer:sleep(1000),
    wait_for_running(Site, Timeout-1);
wait_for_running_1(_Site, _Status, Timeout) when Timeout =< 0 ->
    {error, timeout};
wait_for_running_1(Site, failed, Timeout) ->
    Now = z_datetime:timestamp(),
    case gen_server:call(?MODULE, {get_status_start, Site}) of
        {ok, {running, _RestartTime}} ->
            ok;
        {ok, {failed, RestartTime}} when RestartTime =< Now + Timeout ->
            Sleep = erlang:max(1, RestartTime - Now + 1),
            timer:sleep(Sleep*1000),
            wait_for_running(Site, Timeout - Sleep);
        {ok, {failed, _RestartTime}} ->
            % Backoff - we need to wait longer than our max timeout
            {error, timeout};
        {ok, {OtherState, _RestartTime}} ->
            % Status changed between ets lookup and the gen_server call
            wait_for_running_1(Site, OtherState, Timeout);
        {error, _} = Error ->
            Error
    end;
wait_for_running_1(Site, _State, Timeout) ->
    timer:sleep(1000),
    wait_for_running(Site, Timeout - 1).



%% @doc Called by the zotonic_filehandler after a file has been changed. This relays the
%% file change event to all sites using the #filewatcher{} event.
-spec filechanged_observer(#zotonic_filehandler_filechange{}, term()) -> ok.
filechanged_observer(#zotonic_filehandler_filechange{} = ChangeEvent, _CallContext) ->
    #zotonic_filehandler_filechange{
        verb = Verb,
        file = File,
        basename = Basename,
        extension = Extension
    } = ChangeEvent,
    Event = #filewatcher{
        verb = Verb,
        file = File,
        basename = Basename,
        extension = Extension
    },
    z_sites_manager:foreach(
        fun(Context) ->
            z_notifier:notify_sync(Event, Context)
        end).


%% @doc Set extra configurations for a site. Will overlay the read configuration when
%% the site is started.
-spec get_site_config_overrides(Site) -> List when
    Site :: atom(),
    List :: proplists:proplist().
get_site_config_overrides(Site) when is_atom(Site) ->
    Key = z_convert:to_atom(z_convert:to_list(Site) ++ "_config_overrides"),
    application:get_env(zotonic_core, Key, []).

%% @doc Override a given site config with arbitrary key/value pairs. Should be called before
%% the site is started.
-spec put_site_config_overrides(Site, Overrides) -> ok when
    Site :: atom(),
    Overrides :: proplists:proplist().
put_site_config_overrides(Site, Overrides) when is_atom(Site), is_list(Overrides) ->
    Key = z_convert:to_atom(z_convert:to_list(Site) ++ "_config_overrides"),
    application:set_env(zotonic_core, Key, Overrides).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([]) ->
    z_module_indexer:new_ets(),
    z_mediaclass:new_ets(),
    ets:new(?SITES_STATUS_TABLE, [set, public, named_table, {keypos, 1}]),
    ok = gen_server:cast(self(), upgrade),
    timer:send_after(?PERIODIC_UPGRADE, periodic_upgrade),
    timer:send_after(?PERIODIC_START, periodic_start),
    zotonic_notifier:observe(
        ?SYSTEM_NOTIFIER, zotonic_filehandler_filechange,
        {?MODULE, filechanged_observer},
        self(), 500),
    {ok, #state{
        sites = #{},
        site_monitors = #{}
    }}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Return all sites
handle_call(get_sites_status, _From, #state{ sites = Sites } = State) ->
    SiteStatus = maps:fold(
        fun(Site, Status, Acc) ->
            Acc#{ Site => Status#site_status.status }
        end,
        #{},
        Sites),
    {reply, SiteStatus, State};

%% @doc Start a site.
handle_call({start, Site}, _From, #state{ sites = Sites } = State) ->
    case do_reload_site_config(Site, Sites) of
        {ok, Sites1} ->
            State1 = State#state{ sites = Sites1 },
            case do_start(Site, State1) of
                {ok, StateStarting} ->
                    do_sync_status(StateStarting#state.sites),
                    {reply, ok, StateStarting};
                {error, _} = Error ->
                    {reply, Error, State1}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

%% @doc Stop a site.
handle_call({stop, Site}, _From, State) ->
    case do_stop(Site, State) of
        {ok, StateStopping} ->
            do_sync_status(StateStopping#state.sites),
            {reply, ok, StateStopping};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({get_site_config, Site}, _From, #state{ sites = Sites } = State) ->
    case maps:find(Site, Sites) of
        {ok, #site_status{ config = Config }} ->
            Overrides = get_site_config_overrides(Site),
            {reply, {ok, z_utils:props_merge(Overrides, Config)}, State};
        error ->
            {reply, {error, bad_name}, State}
    end;

handle_call({get_status_start, Site}, _From, #state{ sites = Sites } = State) ->
    Reply = case maps:find(Site, Sites) of
        {ok, #site_status{ status = Status, start_time = StartTime }} ->
            {ok, {Status, StartTime}};
        error ->
            {error, bad_name}
    end,
    {reply, Reply, State};

handle_call(get_sites_hosts, _From, #state{ sites = Sites } = State) ->
    {reply, {ok, do_get_sites_hosts(Sites)}, State};

handle_call(get_fallback_site, _From, #state{ sites = Sites } = State) ->
    {reply, do_get_fallback_site(Sites), State};

handle_call(info, _From, #state{ sites = Sites } = State) ->
    {reply, {ok, Sites}, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

handle_cast(scan_sites, State) ->
    State1 = rescan_sites(State),
    do_sync_status(State1#state.sites),
    {noreply, State1};

%% @doc Sync known sites with loaded sites
handle_cast(upgrade, #state{ sites = Sites } = State) ->
    UpgradedSites = do_upgrade(Sites),
    do_sync_status(UpgradedSites),
    {noreply, State#state{ sites = UpgradedSites }};

%% @doc Handle load of a module, check observers and schema
handle_cast({module_loaded, Module}, State) ->
    do_load_module(Module, State),
    {noreply, State};

handle_cast({set_site_status, Site, Status}, #state{ sites = Sites } = State) ->
    Sites1 = case maps:find(Site, Sites) of
        {ok, #site_status{ status = Status }} ->
            Sites;
        {ok, #site_status{ status = starting } = S} when Status =:= running ->
            z_sites_dispatcher:update_dispatchinfo(),
            S1 = S#site_status{
                status = running,
                start_time = z_datetime:timestamp()
            },
            Sites#{ Site => S1 };
        {ok, #site_status{ status = retrying } = S} when Status =:= running; Status =:= starting ->
            z_sites_dispatcher:update_dispatchinfo(),
            S1 = S#site_status{
                status = running,
                start_time = z_datetime:timestamp()
            },
            Sites#{ Site => S1 };
        {ok, #site_status{ status = CurStatus }} ->
            ?LOG_NOTICE(#{
                text => <<"Site status change">>,
                in => zotonic_core,
                old_status => CurStatus,
                status => Status
            }, #{ site => Site }),
            Sites;
        error ->
            ?LOG_NOTICE(#{
                text => <<"Site status change">>,
                in => zotonic_core,
                old_status => unknown,
                status => Status
            }, #{ site => Site }),
            Sites
    end,
    do_sync_status(Sites1),
    {noreply, State#state{ sites = Sites1 }};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}

handle_info({'DOWN', MRef, process, Pid, Reason}, State) ->
    State1 = handle_down(MRef, Pid, Reason, State),
    do_sync_status(State1#state.sites),
    {noreply, State1};

handle_info(periodic_upgrade, #state{ sites = Sites } = State) ->
    UpgradedSites = do_upgrade(Sites),
    timer:send_after(?PERIODIC_UPGRADE, periodic_upgrade),
    do_sync_status(UpgradedSites),
    erlang:garbage_collect(),
    {noreply, State#state{ sites = UpgradedSites }};

handle_info(periodic_start, State) ->
    State1 = do_start_sites(State),
    State2 = do_cleanup_crash_state(State1),
    timer:send_after(?PERIODIC_START, periodic_start),
    do_sync_status(State2#state.sites),
    {noreply, State2};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    ?DEBUG({z_sites_manager, _Info}),
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Sync the status of all sites to the SITES_STATUS_TABLE ets table.
do_sync_status(Sites) ->
    % Update existing, remove unknown
    IsChanged = lists:foldl(
        fun({Site, Status}, AccChanged) ->
            case maps:find(Site, Sites) of
                {ok, #site_status{ status = Status }} ->
                    AccChanged;
                {ok, #site_status{ status = NewStatus }} ->
                    ets:insert(?SITES_STATUS_TABLE, {Site, NewStatus}),
                    true;
                error ->
                    ets:delete(?SITES_STATUS_TABLE, Site),
                    true
            end
        end,
        false,
        ets:tab2list(?SITES_STATUS_TABLE)),
    % Insert new
    IsChanged1 = maps:fold(
        fun(Site, #site_status{ status = Status }, AccChanged) ->
            case ets:lookup(?SITES_STATUS_TABLE, Site) of
                [] ->
                    ets:insert(?SITES_STATUS_TABLE, {Site, Status}),
                    true;
                _ ->
                    AccChanged
            end
        end,
        IsChanged,
        Sites),
    case IsChanged1 of
        true ->
            notify_status(),
            z_sites_dispatcher:update_hosts();
        false ->
            ok
    end.

notify_status() ->
    Sites = ets:tab2list(?SITES_STATUS_TABLE),
    zotonic_notifier:notify(sites_status, Sites, undefined).

% status2map(#site_status{ } = S) ->
%     #{
%         site => S#site_status.site,
%         is_enabled => S#site_status.is_enabled,
%         status => S#site_status.status,
%         start_time => tm(S#site_status.start_time),
%         stop_time => tm(S#site_status.stop_time),
%         stop_count => S#site_status.stop_count,
%         crash_time => tm(S#site_status.crash_time),
%         crash_count => S#site_status.crash_count
%     }.

% tm(undefined) ->
%     undefined;
% tm({MSecs, Secs, _USecs}) ->
%     z_datetime:timestamp_to_datetime(MSecs * 1000000 + Secs).

% ----------------------------------------------------------------------------

do_start_sites(#state{ sites = Sites } = State) ->
    Now = z_datetime:timestamp(),
    NStarting = maps:fold(
        fun
            (_, #site_status{ status = starting }, Count) -> Count+1;
            (_, #site_status{ status = retrying }, Count) -> Count+1;
            (_, _, Count) -> Count
        end,
        0,
        Sites),
    NQueued = maps:fold(
        fun
            (_, #site_status{ status = new, start_time = S }, Count)
                when S =< Now -> Count+1;
            (_, #site_status{ status = failed, start_time = S }, Count)
                when S =< Now -> Count+1;
            (_, _, Count) ->
                Count
        end,
        0,
        Sites),
    maybe_start_sites(NStarting, NQueued, Now, State).

maybe_start_sites(_, 0, _Now, State) ->
    State;
maybe_start_sites(NStarting, _NQueued, _Now, State) when NStarting >= ?MAX_PARALLEL_START ->
    State;
maybe_start_sites(NStarting, _NQueued, Now, State) ->
    StartAll = maps:fold(
        fun
            (Site, #site_status{ status = new, start_time = ST }, Acc) when ST =< Now ->
                [ {ST, Site} | Acc];
            (Site, #site_status{ status = failed, start_time = ST }, Acc) when ST =< Now ->
                [ {ST, Site} | Acc];
            (_Site, _SiteStatus, Acc) ->
                Acc
        end,
        [],
        State#state.sites),
    MaxStarting = ?MAX_PARALLEL_START - NStarting,
    StartTop = lists:sublist(
        lists:sort(StartAll),
        max(0, min(length(StartAll), MaxStarting))),
    lists:foldl(
        fun({_, Site}, StateAcc) ->
            case do_start(Site, StateAcc) of
                {ok, StateAcc1} -> StateAcc1;
                {error, _} -> StateAcc
            end
        end,
        State,
        StartTop).

% ----------------------------------------------------------------------------

do_start(Site, #state{ sites = Sites } = State) ->
    case maps:find(Site, Sites) of
        {ok, SiteStatus} ->
            case do_start_site(SiteStatus) of
                {ok, SiteStatus1} ->
                    State1 = State#state{
                        sites = Sites#{ Site => SiteStatus1 }
                    },
                    State2 = ensure_site_monitor(SiteStatus1, State1),
                    {ok, State2};
                {error, _} = Error ->
                    Error
            end;
        error ->
            ?LOG_WARNING(#{
                action => start_request,
                in => zotonic_core,
                result => error,
                reason => bad_name,
                text => <<"Requested to start unknown site">>
            }, #{ site => Site }),
            {error, bad_name}
    end.

do_start_site(#site_status{ site = Site } = SiteStatus) ->
    case site_is_startable(SiteStatus) of
        {true, StartState} ->
            ?LOG_NOTICE(#{
                text => <<"Site starting">>,
                in => zotonic_core,
                action => starting
            }, #{ site => Site }),
            case z_sites_sup:start_site(Site) of
                {ok, Pid} ->
                    {ok, SiteStatus#site_status{
                        status = StartState,
                        pid = Pid
                    }};
                {error, {already_started, Pid}} ->
                    % seems we have a race condition here
                    ?LOG_ERROR(#{
                        text => <<"Site already started, this shouldn't happen.">>,
                        in => zotonic_core,
                        result => error,
                        reason => already_started
                    }, #{ site => Site }),
                    {ok, SiteStatus#site_status{
                        pid = Pid
                    }};
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        text => "Site start failed",
                        in => zotonic_core,
                        result => error,
                        reason => Reason
                    }, #{ site => Site }),
                    Error
            end;
        false ->
            {error, SiteStatus#site_status.status}
    end.

site_is_startable(#site_status{ status = new }) -> {true, starting};
site_is_startable(#site_status{ status = stopped }) -> {true, starting};
site_is_startable(#site_status{ status = failed }) -> {true, retrying};
site_is_startable(_) -> false.


ensure_site_monitor(#site_status{ site = Site, pid = Pid }, #state{ site_monitors = Ms } = State) ->
    case maps:find(Pid, Ms) of
        {ok, _} ->
            State;
        error ->
            MRef = erlang:monitor(process, Pid),
            State#state{ site_monitors = Ms#{ Pid => {MRef, Site} } }
    end.

% ----------------------------------------------------------------------------

do_stop(Site, #state{ sites = Sites } = State) ->
    case maps:find(Site, Sites) of
        {ok, SiteStatus} ->
            case do_stop_site(SiteStatus) of
                {ok, SiteStatus1} ->
                    State1 = State#state{
                        sites = Sites#{ Site => SiteStatus1 }
                    },
                    {ok, State1};
                {error, _} = Error ->
                    Error
            end;
        error ->
            {error, bad_name}
    end.

do_stop_site(#site_status{ pid = Pid } = SiteStatus) ->
    case site_is_stoppable(SiteStatus) of
        true when is_pid(Pid) ->
            z_sites_sup:stop_site(Pid),
            {ok, SiteStatus#site_status{ status = stopping }};
        true when Pid =:= undefined ->
            {ok, SiteStatus#site_status{ status = stopped }};
        false ->
            {error, SiteStatus#site_status.status}
    end.

site_is_stoppable(#site_status{ status = running }) -> true;
site_is_stoppable(#site_status{ status = starting }) -> true;
site_is_stoppable(#site_status{ status = retrying }) -> true;
site_is_stoppable(_) -> false.

% ----------------------------------------------------------------------------

handle_down(MRef, Pid, Reason, #state{ site_monitors = Ms } = State) ->
    case maps:find(Pid, Ms) of
        {ok, {MRef, Site}} ->
            State1 = State#state{ site_monitors = maps:remove(Pid, Ms) },
            Sites1 = do_site_down(Site, Reason, State1#state.sites),
            State1#state{ sites = Sites1 };
        error ->
            ?LOG_WARNING(#{
                text => <<"'DOWN' for unknown site">>,
                in => zotonic_core,
                result => error,
                reason => Reason
            }),
            State
    end.

do_site_down(Site, Reason, Sites) ->
    case maps:find(Site, Sites) of
        {ok, #site_status{ status = removing }} ->
            z_sites_dispatcher:update_dispatchinfo(),
            maps:remove(Site, Sites);
        {ok, Status} ->
            Status1 = Status#site_status{
                pid = undefined,
                status = new_status_after_down(Site, Status#site_status.status, Reason),
                stop_count = Status#site_status.stop_count + 1,
                stop_time = os:timestamp()
            },
            Status2 = maybe_schedule_restart(Status1),
            z_sites_dispatcher:update_dispatchinfo(),
            Sites#{ Site => Status2 };
        error ->
            ?LOG_WARNING(#{
                text => <<"'DOWN' for site, but no site status found">>,
                in => zotonic_core,
                site => Site,
                result => error,
                reason => Reason
            }),
            Sites
    end.

new_status_after_down(_Site, stopping, shutdown) ->
    stopped;
new_status_after_down(Site, Status, Reason) ->
    ?LOG_ERROR(#{
        text => <<"Site is down">>,
        in => zotonic_core,
        old_status => Status,
        status => failed,
        reason => Reason
    }, #{ site => Site }),
    failed.

maybe_schedule_restart(#site_status{ status = stopped } = Status) ->
    Status;
maybe_schedule_restart(#site_status{ status = failed } = Status) ->
    % Non normal failure - site will be restarted
    Status#site_status{
        start_time = start_backoff(Status#site_status.crash_count + 1),
        crash_count = Status#site_status.crash_count + 1,
        crash_time = os:timestamp()
    }.

start_backoff(N) when N < 2 ->
    z_datetime:timestamp();
start_backoff(N) when N < 10 ->
    z_datetime:timestamp() + ?BACKOFF_SHORT;
start_backoff(_N) ->
    z_datetime:timestamp() + ?BACKOFF_LONG.


% ----------------------------------------------------------------------------

%% @doc If a site is running longer than ?PERIOD_CLEAR_CRASH seconds, then
%%      clear the crash count, assuming previous crashes are gone.
do_cleanup_crash_state(#state{ sites = Sites } = State) ->
    ClearTime = z_datetime:timestamp() - ?PERIOD_CLEAR_CRASH,
    Sites1 = maps:map(
        fun
            (_, #site_status{status = running, crash_count = N, start_time = T } = S)
                when N > 0, T < ClearTime ->
                S#site_status{ crash_count = 0 };
            (_, S) ->
                S
        end,
        Sites),
    State#state{ sites = Sites1 }.

% ----------------------------------------------------------------------------

%% @doc Reload the site's config files.
do_reload_site_config(Site, Sites) ->
    case maps:find(Site, Sites) of
        {ok, S} ->
            case scan_app(Site) of
                {true, NewConfig} ->
                    S1 = S#site_status{ config = NewConfig },
                    {ok, Sites#{ Site => S1 }};
                false ->
                    {ok, Sites}
            end;
        error ->
            ?LOG_INFO(#{
                text => <<"Requested to reload site config from unknown site">>,
                in => zotonic_core,
                site => Site
            }),
            {error, bad_name}
    end.

% ----------------------------------------------------------------------------


%% @doc Rescan all sites, add new sites to the sites map
rescan_sites(#state{ sites = Sites } = State) ->
    ScannedSites = do_scan_sites(),
    remove_unknown_sites(Sites, ScannedSites),
    NewSites = insert_new_sites(Sites, ScannedSites),
    self() ! startup_check,
    State#state{ sites = NewSites }.

%% @doc Check all known sites against the scanned sites.
%%      Stop all sites that are not in the scanned sites.
remove_unknown_sites(Sites, ScannedSites) ->
    Removed = maps:fold(
        fun(Site, _SiteStatus, Acc) ->
            case maps:is_key(Site, ScannedSites) of
                true -> Acc;
                false -> [ Site | Acc ]
            end
        end,
        [],
        Sites),
    lists:foreach(
        fun(Site) ->
            self() ! {remove_site, Site}
        end,
        Removed).

%% @doc Check all known sites, add a new site status record
%%      for the newly started site.
insert_new_sites(Sites, ScannedSites) ->
    maps:fold(
        fun(Site, Cfg, Acc) ->
            case maps:find(Site, Sites) of
                {ok, _} ->
                    Acc;
                error ->
                    S = new_site_status(Site, initial_status(Cfg), Cfg),
                    Acc#{ Site => S }
            end
        end,
        Sites,
        ScannedSites).

initial_status(Cfg) ->
    case proplists:get_value(enabled, Cfg, true) of
        true -> new;
        false -> stopped
    end.

new_site_status(Site, new, Cfg) ->
    #site_status{
        site = Site,
        status = new,
        start_time = z_datetime:timestamp(),
        config = Cfg
    };
new_site_status(Site, stopped, Cfg) ->
    #site_status{
        site = Site,
        status = stopped,
        config = Cfg
    }.

%% @doc Scan all sites subdirectories for the site configurations.
-spec do_scan_sites() -> #{ Site::atom() => proplists:proplist() }.
do_scan_sites() ->
    List = do_scan_sites( is_testsandbox_node() ),
    lists:foldl(
        fun(Cfg, Acc) ->
            {site, Site} = proplists:lookup(site, Cfg),
            Acc#{ Site => Cfg }
        end,
        #{},
        List).

do_scan_sites(true) ->
    lists:filter(
        fun is_testsandbox_site/1,
        scan_lib_dir( z_path:build_lib_dir() ));
do_scan_sites(false) ->
    lists:filter(
        fun(Cfg) -> not is_testsandbox_site(Cfg) end,
        scan_lib_dir( z_path:build_lib_dir() )).

is_testsandbox_site(Cfg) ->
   proplists:get_value(site, Cfg) =:= zotonic_site_testsandbox.

scan_lib_dir(Directory) ->
    Apps = filelib:wildcard( filename:join(Directory, "*") ),
    Apps1 = lists:filter(
        fun(AppDir) ->
            Basename = filename:basename(AppDir),
            hd(Basename) =/= $.
            andalso lists:last(Basename) =/= $~
            andalso lists:last(Basename) =/= $#
            andalso filelib:is_dir(AppDir)
            andalso filelib:is_dir( filename:join(AppDir, "priv") )
        end,
        Apps),
    lists:filtermap( fun scan_app_dir/1, Apps1 ).

scan_app_dir(AppDir) ->
    ensure_code_path(AppDir),
    App = z_convert:to_atom( filename:basename(AppDir) ),
    scan_app(App).

scan_app(App) ->
    case z_sites_config:config_files(App) of
        [] -> false;
        Fs ->
            case z_sites_config:read_configs(Fs) of
                {ok, Map} ->
                    _ = application:load(App),
                    Map1 = Map#{ site => App },
                    {true, to_list(Map1)};
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        text => <<"Error reading config files">>,
                        in => zotonic_core,
                        app => App,
                        result => error,
                        reason => Reason
                    }),
                    false
            end
    end.

to_list(Map) ->
    L = maps:to_list(Map),
    lists:map(
        fun
            ({K, M}) when is_map(M) ->
                {K, maps:to_list(M)};
            (KV) ->
                KV
        end,
        L).

ensure_code_path(SitePath) ->
    Ebin = filename:join(SitePath, "ebin"),
    case lists:member(Ebin, code:get_path()) of
        false -> code:add_pathz(Ebin);
        true -> ok
    end.

do_get_sites_hosts(Sites) ->
    FallbackSite = do_get_fallback_site(Sites),
    maps:map(
        fun(Site, SiteStatus) ->
            do_get_sites_hosts_1(SiteStatus, Site =:= FallbackSite)
        end,
        Sites).

do_get_sites_hosts_1(#site_status{ status = Status, config = Cfg }, IsFallback) ->
    HostPrioList = hosts_from_config(Cfg),
    case IsFallback of
        true ->
            {Status, HostPrioList ++ [{<<"*">>, 99}], do_is_site_redirect(Cfg)};
        false ->
            {Status, HostPrioList, do_is_site_redirect(Cfg)}
    end.

hosts_from_config(Config) ->
    Hostname = proplists:get_value(hostname, Config),
    HostAlias = case proplists:get_value(hostalias, Config, []) of
        List when is_list(List) -> ensure_alias_list(List);
        _ -> []
    end,
    HostSmtp = proplists:get_value(smtphost, Config),
    Hs = [{Hostname, 1}]
        ++ [ {Alias, 2} || Alias <- HostAlias ]
        ++ [ {HostSmtp, 3} ],
    lists:filtermap(
        fun
            ({undefined, _}) -> false;
            ({none, _}) -> false;
            ({"", _}) -> false;
            ({<<"">>, _}) -> false;
            ({Host, Prio}) -> {true, {z_convert:to_binary(Host), Prio}}
        end,
        Hs).

do_is_site_redirect(Cfg) ->
    case proplists:get_value(redirect, Cfg, true) of
        true -> true;
        false -> false;
        undefined -> true
    end.

% Handle the case where a user just gives a single hostname.
ensure_alias_list([C|_] = Alias) when is_integer(C) -> [Alias];
ensure_alias_list(Alias) -> Alias.

%% @doc Check which site will act as fallback site
do_get_fallback_site(Sites) ->
    case has_zotonic_site(Sites) of
        true -> zotonic_site_status;
        false -> do_get_fallback_site_1(Sites)
    end.

%% @doc Return the zotonic_site_status, or any running site.
do_get_fallback_site_1(Sites) ->
    maps:fold(
        fun
            (Site, #site_status{status = running}, undefined) ->
                Site;
            (_Site, _SiteStatus, FirstFoundSite) ->
                FirstFoundSite
        end,
        undefined,
        Sites).

%% @doc Check if the 'zotonic_site_status' is one of the running sites
has_zotonic_site(#{ zotonic_site_status := #site_status{ is_enabled = IsEnabled } }) ->
    IsEnabled;
has_zotonic_site(_) ->
    false.


%% @doc Queue new sites for starting, stop removed sites.
-spec do_upgrade(CurrentSites :: map()) -> NewSites :: map().
do_upgrade(CurrentSites) ->
    AvailableSites = do_scan_sites(),
    AvailSiteNames = maps:keys(AvailableSites),
    CurrSiteNames = maps:keys(CurrentSites),
    ToStop = CurrSiteNames -- AvailSiteNames,
    ToStart = AvailSiteNames -- CurrSiteNames,
    CurrentSites1 = do_stop_removed_sites(CurrentSites, ToStop),
    do_add_new_sites(CurrentSites1, ToStart, AvailableSites).

do_stop_removed_sites(Sites, []) ->
    Sites;
do_stop_removed_sites(Sites, [Stop|Other]) ->
    #{ Stop := SiteStatus } = Sites,
    Sites1 = case SiteStatus#site_status.pid of
        undefined ->
            maps:remove(Stop, Sites);
        Pid ->
            SiteStatus1 = #site_status{ status = removing },
            z_sites_sup:stop_site(Pid),
            Sites#{ Stop => SiteStatus1 }
    end,
    do_stop_removed_sites(Sites1, Other).

do_add_new_sites(Sites, [], _SiteConfigs) ->
    Sites;
do_add_new_sites(Sites, [Start|Other], SiteConfigs) ->
    #{ Start := Config } = SiteConfigs,
    SiteStatus = new_site_status(Start, initial_status(Config), Config),
    Sites1 = Sites#{ Start => SiteStatus },
    do_add_new_sites(Sites1, Other, SiteConfigs).


%% @doc Check if the current beam is running the testsandbox
is_testsandbox_node() ->
    [Base|_] = string:tokens(atom_to_list(node()), "@"),
    case lists:last(string:tokens(Base, "_")) of
        "testsandbox" -> true;
        _ -> false
    end.


%% @doc Handle the load of a module by the code_server, maybe reattach observers.
do_load_module(Module, State) ->
    ?LOG_DEBUG(#{
        text => <<"Reloading module">>,
        in => zotonic_core,
        module => Module
    }),
    do_load_module(is_running_site(Module, State), is_module(Module), Module, State).

do_load_module(true, _IsModule, Site, _State) ->
    try
        z_module_manager:module_reloaded(Site, z_context:new(Site))
    catch
        _:_ ->
            ok
    end;
do_load_module(false, true, Module, #state{ sites = Sites }) ->
    Running = maps:fold(
        fun
            (Site, #site_status{ status = running }, Acc) -> [ Site | Acc ];
            (_Site, _SiteStatus, Acc) -> Acc
        end,
        [],
        Sites),
    lists:foreach(
        fun(Site) ->
            try
                z_module_manager:module_reloaded(Module, z_context:new(Site))
            catch
                _:_ ->
                    ok
            end
        end,
        Running);
do_load_module(false, false, _Module, _State) ->
    ok.

is_running_site(Module, #state{ sites = Sites }) ->
    case maps:find(Module, Sites) of
        {ok, #site_status{ status = running }} -> true;
        {ok, _} -> false;
        error -> false
    end.

is_module(Module) ->
    case atom_to_list(Module) of
        "mod_" ++ _ -> true;
        ModS -> string:str(ModS, "_mod_") > 0
    end.
