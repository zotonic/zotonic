%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%% @doc Server managing all sites running inside Zotonic.  Starts the sites
%% according to the config files in the sites subdirectories.

%% Copyright 2009-2010 Marc Worrell
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
    get_sites_all/0,
    get_sites_status/0,
    get_site_status/1,
    get_site_contexts/0,
    get_site_config/1,
    get_fallback_site/0,
    get_builtin_sites/0,
    all_sites_running/0, all_sites_running/1,
    module_loaded/1,
    info/0,
    foreach/1,

    stop/1,
    start/1,
    restart/1,
    await_startup/1,

    get_site_config_overrides/1,
    put_site_config_overrides/2
]).


-include_lib("zotonic.hrl").

-record(state, {sup}).

-define(SITES_START_TIMEOUT,  3600000).  % 1 hour
-define(SITES_SUPERVISOR, 'z_sites_manager$supervisor').

-type site_status() :: waiting | running | retrying | failed | stopped.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, ?SITES_START_TIMEOUT}]).


%% @doc Sync the supervised sites with the sites in the sites directory.
%% Removes and stops deleted sites, adds (but does not start) new sites.
upgrade() ->
    gen_server:cast(?MODULE, upgrade).


%% @doc Return a list of active site names.
-spec get_sites() -> [ atom() ].
get_sites() ->
    z_supervisor:running_children(?SITES_SUPERVISOR).

%% @doc Return a list of all site names.
-spec get_sites_all() -> [ atom() ].
get_sites_all() ->
    lists:flatten(
        lists:map(fun({_State,Children}) ->
                    [ Name || {Name,_Spec,_RunState,_Time} <- Children ]
                  end,
                  z_supervisor:which_children(?SITES_SUPERVISOR))).

%% @doc Get the status of a particular site
get_site_status(#context{site=Site}) ->
    get_site_status(Site);
get_site_status(Site) when is_atom(Site) ->
    gen_server:call(?MODULE, {site_status, Site}).

%% @doc Return a list of all sites and their status.
-spec get_sites_status() -> list().
get_sites_status() ->
    gen_server:call(?MODULE, get_sites_status).

%% @doc Return information on all running sites.
-spec info() -> [ {atom(), integer()} ].
info() ->
    gen_server:call(?MODULE, info).

%% @doc Return true iff all sites are running.
-spec all_sites_running() -> boolean().
all_sites_running() ->
    all_sites_running(info()).

-spec all_sites_running([ {atom(), integer()} ]) -> boolean().
all_sites_running(StateInfo) ->
    all_sites_running(StateInfo, true).

all_sites_running([], true) -> true;
all_sites_running([{State, Count}| _Rest], true) when Count > 0 andalso State =/= running ->
    false;
all_sites_running([_Info|Rest], true) ->
    all_sites_running(Rest, true).

%% @doc Do something for all sites that are currently running.
foreach(Fun) ->
    lists:foreach(fun(Site) ->
                    try
                        Fun(z_context:new(Site))
                    catch
                        _:_ -> ok
                    end
                  end,
                  get_sites()).

%% @doc Return a list of contexts initialized for all active sites.
-spec get_site_contexts() -> [ #context{} ].
get_site_contexts() ->
    [ z_context:new(Name) || Name <- get_sites() ].

%% @doc Fetch the configuration of a specific site.
-spec get_site_config(atom()) -> {ok, list()} | {error, term()}.
get_site_config(Site) ->
    case parse_config(get_site_config_file(Site)) of
        {ok, Config} ->
            {ok, z_utils:props_merge(get_site_config_overrides(Site), merge_os_env(Config))};
        Other ->
            Other
    end.

%% @doc Resolve {env, "ENV_NAME"} tuples from site config into the site configuration.
merge_os_env(SiteConfig) ->
    lists:map(
      fun({K, {env, Name}}) -> {K, os:getenv(Name)};
         ({K, {env, Name, Default}}) -> {K, os:getenv(Name, Default)};
         ({K, {env_int, Name}}) -> {K, z_convert:to_integer(os:getenv(Name))};
         ({K, {env_int, Name, Default}}) -> {K, z_convert:to_integer(os:getenv(Name, Default))};
         ({K, V}) -> {K, V}
      end, SiteConfig).


%% @doc Return the name of the site to handle unknown Host requests
-spec get_fallback_site() -> atom() | undefined.
get_fallback_site() ->
    Sites = scan_sites(),
    case has_zotonic_site(Sites) of
        true -> zotonic_status;
        false -> get_fallback_site(Sites)
    end.

%% @doc The list of builtin sites, they are located in the priv/sites directory.
-spec get_builtin_sites() -> [ atom() ].
get_builtin_sites() ->
    [zotonic_status, testsandbox].

%% @doc Stop a site or multiple sites.
stop([Node, Site]) ->
    rpc:call(Node, ?MODULE, stop, [Site]);
stop(Site) ->
    gen_server:cast(?MODULE, {stop, Site}).

%% @doc Start a site or multiple sites.
start([Node, Site]) ->
    rpc:call(Node, ?MODULE, start, [Site]);
start(Site) ->
    gen_server:cast(?MODULE, {start, Site}).

%% @doc Restart a site or multiple sites.
restart([Node, Site]) ->
    rpc:call(Node, ?MODULE, restart, [Site]);
restart(Site) ->
    gen_server:cast(?MODULE, {restart, Site}).

%% @doc Tell the sites manager that a module was loaded, check
%%      changes to observers, schema.
module_loaded(Module) ->
    gen_server:cast(?MODULE, {module_loaded, Module}).


%% @doc Wait for a site to complete its startup sequence. Note - due
%% to the way the site startup works currently, we cannot know whether
%% the site has already started or not. Therefore, this function
%% should only be called when you are certain the site has not
%% completed starting up, otherwise it will block infinitely.
await_startup(Context = #context{}) ->
    await_startup(z_context:site(Context));
await_startup(Site) when is_atom(Site) ->
    case get_site_status(Site) of
        {ok, running} ->
            % Now wait for the sites modules to be started
            Context = z_context:new(Site),
            z_module_manager:await_upgrade(Context);
        {ok, failed} ->
            {error, failed};
        {ok, stopped} ->
            {error, stopped};
        {ok, retrying} ->
            timer:sleep(1000),
            await_startup(Site);
        {error, _} = Error ->
            Error
    end.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([]) ->
    {ok, Sup} = z_supervisor:start_link({local, ?SITES_SUPERVISOR}, []),
    z_supervisor:set_manager_pid(Sup, self()),
    ets:new(?MODULE_INDEX, [set, public, named_table, {keypos, #module_index.key}]),
    ets:new(?MEDIACLASS_INDEX, [set, public, named_table, {keypos, #mediaclass_index.key}]),
    add_sites_to_sup(Sup, scan_sites()),
    {ok, #state{sup=Sup}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Return all sites
handle_call(get_sites_status, _From, State) ->
    Grouped = z_supervisor:which_children(State#state.sup),
    Ungrouped = lists:foldr(fun({Status, Sites}, Acc) ->
                                    [begin
                                         [Name|Rest] = tuple_to_list(Site),
                                         [Name,Status|Rest]
                                     end || Site <- Sites] ++ Acc end,
                            [],
                            Grouped),
    {reply, lists:sort(Ungrouped), State};

%% @doc Are all sites running?
handle_call(info, _From, State) ->
    Grouped = z_supervisor:which_children(State#state.sup),
    {reply, info(Grouped), State};

%% @doc Are all sites running?
handle_call({site_status, Site}, _From, State) ->
    SiteStatus = handle_site_status(Site, State),
    {reply, SiteStatus, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Sync known sites with loaded sites
handle_cast(upgrade, State) ->
    {noreply, handle_upgrade(State)};

%% @doc Stop a site, assume it is a known site.
handle_cast({stop, Site}, State) ->
    z_supervisor:stop_child(State#state.sup, Site),
    {noreply, State};

%% @doc Start a site, assume it is a known site.
handle_cast({start, Site}, State) ->
    z_supervisor:start_child(State#state.sup, Site, infinity),
    {noreply, State};

%% @doc Start a site, assume it is a known site.
handle_cast({restart, Site}, State) ->
    z_supervisor:restart_child(State#state.sup, Site),
    {noreply, State};

%% @doc A site started - report
handle_cast({supervisor_child_started, Child, SitePid}, State) ->
    lager:info("Site started: ~p (~p)", [Child#child_spec.name, SitePid]),
    z_sites_dispatcher:update_dispatchinfo(),
    {noreply, State};

%% @doc A site stopped - report
handle_cast({supervisor_child_stopped, Child, SitePid}, State) ->
    lager:info("Site stopped: ~p (~p)", [Child#child_spec.name, SitePid]),
    z_sites_dispatcher:update_dispatchinfo(),
    {noreply, State};

%% @doc Handle load of a module, check observers and schema
handle_cast({module_loaded, Module}, State) ->
    do_load_module(Module, State),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    ?DEBUG({z_sites_manager, _Info}),
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

%% @doc Get the running status of a particular site
-spec handle_site_status(atom(), #state{}) -> {ok, site_status()} | {error, notfound}.
handle_site_status(Site, State) ->
    Grouped = z_supervisor:which_children(State#state.sup),
    handle_site_status_1(Site, Grouped).

handle_site_status_1(_Site, []) ->
    {error, notfound};
handle_site_status_1(Site, [{Status, Sites}|Statuses]) ->
    case lists:keymember(Site, 1, Sites) of
        true -> {ok, Status};
        false -> handle_site_status_1(Site, Statuses)
    end.

%% @doc Get the file path of the config file for a site.
get_site_config_file(Site) ->
    case lists:member(Site, get_builtin_sites()) of
        true ->
            filename:join([z_utils:lib_dir(priv), "sites", Site, "config"]);
        false ->
            filename:join([z_path:user_sites_dir(), Site, "config"])
    end.


%% @doc Scan all sites subdirectories for the site configurations.
-spec scan_sites() -> [ list() ].
scan_sites() ->
    scan_sites(is_testsandbox()).

scan_sites(true) ->
    Sites = [testsandbox],
    ConfigFiles = [get_site_config_file(Site) || Site <- Sites],
    ParsedConfigs = [parse_config(CfgFile) || CfgFile <- ConfigFiles],
    [SiteConfig || {ok, SiteConfig} <- ParsedConfigs];
scan_sites(false) ->
    BuiltinSites = get_builtin_sites() -- [testsandbox],
    Builtin = [ parse_config(get_site_config_file(Builtin)) || Builtin <- BuiltinSites ],
    [ BuiltinCfg || {ok, BuiltinCfg} <- Builtin ] ++ scan_directory(z_path:user_sites_dir()).

scan_directory(Directory) ->
    ConfigFiles = z_utils:wildcard(filename:join([Directory, "*", "config"])),
    ParsedConfigs = [ parse_config(CfgFile) || CfgFile <- ConfigFiles ],
    [ SiteConfig || {ok, SiteConfig} <- ParsedConfigs ].

parse_config(CfgFile) ->
    SitePath = filename:dirname(CfgFile),
    Site = z_convert:to_atom(filename:basename(SitePath)),
    ConfigFiles = [ CfgFile | config_d_files(SitePath) ],
    parse_config(ConfigFiles, [{site, Site}]).

%% @doc Parse configurations from multiple files, merging results. The last file wins.
parse_config([], SiteConfig) ->
    {ok, SiteConfig};
parse_config([C|T], SiteConfig) ->
    case file:consult(C) of
        {ok, [NewSiteConfig|_]} when is_list(NewSiteConfig) ->
            SortedNewConfig = lists:ukeysort(1, NewSiteConfig),
            MergedConfig = lists:ukeymerge(1, SortedNewConfig, SiteConfig),
            parse_config(T, MergedConfig);
        {ok, [NotAList|_]} ->
            lager:error("Expected a list in the site config ~s but got ~p",
                        [C, NotAList]),
            parse_config(T, SiteConfig);
        {error, Reason} = Error ->
            lager:error("Could not consult site config: ~s: ~s",
                        [C, unicode:characters_to_binary(file:format_error(Reason))]),
            Error
    end.

%% @doc Get site config.d contents in alphabetical order.
%% Filter out files starting with '.' or ending with '~'.
config_d_files(SitePath) ->
    Path = filename:join([SitePath, "config.d", "*"]),
    lists:sort([ F || F <- z_utils:wildcard(Path),
                      filelib:is_regular(F),
                      lists:nth(1, filename:basename(F)) =/= $.,
                      lists:last(filename:basename(F)) =/= $~ ]).


has_zotonic_site([]) ->
    false;
has_zotonic_site([SiteProps|Rest]) ->
    case proplists:get_value(site, SiteProps) of
        zotonic_status -> proplists:get_value(enabled, SiteProps, false);
        _ -> has_zotonic_site(Rest)
    end.

%% @todo Make this the 'first' running site, not the first enabled site.
get_fallback_site([]) ->
    undefined;
get_fallback_site([SiteProps|Rest]) ->
    case proplists:get_value(enabled, SiteProps, false) of
        true ->
            {site, Name} = proplists:lookup(site, SiteProps),
            Name;
        false ->
            get_fallback_site(Rest)
    end.

%% @doc Initialisation: add all sites to the sites supervisor, start the enabled sites.
add_sites_to_sup(_Sup, []) ->
    ok;
add_sites_to_sup(Sup, [SiteProps|Rest]) ->
    case proplists:lookup(site, SiteProps) of
        {site, Name} ->
            Spec = #child_spec{name=Name, mfa={z_site_sup, start_link, [Name]}},
            ok = z_supervisor:add_child(Sup, Spec),
            case proplists:get_value(enabled, SiteProps, false) of
                true -> z_supervisor:start_child(Sup, Name, infinity);
                false -> leave_in_stop_state
            end;
        _ ->
            ?DEBUG({error, {missing_host, SiteProps}})
    end,
    add_sites_to_sup(Sup, Rest).


%% @spec handle_upgrade(State) -> ok
%% @doc Add children if necessary, do not start them yet.
handle_upgrade(State) ->
    SiteProps = scan_sites(),
    Old = sets:from_list([Name || Name <- supervised_sites(State#state.sup)]),
    New = sets:from_list([Name || Name <- hosted_sites(SiteProps)]),
    Kill = sets:subtract(Old, New),
    Add = sets:subtract(New, Old),

    sets:fold(fun (Name, ok) ->
                z_supervisor:delete_child(State#state.sup, Name),
                ok
              end, ok, Kill),

    sets:fold(fun (Name, ok) ->
                CS = #child_spec{name=Name, mfa={z_site_sup, start_link, [Name]}},
                z_supervisor:add_child(State#state.sup, CS),
                ok
              end, ok, Add),
    State.


supervised_sites(Sup) ->
    names(z_supervisor:which_children(Sup), []  ).

    names([], Acc) ->
        Acc;
    names([{_RunState,CS}|Rest], Acc) ->
        Names = [ Name || {Name, _Child, _Pid, _Time} <- CS ],
        names(Rest, Acc ++ Names).

hosted_sites(SiteProps) ->
    L = [ proplists:get_value(site, Props) || Props <- SiteProps ],
    [ Name || Name <- L, Name /= undefined ].

info(Grouped) ->
    info(Grouped, []).

info([], Info) ->
    Info;
info([{State, L} | Rest], Info) ->
    info(Rest, [{State, length(L)} | Info]).

%% @doc Check if the current beam is running the testsandbox
is_testsandbox() ->
    [Base|_] = string:tokens(atom_to_list(node()), "@"),
    case lists:last(string:tokens(Base, "_")) of
        "testsandbox" -> true;
        _ -> false
    end.


%% @doc Handle the load of a module by the code_server, maybe reattach observers.
do_load_module(Module, State) ->
    lager:debug("z_sites_manager: reloading ~p", [Module]),
    do_load_module(is_running_site(Module, State), is_module(Module), Module, State).

do_load_module(true, _IsModule, Site, _State) ->
    try
        z_module_manager:module_reloaded(Site, z_context:new(Site))
    catch
        _:_ ->
            ok
    end;
do_load_module(false, true, Module, State) ->
    Grouped = z_supervisor:which_children(State#state.sup),
    Running = proplists:get_value(running, Grouped, []),
    lists:foreach(fun(SiteChild) ->
                    try
                        Site = element(1, SiteChild),
                        z_module_manager:module_reloaded(Module, z_context:new(Site))
                    catch
                        _:_ ->
                            ok
                    end
                  end,
                  Running);
do_load_module(false, false, _Module, _State) ->
    ok.

is_running_site(Module, State) ->
    {ok, running} =:= handle_site_status(Module, State).

is_module(Module) ->
    case atom_to_list(Module) of
        "mod_"++_ -> true;
        _ -> false
    end.

get_site_config_overrides(Site) when is_atom(Site) ->
    Key = z_convert:to_atom(z_convert:to_list(Site) ++ "_config_overrides"),
    application:get_env(zotonic, Key, []).

%% @doc Override a given site config with arbitrary key/value
%% pairs. Should be called before the site is started.
put_site_config_overrides(Site, Overrides) when is_atom(Site), is_list(Overrides) ->
    Key = z_convert:to_atom(z_convert:to_list(Site) ++ "_config_overrides"),
    application:set_env(zotonic, Key, Overrides).
