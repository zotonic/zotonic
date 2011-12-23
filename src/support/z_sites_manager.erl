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
    update_dispatchinfo/0,
    get_sites/0,
    get_sites_all/0,
    get_sites_status/0,
    get_site_contexts/0,
    get_site_config/1,
    get_fallback_site/0,
    
    stop/1,
    start/1,
    restart/1
]).


-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

-record(state, {sup}).

-define(SITES_START_TIMEOUT,  3600000).  % 1 hour


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


%% @doc Update the webmachine dispatch information. Collects dispatch information from all sites and sends it
%% to webmachine for updating its dispatch lists and host information.
update_dispatchinfo() ->
    DispatchList = [ fetch_dispatchinfo(Site) || Site <- get_sites() ],
    z_sites_dispatcher:set_dispatch_rules(DispatchList),
    ok.

    fetch_dispatchinfo(Site) ->
        Name = z_utils:name_for_host(z_dispatcher, Site),
        {Host, Hostname, Streamhost, SmtpHost, Hostalias, Redirect, DispatchList} = 
			z_dispatcher:dispatchinfo(Name),
        #wm_host_dispatch_list{
            host=Host, hostname=Hostname, streamhost=Streamhost, smtphost=SmtpHost, hostalias=Hostalias,
            redirect=Redirect, dispatch_list=DispatchList
        }.


%% @doc Return a list of active site names.
%% @spec get_sites() -> [ atom() ]
get_sites() ->
    gen_server:call(?MODULE, get_sites). 

%% @doc Return a list of all site names.
%% @spec get_sites_all() -> [ atom() ]
get_sites_all() ->
    gen_server:call(?MODULE, get_sites_all). 

%% @doc Return a list of all sites and their status.
%% @spec get_sites_status() -> PropList
get_sites_status() ->
    gen_server:call(?MODULE, get_sites_status). 


%% @doc Return a list of contexts initialized for all active sites.
%% @spec get_site_contexts() -> [ Context ]
get_site_contexts() ->
    [ z_context:new(Name) || Name <- get_sites() ].


%% @doc Return the name of the site to handle unknown Host requests
%% @spec get_fallback_site() -> atom() | undefined
get_fallback_site() ->
    Sites = scan_sites(),
    case has_zotonic_site(Sites) of
        true -> zotonic_status;
        false -> get_fallback_site(Sites)
    end.
    
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


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init([]) ->
    {ok, Sup} = z_supervisor:start_link([]),
    add_sites_to_sup(Sup, scan_sites()),
    {ok, #state{sup=Sup}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Return the active sites
handle_call(get_sites, _From, State) ->
    {reply, z_supervisor:running_children(State#state.sup), State};

%% @doc Return all sites
handle_call(get_sites_all, _From, State) ->
    All = lists:flatten(
                lists:map(fun({_State,Children}) ->
                            [ Name || {Name,_Spec,_RunState,_Time} <- Children ]
                          end,
                          z_supervisor:which_children(State#state.sup))),
    {reply, All, State};


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
    z_supervisor:start_child(State#state.sup, Site),
    {noreply, State};

%% @doc Start a site, assume it is a known site.
handle_cast({restart, Site}, State) ->
    z_supervisor:restart_child(State#state.sup, Site),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
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


%% @doc Scan all sites subdirectories for the site configurations.
%% @spec scan_sites() -> [ SiteProps ]
scan_sites() ->
    SitesDir = filename:join([z_utils:lib_dir(priv), "sites", "*", "config"]),
    Configs = [ parse_config(C) || C <- filelib:wildcard(SitesDir) ],
    [ C || C <- Configs, is_list(C) ].

    parse_config(C) ->
        case file:consult(C) of
            {ok, [SiteConfig|_]} -> 
                %% check host option
                Host = list_to_atom(
                         hd(lists:reverse(
                              filename:split(
                                filename:dirname(C)
                               )))),
                case proplists:get_value(host, SiteConfig) of
                    undefined ->
                        lists:keystore(host, 1, SiteConfig, {host, Host});
                    Host ->
                        SiteConfig;
                    InvalidHost ->
                        error_logger:warning_msg("Ignoring invalid `host' option in site config: ~s: {host, ~p}~n",
                                                 [C, InvalidHost]),
                        lists:keystore(host, 1, proplists:delete(host, SiteConfig), {host, Host})
                end;
            {error, Reason} ->
                Message = io_lib:format("Could not consult site config: ~s: ~s", [C, file:format_error(Reason)]),
                ?ERROR("~s~n", [Message]),
                {error, Message}
        end.

%% @doc Fetch the configuration of a specific site.
%% @spec get_site_config(Site::atom()) -> SiteProps::list() | {error, Reason}
get_site_config(Site) ->
    ConfigFile = filename:join([z_utils:lib_dir(priv), "sites", Site, "config"]),
    parse_config(ConfigFile).


has_zotonic_site([]) ->
    false;
has_zotonic_site([SiteProps|Rest]) ->
    case proplists:get_value(host, SiteProps) of
        zotonic_status -> proplists:get_value(enabled, SiteProps, false);
        _ -> has_zotonic_site(Rest)
    end.

%% @todo Make this the 'first' running site, not the first enabled site.
get_fallback_site([]) ->
    undefined;
get_fallback_site([SiteProps|Rest]) ->
    case proplists:get_value(enabled, SiteProps, false) of
        true ->
            {host, Name} = proplists:lookup(host, SiteProps),
            Name;
        false ->
            get_fallback_site(Rest)
    end.

%% @doc Initialisation: add all sites to the sites supervisor, start the enabled sites.
add_sites_to_sup(_Sup, []) ->
    ok;
add_sites_to_sup(Sup, [SiteProps|Rest]) ->
    case proplists:lookup(host, SiteProps) of
        {host, Name} ->
            Spec = #child_spec{name=Name, mfa={z_site_sup, start_link, [Name]}},
            ok = z_supervisor:add_child(Sup, Spec),
            case proplists:get_value(enabled, SiteProps, false) of
                true -> z_supervisor:start_child(Sup, Name);
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
    L = [ proplists:get_value(host, Props) || Props <- SiteProps ],
    [ Name || Name <- L, Name /= undefined ].

