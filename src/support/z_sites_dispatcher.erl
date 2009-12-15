%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-12-15
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2009 Marc Worrell
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

-module(z_sites_dispatcher).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
	dispatch/3,
	set_dispatch_rules/1
]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

-record(state, {rules}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).



%% @doc Match the host and path to a dispatch rule.
%% @spec dispatch(Host::string(), Path::string(), Req::webmachine_request) -> {dispatch(), Req2::webmachine_request}
%% @type dispatch() -> {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens}
%%                   | {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath}
%%                   | handled
dispatch(Host, Path, Req) ->
    gen_server:call(?MODULE, {dispatch, Host, Path, Req}).


%% @doc Store a new set of dispatch rules, called when a site refreshes its modules or when a site is restarted.
set_dispatch_rules(DispatchRules) ->
	gen_server:cast(?MODULE, {set_dispatch_rules, DispatchRules}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    application:set_env(webmachine, dispatcher, ?MODULE),
    {ok, #state{rules=[]}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Match a host/path to the dispatch rules.  Return a match result or a no_dispatch_match tuple.
handle_call({dispatch, HostAsString, PathAsString, Req}, _From, State) ->
    Reply = case get_host_dispatch_list(HostAsString, State#state.rules, Req) of
        {ok, Host, DispatchList} ->
            {ok, Req1a} = Req:set_metadata(zotonic_host, Host),
            Req1b = {webmachine_request, Req1a},
            case webmachine_dispatcher:dispatch(PathAsString, DispatchList) of
                {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens} ->
                    {{no_dispatch_match, undefined, undefined}, Req1b};
                {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} ->
                    {{Mod, ModOpts, HostTokens, Port, PathTokens, [{zotonic_host, Host}|Bindings], AppRoot, StringPath}, Req1b}
            end;
        {redirect, Hostname} ->
            %% Redirect to another host name.
            {RawPath, _} = Req:raw_path(),
            Uri = "http://" ++ Hostname ++ RawPath,
            {ok, Req1a} = Req:add_response_header("Location", Uri),
            Req1b = {webmachine_request, Req1a},
            {ok, Req2a} = Req1b:send_response(301),
            Req2b = {webmachine_request, Req2a},
            {LogData, _} = Req2b:log_data(),
            LogModule = 
                case application:get_env(webmachine,webmachine_logger_module) of
                    {ok, Val} -> Val;
                    _ -> webmachine_logger
                end,
            spawn(LogModule, log_access, [LogData]),
            handled;
        no_host_match ->
            {{no_dispatch_match, undefined, undefined}, Req}
    end,
    {reply, Reply, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Load a new set of dispatch rules.
handle_cast({set_dispatch_rules, Rules}, State) ->
    {noreply, State#state{rules=Rules}};

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


%% @doc Fetch the host and dispatch list for the request
%% @spec get_host_dispatch_list(webmachine_request()) -> {ok, Host::atom(), DispatchList::list()} | {redirect, Hostname::string()} | no_host_match
get_host_dispatch_list(WMHost, DispatchList, Req) ->
    case DispatchList of
        [#wm_host_dispatch_list{}|_] ->
            {Host, Port} = split_host(WMHost),
            case get_dispatch_host(Host, DispatchList) of
                {ok, DL} ->
                    {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list};

                undefined ->
                    FoundHost = case get_dispatch_alias(Host, DispatchList) of
                                    {ok, _} = Found -> Found;
                                    undefined -> get_dispatch_default(DispatchList)
                                end,
                    case FoundHost of
                        {ok, DL} ->
                            {Method, _} = Req:method(),
                            case DL#wm_host_dispatch_list.redirect andalso is_hostname(DL#wm_host_dispatch_list.hostname) andalso Method =:= 'GET' of
                                true ->
                                    % Redirect, keep the port number
                                    Hostname = DL#wm_host_dispatch_list.hostname,
                                    Hostname1 = case Port of
                                                    "80" -> Hostname;
                                                    _ -> Hostname ++ [$:|Port]
                                                end,
                                    {redirect, Hostname1};
                                false ->
                                    {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list}
                            end;
                        undefined ->
                            no_host_match
                    end
            end;
        _ ->
            {ok, default, DispatchList} 
    end.


split_host(Host) ->
    case Host of
        undefined -> 
            {"", "80"};
        _ -> 
            % Split the optional port number from the host name
            [H|Rest] = string:tokens(string:to_lower(Host), ":"),
            case Rest of
                [] -> {H, "80"};
                [Port|_] -> {H, Port}
            end
    end.


%% @doc Search the host where the main hostname matches the requested host
get_dispatch_host(_Host, []) ->
    undefined;
get_dispatch_host(Host, [#wm_host_dispatch_list{hostname=Host} = DL|_]) ->
    {ok, DL};
get_dispatch_host(Host, [_|Rest]) ->
    get_dispatch_host(Host, Rest).


%% @doc Search the host where the req hostname is an alias of main host
get_dispatch_alias(_Host, []) ->
    undefined;
get_dispatch_alias(Host, [#wm_host_dispatch_list{hostalias=Alias} = DL|Rest]) ->
    case lists:member(Host, Alias) of
        true  -> {ok, DL};
        false -> get_dispatch_alias(Host, Rest)
    end.


%% @doc Search the host with the name 'default' for fallback of unknown hostnames.
get_dispatch_default([]) ->
    undefined;
get_dispatch_default([#wm_host_dispatch_list{host=default} = DL|_]) ->
    {ok, DL};
get_dispatch_default([_|Rest]) ->
    get_dispatch_default(Rest).


%% @doc Check if the hostname is a hostname suitable to redirect to
is_hostname(undefined) -> false;
is_hostname("") -> false;
is_hostname("localhost") -> false;
is_hostname("127.0.0.1") -> false;
is_hostname(_) -> true.
