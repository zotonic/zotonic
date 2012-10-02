%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% Date: 2009-12-15
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2009-2011 Marc Worrell
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

% Authors of the Webmachine dispatch matcher.
-author('Robert Ahrens <rahrens@basho.com>').
-author('Justin Sheehy <justin@basho.com>').
-author('Bryan Fink <bryan@basho.com>').

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
         dispatch/3,
         get_fallback_site/0,
         get_host_for_domain/1,
         update_dispatchinfo/0
        ]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

-record(state, {rules, fallback_site}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).



%% @doc Update the webmachine dispatch information. Collects dispatch information from all sites and sends it
%% to webmachine for updating its dispatch lists and host information.
update_dispatchinfo() ->
    gen_server:cast(?MODULE, update_dispatchinfo).


%% @doc Match the host and path to a dispatch rule.
%% @spec dispatch(Host::string(), Path::string(), ReqData::wm_reqdata) -> {dispatch(), NewReqData}
%% @type dispatch() = {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens}
%%                  | {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath}
%%                  | handled
dispatch(Host, Path, ReqData) ->
    % Classify the user agent
    {ok, ReqDataUA} = z_user_agent:set_class(ReqData),
    Method = wrq:method(ReqData),
    Protocol = case wrq:is_ssl(ReqData) of true -> https; false -> http end,
    % Find a matching dispatch rule 
    DispReq = #dispatch{host=Host, path=Path, method=Method, protocol=Protocol},
    case gen_server:call(?MODULE, DispReq) of
        {no_dispatch_match, MatchedHost, NonMatchedPathTokens, Bindings} when MatchedHost =/= undefined ->
            {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqDataUA),

            Context = case lists:keyfind(z_language, 1, Bindings) of
                          {z_language, Language} -> z_context:set_language(list_to_atom(Language), z_context:new(MatchedHost));
                          false -> z_context:new(MatchedHost)
                      end,
            RewrittenPath = case NonMatchedPathTokens of 
                                undefined -> Path;
                                _ -> string:join(NonMatchedPathTokens, "/")
                            end,
            case z_notifier:first(DispReq#dispatch{path=RewrittenPath}, Context#context{wm_reqdata=ReqDataHost}) of
                {ok, Id} when is_integer(Id) ->
                    %% Retry with the resource's default page uri
                    DefaultPagePath = binary_to_list(m_rsc:p_no_acl(Id, default_page_url, Context)),
                    case gen_server:call(?MODULE, DispReq#dispatch{path=DefaultPagePath}) of
                        {no_dispatch_match, _, _, _} = M ->
                            {M, ReqDataHost};
                        {redirect, ProtocolAsString, Hostname} ->
                            {handled, redirect(false, ProtocolAsString, Hostname, ReqDataUA)};
                        {Match, MatchedHost} ->
                            {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqDataUA),
                            {Match, ReqDataHost}
                    end;
                {ok, #dispatch_match{
                        dispatch_name=SDispatchName,
                        mod=SMod,
                        mod_opts=SModOpts,
                        path_tokens=SPathTokens,
                        bindings=SBindings,
                        app_root=SAppRoot,
                        string_path=SStringPath}} ->
                    {{SMod, SModOpts, 
                      [], none, % Host info
                      SPathTokens, [{zotonic_dispatch, SDispatchName},{zotonic_host, MatchedHost}|SBindings] ++ Bindings, 
                      SAppRoot, SStringPath}, 
                     ReqDataHost};
                {ok, #dispatch_redirect{location=Location, is_permanent=IsPermanent}} ->
                    {handled, redirect(IsPermanent, Location, ReqDataHost)};
                undefined ->
                    {{no_dispatch_match, MatchedHost, NonMatchedPathTokens}, ReqDataHost}
            end;

        {redirect, ProtocolAsString, Hostname} ->
            {handled, redirect(false, ProtocolAsString, Hostname, ReqDataUA)};

        {Match, MatchedHost} ->
            {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqDataUA),
            {Match, ReqDataHost};
        
        no_host_match ->
            {{no_dispatch_match, undefined, undefined, []}, undefined}
    end.


%% @doc Retrieve the fallback site.
get_fallback_site() ->
    gen_server:call(?MODULE, {get_fallback_site}).

%% @doc Fetch the host from the given domain name
get_host_for_domain(Domain) when is_binary(Domain) ->
    get_host_for_domain(binary_to_list(Domain));
get_host_for_domain(Domain) ->
    gen_server:call(?MODULE, {get_host_for_domain, Domain}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    {ok, #state{rules=collect_dispatchrules(), fallback_site=z_sites_manager:get_fallback_site()}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Match a host/path to the dispatch rules.  Return a match result or a no_dispatch_match tuple.
handle_call(#dispatch{host=HostAsString, path=PathAsString, method=Method, protocol=Protocol}, _From, State) ->
    Reply = case get_host_dispatch_list(HostAsString, State#state.rules, State#state.fallback_site, Method) of
                {ok, Host, DispatchList} ->
                    case wm_dispatch(Protocol, HostAsString, Host, PathAsString, DispatchList) of
                        {redirect, _ProtocolAsString, _Hostname} = R ->
                            R;
                        {no_dispatch_match, UnmatchedPathTokens, Bindings} ->
                            {no_dispatch_match, Host, UnmatchedPathTokens, Bindings};
                        {DispatchName, Mod, ModOpts, PathTokens, Bindings, AppRoot, StringPath} ->
                            {{Mod, ModOpts, 
                              [], none, % Host info
                              PathTokens, [{zotonic_dispatch, DispatchName},{zotonic_host, Host}|Bindings], 
                              AppRoot, StringPath}, 
                             Host}
                    end;
                {redirect, Hostname} ->
                    ProtocolAsString = case Protocol of
                                           https ->"https";
                                           http -> "http"
                                       end,
                    {redirect, ProtocolAsString, Hostname};
                no_host_match ->
                    no_host_match
            end,
    {reply, Reply, State};

%% @doc Return the fallback site
handle_call({get_fallback_site}, _From, State) ->
    {reply, State#state.fallback_site, State};


%% @doc Find the host that handles the given domain name
handle_call({get_host_for_domain, Domain}, _From, State) ->
    {reply, handle_host_for_domain(Domain, State#state.rules), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Reloads the dispatch rules.
%% @todo Do SSL filtering per host (instead of on a system wide basis).
handle_cast(update_dispatchinfo, State) ->
    {noreply, State#state{rules=collect_dispatchrules()}};


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


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


%% @doc Collect all dispatch rules for all sites, normalize and filter them.
collect_dispatchrules() ->
    Rules = [ filter_rules(fetch_dispatchinfo(Site), Site) || Site <- z_sites_manager:get_sites() ],
    Rules1 = normalize_streamhosts(Rules),
    compile_regexps_hosts(Rules1).


%% @doc Fetch dispatch rules for a specific site.
fetch_dispatchinfo(Site) ->
    Name = z_utils:name_for_host(z_dispatcher, Site),
    {Host, Hostname, Streamhost, SmtpHost, Hostalias, Redirect, DispatchList} = z_dispatcher:dispatchinfo(Name),
    #wm_host_dispatch_list{
                            host=Host, hostname=Hostname, streamhost=Streamhost, smtphost=SmtpHost, hostalias=Hostalias,
                            redirect=Redirect, dispatch_list=DispatchList
                          }.


%% @doc Redirect to another host name.
redirect(IsPermanent, ProtocolAsString, Hostname, ReqData) ->
    RawPath = wrq:raw_path(ReqData),
    Uri = ProtocolAsString ++ "://" ++ Hostname ++ RawPath,
    redirect(IsPermanent, Uri, ReqData).

redirect(IsPermanent, Location, ReqData) -> 
    RD1 = wrq:set_resp_header("Location", Location, ReqData),
    {ok, RD2} = webmachine_request:send_response(case IsPermanent of true -> 301; false -> 302 end, RD1),
    LogData = webmachine_request:log_data(RD2),
    LogModule = 
        case application:get_env(webmachine,webmachine_logger_module) of
            {ok, Val} -> Val;
            _ -> webmachine_logger
        end,
    spawn(LogModule, log_access, [LogData]),
    RD2.


%% @doc Fetch the host for the given domain
handle_host_for_domain(Domain, DispatchList) ->
    {Host, _Port} = split_host(Domain),
    case get_dispatch_host(Host, DispatchList) of
        {ok, DL} ->
            {ok, DL#wm_host_dispatch_list.host};
        undefined ->
            case get_dispatch_alias(Host, DispatchList) of
                {ok, DL} ->
                    {ok, DL#wm_host_dispatch_list.host};
                undefined ->
                    undefined
            end
    end.

%% @doc Fetch the host and dispatch list for the request
%% @spec get_host_dispatch_list(WMHost, DispatchList, Fallback, Method) -> {ok, Host::atom(), DispatchList::list()} | {redirect, Hostname::string()} | no_host_match
get_host_dispatch_list(WMHost, DispatchList, Fallback, Method) ->
    case DispatchList of
        [#wm_host_dispatch_list{}|_] ->
            {Host, Port} = split_host(WMHost),
            case get_dispatch_host(Host, DispatchList) of
                {ok, DL} ->
                    {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list};

                undefined ->
                    FoundHost = case get_dispatch_alias(Host, DispatchList) of
                                    {ok, _} = Found -> Found;
                                    undefined -> get_dispatch_fallback(DispatchList, Fallback)
                                end,
                    case FoundHost of
                        {ok, DL} ->
                            case DL#wm_host_dispatch_list.redirect 
                                andalso is_hostname(DL#wm_host_dispatch_list.hostname) 
                                andalso Method =:= 'GET' 
                            of
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
                            %% Always fallback to the zotonic host
                            zotonic
                    end
            end;
        _ ->
            {ok, Fallback, DispatchList} 
    end.


split_host(Host) ->
    case Host of
        undefined -> 
            {"", "80"};
        [] -> 
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
get_dispatch_host(Host, DLs) ->
    case get_dispatch_host1(Host, DLs) of
        undefined ->
            case make_streamhost(Host) of
                Host -> undefined;
                StreamHost -> get_dispatch_host1(StreamHost, DLs)
            end;
        Found -> Found
    end.

    get_dispatch_host1(_Host, []) ->
        undefined;
    get_dispatch_host1(Host, [#wm_host_dispatch_list{hostname=Host} = DL|_]) ->
        {ok, DL};
    get_dispatch_host1(Host, [#wm_host_dispatch_list{streamhost=Host} = DL|_]) ->
        {ok, DL};
    get_dispatch_host1(Host, [#wm_host_dispatch_list{smtphost=Host} = DL|_]) ->
        {ok, DL};
    get_dispatch_host1(Host, [_|Rest]) ->
        get_dispatch_host1(Host, Rest).


%% @doc Search the host where the req hostname is an alias of main host.
get_dispatch_alias(_Host, []) ->
    undefined;
get_dispatch_alias(Host, [#wm_host_dispatch_list{hostalias=Alias} = DL|Rest]) ->
    case lists:member(Host, Alias) of
        true  -> {ok, DL};
        false -> get_dispatch_alias(Host, Rest)
    end.


%% @doc Retrieve the dispatch list of the fallback site for unknown hostnames.
get_dispatch_fallback([], _FallbackHost) ->
    undefined;
get_dispatch_fallback([#wm_host_dispatch_list{host=FallbackHost} = DL|_], FallbackHost) ->
    {ok, DL};
get_dispatch_fallback([_|Rest], FallbackHost) ->
    get_dispatch_fallback(Rest, FallbackHost).


%% @doc Check if the hostname is a hostname suitable to redirect to
is_hostname(undefined) -> false;
is_hostname("") -> false;
is_hostname("localhost") -> false;
is_hostname("127.0.0.1") -> false;
is_hostname(_) -> true.


%% @doc Compile all regexps in the dispatch lists
compile_regexps_hosts(DLs) ->
    compile_regexps_hosts(DLs, []).

compile_regexps_hosts([], Acc) ->
    lists:reverse(Acc);
compile_regexps_hosts([#wm_host_dispatch_list{dispatch_list=DispatchList} = DL|Rest], Acc) ->
    DispatchList1 = compile_regexps(DispatchList, []),
    compile_regexps_hosts(Rest, [DL#wm_host_dispatch_list{dispatch_list=DispatchList1}|Acc]).

compile_regexps([], Acc) ->
    lists:reverse(Acc);
compile_regexps([{DispatchName, PathSchema, Mod, Props}|Rest], Acc) ->
    PathSchema1 = compile_re_path(PathSchema, []),
    compile_regexps(Rest, [{DispatchName, PathSchema1, Mod, Props}|Acc]).

compile_re_path([], Acc) ->
    lists:reverse(Acc);
compile_re_path([{Token, {Mod, Fun}}|Rest], Acc) ->
    compile_re_path(Rest, [{Token, {Mod,Fun}}|Acc]);
compile_re_path([{Token, RE}|Rest], Acc) ->
    {ok, MP} = re:compile(RE),
    compile_re_path(Rest, [{Token, MP}|Acc]);
compile_re_path([{Token, RE, Options}|Rest], Acc) ->
    {CompileOpt,RunOpt} = lists:partition(fun is_compile_opt/1, Options),
    {ok, MP} = re:compile(RE, CompileOpt),
    compile_re_path(Rest, [{Token, MP, RunOpt}|Acc]);
compile_re_path([Token|Rest], Acc) ->
    compile_re_path(Rest, [Token|Acc]).

%% Only allow options valid for the re:compile/3 function.
is_compile_opt(unicode) -> true;
is_compile_opt(anchored) -> true;
is_compile_opt(caseless) -> true;
is_compile_opt(dotall) -> true;
is_compile_opt(extended) -> true;
is_compile_opt(ungreedy) -> true;
is_compile_opt(no_auto_capture) -> true;
is_compile_opt(dupnames) -> true;
is_compile_opt(_) -> false.


%% @doc Normalize streamhosts, remove any prepending # or *
normalize_streamhosts(DLs) ->
    normalize_streamhosts(DLs, []).

    normalize_streamhosts([], Acc) ->
        lists:reverse(Acc);
    normalize_streamhosts([#wm_host_dispatch_list{streamhost=Host} = DL|Rest], Acc) ->
        normalize_streamhosts(Rest, [DL#wm_host_dispatch_list{streamhost=make_streamhost(Host)}|Acc]).

make_streamhost(Hostname) ->
    case make_streamhost1(Hostname) of
        false -> Hostname;
        Other -> Other
    end.

    make_streamhost1([C|Hostname]) when C >= $0 andalso C =< $9 ->
        make_streamhost1(Hostname);
    make_streamhost1([$?|Hostname]) ->
        Hostname;
    make_streamhost1([$*|Hostname]) ->
        Hostname;
    make_streamhost1([$.|_] = Hostname) ->
        Hostname;
    make_streamhost1(_) -> 
        false.


%% @doc Filter all rules, also used to set/reset protocol (https) options.
filter_rules(Rules, Site) ->
    z_notifier:foldl(dispatch_rules, Rules, z_context:new(Site)).


%%%%%%% Adapted version of Webmachine dispatcher %%%%%%%%
% Main difference is that we want to know which dispatch rule was choosen.
% We also added check functions and regular expressions to match vars.

%% Author Robert Ahrens <rahrens@basho.com>
%% Author Justin Sheehy <justin@basho.com>
%% Copyright 2007-2009 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-define(SEPARATOR, $\/).
-define(MATCH_ALL, '*').

%% @spec wm_dispatch(Protocol, HostAsString, Host::atom(), Path::string(), DispatchList::[matchterm()]) ->
%%                                            dispterm() | dispfail()
%% @doc Interface for URL dispatching.
%% See also http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
wm_dispatch(Protocol, HostAsString, Host, PathAsString, DispatchList) ->
    Context = z_context:new(Host),
    Path = string:tokens(PathAsString, [?SEPARATOR]),
    IsDir = lists:last(PathAsString) == ?SEPARATOR,
    {Path1, Bindings} = z_notifier:foldl(#dispatch_rewrite{is_dir=IsDir, path=PathAsString}, {Path, []}, Context),
    try_path_binding(Protocol, HostAsString, Host, DispatchList, Path1, Bindings, extra_depth(Path1, IsDir), Context).

% URIs that end with a trailing slash are implicitly one token
% "deeper" than we otherwise might think as we are "inside"
% a directory named by the last token.
extra_depth([], _IsDir) -> 1;
extra_depth(_Path, true) -> 1;
extra_depth(_, _) -> 0.


try_path_binding(_Protocol, _HostAsString, _Host, [], PathTokens, Bindings, _ExtraDepth, _Context) ->
    {no_dispatch_match, PathTokens, Bindings};
try_path_binding(Protocol, HostAsString, Host, [{DispatchName, PathSchema, Mod, Props}|Rest], PathTokens, Bindings, ExtraDepth, Context) ->
    case bind(Host, PathSchema, PathTokens, Bindings, 0, Context) of
        {ok, Remainder, NewBindings, Depth} ->
            case proplists:get_value(protocol, Props) of
                % Force switch to normal http protocol
                undefined when Protocol =/= http ->
                    {Host1, HostPort} = split_host(HostAsString),
                    Host2 = add_port(http, Host1, HostPort),
                    {redirect, "http", Host2};

                % Force switch to other (eg. https) protocol
                {NewProtocol, NewPort} when NewProtocol =/= Protocol ->
                    {Host1, _Port} = split_host(HostAsString),
                    Host2 = add_port(NewProtocol, Host1, NewPort),
                    {redirect, z_convert:to_list(NewProtocol), Host2};

                % 'keep' or correct protocol
                _ ->
                    {DispatchName, Mod, Props, Remainder, NewBindings, 
                        calculate_app_root(Depth + ExtraDepth), 
                        reconstitute(Remainder)}
            end;
        fail -> 
            try_path_binding(Protocol, HostAsString, Host, Rest, PathTokens, Bindings, ExtraDepth, Context)
    end.
    
add_port(http, Host, 80) -> Host;
add_port(https, Host, 443) -> Host;
add_port(_, Host, Port) -> Host++[$:|z_convert:to_list(Port)].

bind(_Host, [], [], Bindings, Depth, _Context) ->
    {ok, [], Bindings, Depth};
bind(_Host, [?MATCH_ALL], Rest, Bindings, Depth, _Context) when is_list(Rest) ->
    {ok, Rest, Bindings, Depth + length(Rest)};
bind(_Host, _, [], _, _, _Context) ->
    fail;
bind(Host, [z_language|RestToken],[Match|RestMatch],Bindings,Depth, Context) ->
    case z_trans:is_language(Match) of
        true -> bind(Host, RestToken, RestMatch, [{z_language, Match}|Bindings], Depth + 1, Context);
        false -> fail
    end;
bind(Host, [Token|RestToken],[Match|RestMatch],Bindings,Depth, Context) when is_atom(Token) ->
    bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth + 1, Context);
bind(Host, [{Token, {Module,Function}}|RestToken],[Match|RestMatch],Bindings,Depth, Context) 
when is_atom(Token), is_atom(Module), is_atom(Function) ->
    case Module:Function(Match, Context) of
        true -> bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth + 1, Context);
        false -> fail;
        {ok, Value} -> bind(Host, RestToken, RestMatch, [{Token, Value}|Bindings], Depth+1, Context)
    end;
bind(Host, [{Token, RegExp}|RestToken],[Match|RestMatch],Bindings,Depth, Context) when is_atom(Token) ->
    case re:run(Match, RegExp) of
        {match, _} -> bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth+1, Context);
        nomatch -> fail
    end;
bind(Host, [{Token, RegExp, Options}|RestToken],[Match|RestMatch],Bindings,Depth, Context) when is_atom(Token) ->
    case re:run(Match, RegExp, Options) of
        {match, []} -> bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth+1, Context);
        {match, [T|_]} when is_tuple(T) -> bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth+1, Context);
        {match, [Captured]} -> bind(Host, RestToken, RestMatch, [{Token, Captured}|Bindings], Depth+1, Context);
        {match, Captured} -> bind(Host, RestToken, RestMatch, [{Token, Captured}|Bindings], Depth+1, Context);
        match -> bind(Host, RestToken, RestMatch, [{Token, Match}|Bindings], Depth+1, Context);
        nomatch -> fail
    end;
bind(Host, [Token|RestToken], [Token|RestMatch], Bindings, Depth, Context) ->
    bind(Host, RestToken, RestMatch, Bindings, Depth + 1, Context);
bind(_Host, _, _, _, _, _Context) ->
    fail.

reconstitute([]) -> "";
reconstitute(UnmatchedTokens) -> string:join(UnmatchedTokens, [?SEPARATOR]).

calculate_app_root(1) -> ".";
calculate_app_root(N) when N > 1 ->
    string:join(lists:duplicate(N, ".."), [?SEPARATOR]).


