%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2013 Marc Worrell
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2009-2013 Marc Worrell
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
         dispatch/4,
         get_fallback_site/0,
         get_host_for_domain/1,
         update_dispatchinfo/0
        ]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

-record(state, {
            rules = [],
            fallback_site = zotonic_status
        }).

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
%% @spec dispatch(Host::string()|atom(), Path::string(), ReqData::wm_reqdata) -> {dispatch(), NewReqData}
%% @type dispatch() = {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens}
%%                  | {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath}
%%                  | handled
dispatch(Host, Path, ReqData) ->
    dispatch(Host, Path, ReqData, undefined).

dispatch(Host, Path, ReqData, TracerPid) ->
    % Classify the user agent
    {ok, ReqDataUA} = z_user_agent:set_class(ReqData),
    Protocol = case wrq:is_ssl(ReqData) of true -> https; false -> http end,
    DispReq = #dispatch{
                    host=Host, 
                    path=Path, 
                    method=wrq:method(ReqData), 
                    protocol=Protocol,
                    tracer_pid=TracerPid
              },
    count_request(Host),
    try
        trace_final(TracerPid, handle_dispatch(gen_server:call(?MODULE, DispReq), DispReq, ReqDataUA))
    catch
        throw:{stop_request, RespCode} ->
            {{stop_request, RespCode}, ReqData}
    end.

%% @doc Retrieve the fallback site.
get_fallback_site() ->
    gen_server:call(?MODULE, get_fallback_site).

%% @doc Fetch the host handling the given domain name
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
    gen_server:cast(self(), update_dispatchinfo),
    {ok, #state{}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Match a host/path to the dispatch rules.  Return a match result, a no_dispatch_match tuple or no_host_match
handle_call({site_dispatch, Host, Dispatch}, _From, State) ->
    case lists:keyfind(Host, #wm_host_dispatch_list.host, State#state.rules) of
        false -> 
            {reply, no_host_match, State};
        WMHost ->
            DL = WMHost#wm_host_dispatch_list.dispatch_list,
            {reply, handle_host_dispatch(Host, DL, Dispatch), State}
    end;
handle_call(#dispatch{host=HostAsString, method=Method} = Dispatch, _From, State) ->
    Reply = case get_host_dispatch_list(HostAsString, State#state.rules, Method) of
                {ok, Host, DispatchList} ->
                    handle_host_dispatch(Host, DispatchList, Dispatch);
                {redirect, _Host} = Redirect ->
                    Redirect;
                {redirect_protocol, _NewProtocol, _NewHost} = Redirect -> 
                    Redirect;
                no_host_match ->
                    no_host_match
            end,
    {reply, Reply, State};

%% @doc Return the fallback site
handle_call(get_fallback_site, _From, State) ->
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
handle_cast(update_dispatchinfo, State) ->
    {noreply, State#state{
                rules=collect_dispatchrules(), 
                fallback_site=z_sites_manager:get_fallback_site()
            }};

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

handle_host_dispatch(Host, DispatchList, #dispatch{host=HostAsString, path=PathAsString, protocol=Protocol, tracer_pid=TracerPid}) ->
    case wm_dispatch(Protocol, HostAsString, Host, PathAsString, DispatchList, TracerPid) of
        {redirect_protocol, _ProtocolAsString, _Hostname} = R ->
            R;
        {no_dispatch_match, UnmatchedPathTokens, Bindings} ->
            {no_dispatch_match, Host, UnmatchedPathTokens, Bindings};
        {DispatchName, Mod, ModOpts, PathTokens, Bindings, AppRoot, StringPath} ->
            {{Mod, ModOpts, 
              [], none, % Host info
              PathTokens, [{zotonic_dispatch, DispatchName},{zotonic_host, Host}|Bindings], 
              AppRoot, StringPath}, 
             Host}
    end.

handle_dispatch({Match, MatchedHost}, _DispReq, ReqDataUA) when is_tuple(Match) ->
    % Known host, known dispatch rule
    count_request(MatchedHost),
    {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqDataUA),
    {Match, ReqDataHost};
handle_dispatch({redirect, MatchedHost}, DispReq, ReqDataUA) when is_atom(MatchedHost) ->
    % Redirect to other host, same path
    count_request(MatchedHost),
    RawPath = wrq:raw_path(ReqDataUA),
    Uri = z_context:abs_url(RawPath, z_context:new(MatchedHost)),
    trace(DispReq#dispatch.tracer_pid, undefined, redirect, [{location, Uri},{permanent,true}]),
    {handled, redirect(true, z_convert:to_list(Uri), ReqDataUA)};
handle_dispatch({redirect, MatchedHost, NewPathOrURI, IsPermanent}, DispReq, ReqDataUA) when is_atom(MatchedHost) ->
    % Redirect to some site, new path or uri
    count_request(MatchedHost),
    AbsURI = z_context:abs_url(NewPathOrURI, z_context:new(MatchedHost)),
    trace(DispReq#dispatch.tracer_pid, undefined, redirect, [{location, AbsURI},{permanent,IsPermanent}]),
    {handled, redirect(IsPermanent, z_convert:to_list(AbsURI), ReqDataUA)};
handle_dispatch({redirect_protocol, NewProtocol, NewHost}, _DispReq, ReqDataUA) ->
    % Switch protocols (mostly http/https switch)
    {handled, redirect(false, z_convert:to_list(NewProtocol), NewHost, ReqDataUA)}; 
handle_dispatch({no_dispatch_match, MatchedHost, NonMatchedPathTokens, Bindings}, DispReq, ReqDataUA) when MatchedHost =/= undefined ->
    % Known host, unknown dispatch rule
    {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqDataUA),
    Context = context_with_language(MatchedHost, Bindings),
    RewrittenPath = case NonMatchedPathTokens of 
                        undefined -> DispReq#dispatch.path;
                        _ -> string:join(NonMatchedPathTokens, "/")
                    end,
    trace(DispReq#dispatch.tracer_pid, RewrittenPath, notify_dispatch, []),
    Redirect = z_notifier:first(DispReq#dispatch{path=RewrittenPath}, Context#context{wm_reqdata=ReqDataHost}),
    handle_rewrite(Redirect, DispReq, MatchedHost, NonMatchedPathTokens, Bindings, ReqDataHost, Context);
handle_dispatch(no_host_match, DispReq, ReqDataUA) ->
    % No host match - try to find matching host by asking all sites
    handle_no_host_match(DispReq, ReqDataUA).


context_with_language(MatchedHost, Bindings) ->
    context_set_language(MatchedHost, language_from_bindings(Bindings)).

context_set_language(MatchedHost, {ok, Language}) ->
    z_context:new(MatchedHost, Language);
context_set_language(MatchedHost, {error, _}) ->
    z_context:new(MatchedHost, undefined).

language_from_bindings(Bindings) ->
    language_from_bindings_1(lists:keyfind(z_language, 1, Bindings)).

language_from_bindings_1({z_language, Language}) ->
    z_trans:to_language_atom(Language);
language_from_bindings_1(false) ->
    {error, not_a_language}.


%% Handle possible request rewrite; used when no dispatch rule matched
handle_rewrite({ok, Id}, DispReq, MatchedHost, NonMatchedPathTokens, Bindings, ReqDataHost, Context) when is_integer(Id) ->
    %% Retry with the resource's default page uri
    case m_rsc:p_no_acl(Id, default_page_url, Context) of
        undefined ->
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id}]),
            count_request(MatchedHost),
            {{no_dispatch_match, MatchedHost, NonMatchedPathTokens}, ReqDataHost};
        DefaultPagePathBin ->
            DefaultPagePath = binary_to_list(DefaultPagePathBin),
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id},{path,DefaultPagePath}]),
            case gen_server:call(?MODULE, DispReq#dispatch{path=DefaultPagePath}) of
                {no_dispatch_match, MatchedHost1, NonMatchedPathTokens1, _} ->
                    count_request(MatchedHost1),
                    {ok, ReqDataHost1} = webmachine_request:set_metadata(zotonic_host, MatchedHost1, ReqDataHost),
                    {{no_dispatch_match, MatchedHost1, NonMatchedPathTokens1}, ReqDataHost1};
                {no_host_match} ->
                    {{no_dispatch_match, undefined, undefined, []}, undefined};
                OtherDispatchMatch ->
                    trace_final(
                        DispReq#dispatch.tracer_pid,
                        set_dispatch_path(
                            handle_dispatch(OtherDispatchMatch, DispReq, ReqDataHost),
                            proplists:get_value(zotonic_dispatch_path, Bindings)))
            end
    end;
handle_rewrite({ok, #dispatch_match{
                            dispatch_name=SDispatchName,
                            mod=SMod,
                            mod_opts=SModOpts,
                            path_tokens=SPathTokens,
                            bindings=SBindings,
                            app_root=SAppRoot,
                            string_path=SStringPath}},
                DispReq, MatchedHost, _NonMatchedPathTokens, Bindings, ReqDataHost, _Context) ->
    trace(DispReq#dispatch.tracer_pid, 
          SPathTokens, 
          rewrite_match, 
          [ {dispatch,SDispatchName},
            {controller,SMod},
            {controller_args,SModOpts},
            {bindings,SBindings}
          ]),
    {{SMod, SModOpts, 
      [], none, % Host info
      SPathTokens, [{zotonic_dispatch, SDispatchName},{zotonic_host, MatchedHost}|SBindings] ++ Bindings, 
      SAppRoot, SStringPath}, 
     ReqDataHost};
handle_rewrite({ok, #dispatch_redirect{location=Location, is_permanent=IsPermanent}},
               DispReq, _MatchedHost, _NonMatchedPathTokens, _Bindings, ReqDataHost, Context) ->
    AbsURI = z_context:abs_url(Location, Context),
    trace(DispReq#dispatch.tracer_pid, undefined, rewrite_redirect, [{location,AbsURI},{permanent,IsPermanent}]),
    {handled, redirect(IsPermanent, AbsURI, ReqDataHost)};
handle_rewrite(undefined, DispReq, MatchedHost, NonMatchedPathTokens, _Bindings, ReqDataHost, _Context) ->
    trace(DispReq#dispatch.tracer_pid, undefined, rewrite_nomatch, []),
    {{no_dispatch_match, MatchedHost, NonMatchedPathTokens}, ReqDataHost}.


set_dispatch_path(Match, undefined) ->
    Match;
set_dispatch_path({{Mod, ModOpts, X, Y, PathTokens, Bindings, AppRoot, StringPath}, Host}, DispatchPath) ->
    Bindings1 = [
        {zotonic_dispatch_path, DispatchPath},
        {zotonic_dispatch_path_rewrite, proplists:get_value(zotonic_dispatch_path, Bindings)}
        | proplists:delete(zotonic_dispatch_path, Bindings)
    ],
    {{Mod, ModOpts, X, Y, PathTokens, Bindings1, AppRoot, StringPath}, Host};
set_dispatch_path(Match, _DispatchPath) ->
    Match.

%% @doc Try to find a site which says it can handle the host.
%%      This enables to have special (short) urls for deep pages.
handle_no_host_match(DispReq, ReqData) ->
    Sites = z_sites_manager:get_sites(),
    DispHost = #dispatch_host{
                    host=DispReq#dispatch.host,
                    path=DispReq#dispatch.path,
                    method=DispReq#dispatch.method,
                    protocol=DispReq#dispatch.protocol
                },
    case first_site_match(Sites, DispHost, ReqData) of
        no_host_match -> handle_dispatch_fallback(DispReq, ReqData);
        Redirect-> handle_dispatch(Redirect, DispReq, ReqData)
    end.


%% @doc Try to find the fallback site (usually zotonic_status).
handle_dispatch_fallback(DispReq, ReqData) ->
    case get_fallback_site() of
        undefined ->
            {{no_dispatch_match, undefined, undefined}, ReqData};
        Site ->
            handle_dispatch(gen_server:call(?MODULE, {site_dispatch, Site, DispReq}), DispReq, ReqData)
    end.


first_site_match([], _DispHost, _ReqData) ->
    no_host_match;
first_site_match([Site|Sites], DispHost, ReqData) ->
    case catch z_notifier:first(DispHost, z_context:new(Site)) of
        {ok, #dispatch_redirect{location=PathOrURI, is_permanent=IsPermanent}} ->
            {redirect, Site, z_convert:to_list(PathOrURI), IsPermanent};
        undefined ->
            first_site_match(Sites, DispHost, ReqData);
        Unexpected ->
            lager:error("dispatch_host for ~p returned ~p on ~p", [Site, Unexpected, DispHost]),
            first_site_match(Sites, DispHost, ReqData)
    end.



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
    case wrq:port(ReqData) of
        debug -> 
            handled;
        {ssl, debug} ->
            handled;
        _Port ->
            RD1 = wrq:set_resp_header("Location", Location, ReqData),
            RespCode = case IsPermanent of true -> 301; false -> 302 end,
            {ok, RD2} = webmachine_request:send_response(RD1#wm_reqdata{response_code = RespCode}),
            LogData = webmachine_request:log_data(RD2),
            {ok, LogModule} = application:get_env(webzmachine, webmachine_logger_module),
            spawn(LogModule, log_access, [LogData]),
            handled
    end.


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
%% @spec get_host_dispatch_list(WMHost, DispatchList, Method) -> {ok, Host::atom(), DispatchList::list()} | {redirect, Hostname::string()} | no_host_match
get_host_dispatch_list(WMHost, [#wm_host_dispatch_list{}|_] = DispatchList, Method) ->
    {Host, _Port} = split_host(WMHost),
    case get_dispatch_host(Host, DispatchList) of
        {ok, DL} ->
            {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list};

        undefined ->
            case get_dispatch_alias(Host, DispatchList) of
                {ok, DL} ->
                    case DL#wm_host_dispatch_list.redirect 
                        andalso is_hostname(DL#wm_host_dispatch_list.hostname) 
                        andalso Method =:= 'GET' 
                    of
                        true ->
                            {redirect, DL#wm_host_dispatch_list.host};
                        false ->
                            {ok, DL#wm_host_dispatch_list.host, DL#wm_host_dispatch_list.dispatch_list}
                    end;
                undefined ->
                    no_host_match
            end
    end;
get_host_dispatch_list(_WMHost, _DispatchList, _Method) ->
    no_host_match.


split_host(Host) ->
    case Host of
        undefined -> {"", "80"};
        none -> {"", "80"};
        [] -> {"", "80"};
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


trace(undefined, _PathTokens, _What, _Args) ->
    ok;
trace(TracerPid, PathTokens, What, Args) ->
    TracerPid ! {trace, PathTokens, What, Args}.

trace_final(undefined, Match) ->
    Match;
trace_final(TracerPid, {{Mod, ModOpts, _X, _Y, _PathTokens, Bindings, _AppRoot, _StringPath}, _Host} = Match) ->
    trace(TracerPid, 
          undefined,
          dispatch,
          [ {controller, Mod},
            {controller_args, ModOpts},
            {bindings, Bindings}
          ]),
    Match;
trace_final(_TracerPid, RedirectOrHandled) ->
    RedirectOrHandled.

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

%% @spec wm_dispatch(Protocol, HostAsString, Host::atom(), Path::string(), DispatchList::[matchterm()], TracerPid::pid()|undefined) ->
%%                                            dispterm() | dispfail()
%% @doc Interface for URL dispatching.
%% See also http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
wm_dispatch(Protocol, HostAsString, Host, PathAsString, DispatchList, TracerPid) ->
    Context = z_context:new(Host),
    Path = string:tokens(PathAsString, [?SEPARATOR]),
    IsDir = lists:last(PathAsString) == ?SEPARATOR,
    {Path1, Bindings} = z_notifier:foldl(#dispatch_rewrite{is_dir=IsDir, path=PathAsString, host=HostAsString}, {Path, []}, Context),
    case Path1 of
        Path -> ok;
        _ -> trace(TracerPid, Path, dispatch_rewrite, [{path,Path1},{bindings,Bindings}])
    end,
    Bindings1 = [
        {zotonic_dispatch_path, Path1}
        | Bindings
    ],
    trace(TracerPid, Path1, try_match, [{bindings,Bindings1}]),
    try_path_binding(Protocol, HostAsString, Host, DispatchList, Path1, Bindings1, extra_depth(Path1, IsDir), TracerPid, Context).

% URIs that end with a trailing slash are implicitly one token
% "deeper" than we otherwise might think as we are "inside"
% a directory named by the last token.
extra_depth([], _IsDir) -> 1;
extra_depth(_Path, true) -> 1;
extra_depth(_, _) -> 0.

try_path_binding(_Protocol, _HostAsString, _Host, [], PathTokens, Bindings, _ExtraDepth, TracerPid, _Context) ->
    trace(TracerPid, PathTokens, no_dispatch_match, []),
    {no_dispatch_match, PathTokens, Bindings};
try_path_binding(Protocol, HostAsString, Host, [{DispatchName, PathSchema, Mod, Props}|Rest], PathTokens, Bindings, ExtraDepth, TracerPid, Context) ->
    case bind(Host, PathSchema, PathTokens, Bindings, 0, Context) of
        {ok, Remainder, NewBindings, Depth} ->
            trace(TracerPid, 
                  PathTokens,
                  match,
                  [ {dispatch, DispatchName},
                    {controller, Mod},
                    {controller_args, Props},
                    {bindings, NewBindings}
                  ]),
            case proplists:get_value(protocol, Props) of
                % Force switch to normal http protocol
                undefined when Protocol =/= http ->
                    {Host1, HostPort} = split_host(z_context:hostname_port(Context)),
                    Host2 = add_port(http, Host1, HostPort),
                    trace(TracerPid, PathTokens, protocol_switch, [{protocol, http}, {host,Host2}]),
                    {redirect_protocol, "http", Host2};

                % Force switch to other (eg. https) protocol
                {NewProtocol, NewPort} when NewProtocol =/= Protocol ->
                    {Host1, _Port} = split_host(HostAsString),
                    Host2 = add_port(NewProtocol, Host1, NewPort),
                    trace(TracerPid, PathTokens, forced_protocol_switch, [{protocol, NewProtocol}, {host,Host2}]),
                    {redirect_protocol, z_convert:to_list(NewProtocol), Host2};

                % 'keep' or correct protocol
                _ ->
                    {DispatchName, Mod, Props, Remainder, NewBindings, 
                        calculate_app_root(Depth + ExtraDepth), 
                        reconstitute(Remainder)}
            end;
        fail -> 
            try_path_binding(Protocol, HostAsString, Host, Rest, PathTokens, Bindings, ExtraDepth, TracerPid, Context)
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

count_request(Host) ->
    exometer:update([zotonic, Host, webzmachine, requests], 1).

