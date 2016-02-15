%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2016 Marc Worrell
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2009-2016 Marc Worrell
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
    dispatch/4,
    get_fallback_site/0,
    get_host_for_domain/1,
    update_dispatchinfo/0
]).

%% Exports for dispatch bindings
-export([
    is_bind_language/2
]).


-export([
    collect_dispatchrules/0,
    collect_dispatchrules/1,
    fetch_dispatchinfo/1
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
%% @spec dispatch(Host::string(), Path::string(), ReqData::wm_reqdata) -> {dispatch(), NewReqData}
%% @type dispatch() = {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens}
%%                  | {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath}
%%                  | handled
dispatch(Host, Path, ReqData) ->
    dispatch(Host, Path, ReqData, undefined).

dispatch(Host, Path, ReqData, TracerPid) when (is_list(Host) orelse is_binary(Host)), is_list(Path) ->
    % Classify the user agent
    z_depcache:in_process(true),
    {ok, ReqDataUA} = z_user_agent:set_class(ReqData),
    Protocol = case wrq:is_ssl(ReqDataUA) of true -> https; false -> http end,
    DispReq = #dispatch{
                    host=Host,
                    path=Path,
                    method=wrq:method(ReqDataUA),
                    protocol=Protocol,
                    tracer_pid=TracerPid
              },
    {Hostname, _Port} = split_host(z_convert:to_list(Host)),
    case ets:lookup(?MODULE, Hostname) of
        [] ->
            % Check for fallback sites or other site handling this hostname
            case find_no_host_match(DispReq, ReqDataUA) of
                {ok, Site} ->
                    dispatch_site(Site, DispReq, ReqDataUA);
                Other ->
                    Other
            end;
        [{_,Site}] ->
            dispatch_site(Site, DispReq, ReqDataUA);
        [{_,Site,_Redirect}] ->
            handle_dispatch_result({redirect, Site}, DispReq, ReqDataUA)
    end.

dispatch_site(Site, #dispatch{tracer_pid=TracerPid, path=Path, host=Hostname} = DispReq, ReqDataUA) ->
    {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, Site, ReqDataUA),
    count_request(Site),
    try
        Context = z_context:set_reqdata(ReqDataHost, z_context:new(Site)),
        {Tokens, IsDir} = split_path(Path),
        {TokensRewritten, Bindings} = dispatch_rewrite(Hostname, Path, Tokens, IsDir, TracerPid, Context),
        Bindings1 = [
            {zotonic_dispatch_path, TokensRewritten},
            {zotonic_host, Site}
            | Bindings
        ],
        trace(TracerPid, TokensRewritten, try_match, [{bindings, Bindings1}]),
        case dispatch_match(TokensRewritten, Context) of
            {ok, {DispatchRule, MatchBindings}} ->
                trace_final(
                        TracerPid,
                        do_dispatch_rule(DispatchRule, Bindings1++fix_match_bindings(MatchBindings, IsDir), TokensRewritten, IsDir, DispReq, ReqDataHost, Context));
            fail ->
                trace_final(
                        TracerPid,
                        do_dispatch_fail(Bindings1, TokensRewritten, IsDir, DispReq, ReqDataHost, Context))
        end
    catch
        throw:{stop_request, RespCode} ->
            {{stop_request, RespCode}, ReqDataUA}
    end.

dispatch_match(Tokens, Context) ->
    Module = z_utils:name_for_host(dispatch, z_context:site(Context)),
    try
        dispatch_compiler:match(Module, Tokens, Context)
    catch
        error:undef ->
            fail
    end.


%% @doc Fix bindings: values should be lists and the '*' should be a string
fix_match_bindings(Ms, IsDir) ->
    [ fix_match_binding(M, IsDir) || M <- Ms ].

fix_match_binding({Name, Value}, _IsDir) when is_binary(Value) ->
    {Name, z_convert:to_list(Value)};
fix_match_binding({'*', List}, IsDir) when is_list(List) ->
    List1 = [ mochiweb_util:quote_plus(B) || B <- List ],
    Path = string:join(List1, "/"),
    case IsDir of
        true -> {'*', Path ++ "/"};
        false -> {'*', Path}
    end;
fix_match_binding(NV, _IsDir) ->
    NV.

split_path(Path) ->
    PathBin = case z_convert:to_binary(Path) of
                 <<"/", P/binary>> -> P;
                 P -> P
              end,
    Parts = binary:split(PathBin, <<"/">>, [global]),
    Parts1 = remove_dotdot(Parts),
    Parts2 = [ unescape(Part) || Part <- Parts1 ],
    case lists:last(Parts2) of
        <<>> -> {lists:sublist(Parts2, length(Parts2)-1), true};
        _ -> {Parts2, false}
    end.

remove_dotdot(Parts) ->
    case remove_dotdot(Parts, []) of
        none -> Parts;
        Parts1 -> remove_dotdot(Parts1)
    end.

remove_dotdot([], _Acc) ->
    none;
remove_dotdot([_,<<"..">>|Rest], Acc) ->
    lists:reverse(Acc, Rest);
remove_dotdot([A|Rest], Acc) ->
    remove_dotdot(Rest, [A|Acc]).

unescape(P) ->
    case binary:match(P, <<"%">>) of
        nomatch -> P;
        _ -> z_convert:to_binary(mochiweb_util:unquote(P))
    end.

dispatch_rewrite(Hostname, Path, Tokens, IsDir, TracerPid, Context) ->
    {Tokens1, Bindings} = z_notifier:foldl(
                            #dispatch_rewrite{is_dir=IsDir, path=Path, host=Hostname},
                            {Tokens, []},
                            Context),
    case Tokens1 of
        Tokens -> ok;
        _ -> trace(TracerPid, Path, dispatch_rewrite, [{path,Tokens1},{bindings,Bindings}])
    end,
    {Tokens1, Bindings}.

%% @doc Retrieve the fallback site.
get_fallback_site() ->
    case ets:lookup(?MODULE, "*") of
        [] -> undefined;
        [{_,Site}] -> Site;
        [{_,Site, _}] -> Site
    end.

%% @doc Fetch the host handling the given domain name
-spec get_host_for_domain(string()|binary()) -> {ok, atom()} | undefined.
get_host_for_domain(Domain) when is_binary(Domain) ->
    get_host_for_domain(binary_to_list(Domain));
get_host_for_domain(Domain) ->
    {Hostname, _Port} = split_host(Domain),
    case ets:lookup(?MODULE, Hostname) of
        [{_,Site}] -> {ok, Site};
        [{_,Site,_Redirect}] -> {ok, Site};
        [] -> undefined
    end.


%% @doc Callback for the dispatch compiler, try to bind a language
is_bind_language(Match, _Context) ->
    z_trans:is_language(Match).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    gen_server:cast(self(), update_hosts),
    gen_server:cast(self(), update_dispatchinfo),
    ets:new(?MODULE, [named_table, set, {keypos, 1}, protected, {read_concurrency, true}]),
    {ok, #state{}}.


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
%% @doc Load all dispatch rules, if anything changed then recompile the dispatcher(s)
handle_cast(update_dispatchinfo, State) ->
    FallbackSite = z_sites_manager:get_fallback_site(),
    do_update_hosts(FallbackSite),
    NewRules = collect_dispatchrules(),
    do_compile_modified(State#state.rules, NewRules),
    {noreply, State#state{
                rules=NewRules,
                fallback_site=FallbackSite
            }};

%% @doc Fetch all active hostnames
handle_cast(update_hosts, State) ->
    FallbackSite = z_sites_manager:get_fallback_site(),
    do_update_hosts(FallbackSite),
    {noreply, State#state{fallback_site=FallbackSite}};

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

%% @doc Update the ets table with all host/site mappings
do_update_hosts(FallbackSite) ->
    Cs = z_sites_manager:get_site_contexts(),
    Hs = lists:flatten([ get_site_hosts(Context) || Context <- Cs ]),
    Hs1 = [ {strip_port(Host), Prio, Site} || {Host, Prio, Site} <- Hs ],
    Hs2 = lists:sort([ {"*", 99, FallbackSite} | Hs1 ]),
    Dict = lists:foldr(
                fun({Host, _, Site}, Acc) ->
                    dict:store(Host, Site, Acc)
                end,
                dict:new(),
                Hs2),
    Redirects = [
        {z_context:site(Context), is_site_redirect(Context), z_context:hostname(Context), z_context:hostname_port(Context)}
        || Context <- Cs
    ],
    % Fetch current list from ets
    HostSiteNew = lists:sort(add_redirects(dict:to_list(Dict), Redirects)),
    HostSiteOld = lists:sort(ets:tab2list(?MODULE)),
    % Insert/delete the diff
    lists:foreach(fun(HostSite) ->
                     ets:delete_object(?MODULE, HostSite)
                  end,
                  HostSiteOld -- HostSiteNew),
    ets:insert(?MODULE, HostSiteNew -- HostSiteOld),
    ok.

is_site_redirect(Context) ->
    case m_site:get(redirect, Context) of
        true -> true;
        false -> false;
        undefined -> true
    end.

add_redirects(HostSites, Redirects) ->
    [ add_redirect({Host, Site}, lists:keyfind(Site, 1, Redirects)) || {Host, Site} <- HostSites ].

add_redirect(HS, {_, false, _, _}) ->
    HS;
add_redirect({Host, Site}, {Site, true, Host, _Redirect}) ->
    {Host, Site};
add_redirect({Host, Site}, {Site, true, _Host, none}) ->
    {Host, Site};
add_redirect({Host, Site}, {Site, true, _Host, Redirect}) ->
    {Host, Site, Redirect}.


strip_port(H) ->
    {H1, _} = split_host(H),
    H1.

get_site_hosts(#context{} = Context) ->
    Site = z_context:site(Context),
    Hostname = z_context:hostname(Context),
    HostAlias = case m_site:get(hostalias, Context) of
                    undefined -> [];
                    List when is_list(List) -> ensure_alias_list(List);
                    _ -> []
                end,
    HostSmtp = m_site:get(smtphost, Context),
    Hs = [{Hostname, 1, Site}]
        ++ [ {Alias, 2, Site} || Alias <- HostAlias ]
        ++ [ {HostSmtp, 2, Site} ],
    lists:filter(
                fun
                    ({undefined, _, _}) -> false;
                    ({[], _, _}) -> false;
                    ({<<>>, _, _}) -> false;
                    (_) -> true
                end,
                Hs).

ensure_alias_list([C|_] = Alias) when is_integer(C) -> [Alias];
ensure_alias_list(Alias) -> Alias.

do_compile_modified(OldDs, NewDs) ->
    Ds = NewDs -- OldDs,
    lists:foreach(fun do_compile/1, Ds).

do_compile(#wm_host_dispatch_list{host=Host, dispatch_list=DL}) ->
    dispatch_compiler:compile_load(z_utils:name_for_host(dispatch, Host), map_z_language(DL)).

map_z_language(DL) ->
    [ map_z_language_1(Disp) || Disp <- DL ].

map_z_language_1({Name, Path, Controller, Opts}) ->
    Path1 = [ map_z_language_2(P) || P <- Path ],
    {Name, Path1, Controller, Opts}.

map_z_language_2(z_language) -> {z_language, {?MODULE, is_bind_language}};
map_z_language_2(X) -> X.


do_dispatch_rule({DispatchName, _, Mod, Props}, Bindings, Tokens, _IsDir, DispReq, ReqData, Context) ->
    #dispatch{tracer_pid=TracerPid, host=Hostname, protocol=Protocol, path=Path} = DispReq,
    Bindings1 = [ {zotonic_dispatch, DispatchName} | Bindings ],
    trace(TracerPid,
          Tokens,
          match,
          [ {dispatch, DispatchName},
            {controller, Mod},
            {controller_args, Props},
            {bindings, Bindings1}
          ]),
    DispatchResult = case proplists:get_value(protocol, Props) of
        % Force switch to normal http protocol
        undefined when Protocol =/= http ->
            {Host1, HostPort} = split_host(z_context:hostname_port(Context)),
            Host2 = add_port(http, Host1, HostPort),
            trace(TracerPid, Tokens, protocol_switch, [{protocol, http}, {host, Host2}]),
            {redirect_protocol, "http", Host2};

        % Force switch to other (eg. https) protocol
        {NewProtocol, NewPort} when NewProtocol =/= Protocol ->
            {Host1, _Port} = split_host(Hostname),
            Host2 = add_port(NewProtocol, Host1, NewPort),
            trace(TracerPid, Tokens, forced_protocol_switch, [{protocol, NewProtocol}, {host,Host2}]),
            IsPermanent = case NewProtocol of
                https -> z_convert:to_bool(m_config:get(mod_ssl, is_permanent, Context));
                _ -> false
            end,
            {redirect_protocol, z_convert:to_list(NewProtocol), Host2, IsPermanent};

        % 'keep' or correct protocol
        _ ->
            % {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath}
            {{Mod, Props,
              [], none,
              Tokens, Bindings1,
              ".", proplists:get_value('*', Bindings1, Path)}, z_context:site(Context)}
    end,
    handle_dispatch_result(DispatchResult, DispReq, ReqData).

do_dispatch_fail(Bindings, Tokens, _IsDir, DispReq, ReqData, Context0) ->
    TokenPath = tokens_to_path(Tokens),
    trace(DispReq#dispatch.tracer_pid, DispReq#dispatch.path, notify_dispatch, []),
    Context = maybe_set_language(Bindings, Context0),
    Redirect = z_notifier:first(DispReq#dispatch{path=TokenPath}, Context#context{wm_reqdata=ReqData}),
    handle_rewrite(Redirect, DispReq, DispReq#dispatch.host, Tokens, Bindings, ReqData, Context).

%% @TODO change this to binaries
tokens_to_path([]) ->
    "/";
tokens_to_path(Ts) ->
    lists:flatten([ [ $/, z_convert:to_list(T) ] || T <- Ts ]).


handle_dispatch_result({Match, MatchedHost}, _DispReq, ReqData) when is_tuple(Match) ->
    % Known host, known dispatch rule
    {ok, ReqDataHost} = webmachine_request:set_metadata(zotonic_host, MatchedHost, ReqData),
    {Match, ReqDataHost};
handle_dispatch_result({redirect, MatchedHost}, DispReq, ReqDataUA) when is_atom(MatchedHost) ->
    % Redirect to other host, same path
    RawPath = wrq:raw_path(ReqDataUA),
    Uri = z_context:abs_url(RawPath, z_context:new(MatchedHost)),
    trace(DispReq#dispatch.tracer_pid, undefined, redirect, [{location, Uri},{permanent,true}]),
    {handled, redirect(true, z_convert:to_list(Uri), ReqDataUA)};
handle_dispatch_result({redirect, MatchedHost, NewPathOrURI, IsPermanent}, DispReq, ReqDataUA) when is_atom(MatchedHost) ->
    % Redirect to some site, new path or uri
    AbsURI = z_context:abs_url(NewPathOrURI, z_context:new(MatchedHost)),
    trace(DispReq#dispatch.tracer_pid, undefined, redirect, [{location, AbsURI},{permanent,IsPermanent}]),
    {handled, redirect(IsPermanent, z_convert:to_list(AbsURI), ReqDataUA)};
handle_dispatch_result({redirect_protocol, NewProtocol, NewHost}, DispReq, ReqDataUA) ->
    handle_dispatch_result({redirect_protocol, NewProtocol, NewHost, false}, DispReq, ReqDataUA);
handle_dispatch_result({redirect_protocol, NewProtocol, NewHost, IsPermanent}, _DispReq, ReqDataUA) ->
    % Switch protocols (mostly http/https switch)
    {handled, redirect(IsPermanent, z_convert:to_list(NewProtocol), NewHost, ReqDataUA)}.


%% Handle possible request rewrite; used when no dispatch rule matched
handle_rewrite({ok, Id}, DispReq, MatchedHost, NonMatchedPathTokens, _Bindings, ReqDataHost, Context) when is_integer(Id) ->
    %% Retry with the resource's default page uri
    case m_rsc:p_no_acl(Id, default_page_url, Context) of
        undefined ->
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id}]),
            {{no_dispatch_match, MatchedHost, NonMatchedPathTokens}, ReqDataHost};
        DefaultPagePathBin ->
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id},{path,DefaultPagePathBin}]),
            {Tokens, IsDir} = split_path(DefaultPagePathBin),
            {TokensRewritten, BindingsRewritten} = dispatch_rewrite(DispReq#dispatch.host, DefaultPagePathBin, Tokens, IsDir, DispReq#dispatch.tracer_pid, Context),
            BindingsRewritten1 = [
                {zotonic_dispatch_path, TokensRewritten},
                {zotonic_host, z_context:site(Context)}
                | BindingsRewritten
            ],
            trace(DispReq#dispatch.tracer_pid, TokensRewritten, try_match, [{bindings, BindingsRewritten1}]),
            case dispatch_match(TokensRewritten, Context) of
                {ok, {DispatchRule, MatchBindings}} ->
                    set_dispatch_path(
                        do_dispatch_rule(
                                    DispatchRule,
                                    BindingsRewritten1++fix_match_bindings(MatchBindings, IsDir),
                                    TokensRewritten, IsDir, DispReq, ReqDataHost,
                                    Context),
                        NonMatchedPathTokens);
                fail ->
                    trace(DispReq#dispatch.tracer_pid, undefined, rewrite_nomatch, []),
                    {{no_dispatch_match, MatchedHost, NonMatchedPathTokens}, ReqDataHost}
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


maybe_set_language(Bindings, Context) ->
    case language_from_bindings(Bindings) of
        {ok, Lang} -> z_context:set_language(Lang, Context);
        {error, _} -> Context
    end.

language_from_bindings(Bindings) ->
    language_from_bindings_1(lists:keyfind(z_language, 1, Bindings)).

language_from_bindings_1({z_language, Language}) ->
    z_trans:to_language_atom(Language);
language_from_bindings_1(false) ->
    {error, not_a_language}.

%% @doc Try to find a site which says it can handle the host.
%%      This enables to have special (short) urls for deep pages.
find_no_host_match(DispReq, ReqData) ->
    Sites = z_sites_manager:get_sites(),
    DispHost = #dispatch_host{
                    host=DispReq#dispatch.host,
                    path=DispReq#dispatch.path,
                    method=DispReq#dispatch.method,
                    protocol=DispReq#dispatch.protocol
                },
    case first_site_match(Sites, DispHost, ReqData) of
        no_host_match -> find_dispatch_fallback();
        Redirect-> Redirect
    end.


%% @doc Try to find the fallback site (usually zotonic_status).
find_dispatch_fallback() ->
    case get_fallback_site() of
        undefined -> {no_dispatch_match, undefined, undefined};
        Site -> {ok, Site}
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
    [ filter_rules(fetch_dispatchinfo(Site), Site) || Site <- z_sites_manager:get_sites() ].

%% @doc Collect all dispatch rules for all sites, normalize and filter them.
collect_dispatchrules(Site) ->
    filter_rules(fetch_dispatchinfo(Site), Site).

%% @doc Fetch dispatch rules for a specific site.
fetch_dispatchinfo(Site) ->
    Name = z_utils:name_for_host(z_dispatcher, Site),
    {Host, Hostname, SmtpHost, Hostalias, Redirect, DispatchList} = z_dispatcher:dispatchinfo(Name),
    #wm_host_dispatch_list{
        host=Host, hostname=Hostname, smtphost=SmtpHost, hostalias=Hostalias,
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

split_host(undefined) -> {"", "80"};
split_host(none) -> {"", "80"};
split_host("") -> {"", "80"};
split_host(Host) ->
    % Split the optional port number from the host name
    [H|Rest] = string:tokens(string:to_lower(Host), ":"),
    case Rest of
        [] -> {H, "80"};
        [Port|_] -> {H, Port}
    end.

% %% @doc Check if the hostname is a hostname suitable to redirect to
% is_hostname(undefined) -> false;
% is_hostname("") -> false;
% is_hostname("localhost") -> false;
% is_hostname("127.0.0.1") -> false;
% is_hostname(_) -> true.

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


add_port(http, Host, 80) -> Host;
add_port(https, Host, 443) -> Host;
add_port(_, Host, Port) -> Host++[$:|z_convert:to_list(Port)].


% calculate_app_root(1) -> ".";
% calculate_app_root(N) when N > 1 ->
%     string:join(lists:duplicate(N, ".."), [?SEPARATOR]).

count_request(Host) ->
    exometer:update([zotonic, Host, webzmachine, requests], 1).
