%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2009-2017 Marc Worrell
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
-behaviour(cowboy_middleware).


%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% cowboy middleware
-export([execute/2]).

%% interface functions
-export([
    dispatch/1,
    dispatch/5,
    get_fallback_site/0,
    get_site_for_hostname/1,
    update_hosts/0,
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

-type site_dispatch_list() :: #site_dispatch_list{}.
-type dispatch_rule() :: {atom(), list(binary()), list()}.
-type hostname() :: binary() | string().

-record(state, {
    rules = [] :: list(dispatch_rule()),
    fallback_site = zotonic_site_status :: atom()
}).

-record(dispatch_nomatch, {
    site = undefined :: atom(),
    host = <<>> :: binary(),
    path_tokens = [] :: [binary()],
    bindings = [] :: [{atom(), binary()}],
    context = undefined :: #context{} | undefined
}).

-record(dispatch_controller, {
    dispatch_rule = undefined :: atom(),
    controller = undefined :: atom(),
    controller_options = [] :: list(),
    path_tokens = [] :: [binary()],
    bindings :: [{atom(), binary()}],
    context :: #context{}
}).

-type dispatch() :: #dispatch_controller{}
                    | #dispatch_nomatch{}
                    | {redirect, Site :: atom(), NewPathOrURI :: binary(), IsPermanent :: boolean()}
                    | {redirect_protocol, http|https, Site :: atom(), IsPermanent :: boolean()}
                    | {stop_request, pos_integer()}.

-export_type([
    dispatch_rule/0,
    hostname/0
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Collect dispatch information from all sites and recompiles the dispatch rules.
-spec update_dispatchinfo() -> ok.
update_dispatchinfo() ->
    gen_server:cast(?MODULE, update_dispatchinfo).


%% @doc Update the host/site mappings
-spec update_hosts() -> ok.
update_hosts() ->
    gen_server:cast(?MODULE, update_hosts).


%% @doc Cowboy middleware, route the new request. Continue with the cowmachine,
%%      requests a redirect or return a 400 on an unknown host.
%%      The cowmachine_proxy middleware must have been called before this.
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req}
    when Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
execute(Req, Env) ->
    case dispatch(Req) of
        #dispatch_controller{} = Match ->
            Context = Match#dispatch_controller.context,
            {ok, Req#{
                bindings => Match#dispatch_controller.bindings
            }, Env#{
                site => z_context:site(Context),
                dispatch_rule => Match#dispatch_controller.dispatch_rule,
                controller => Match#dispatch_controller.controller,
                controller_options => Match#dispatch_controller.controller_options,
                path_tokens => Match#dispatch_controller.path_tokens,
                bindings => Match#dispatch_controller.bindings,
                context => Context
            }};
        #dispatch_nomatch{site = Site, bindings = Bindings, context = Context} ->
            handle_error(404, cowboy_req:method(Req), Site, Req, Env, Bindings, Context);
        {redirect, Site, undefined, IsPermanent} ->
            case z_sites_manager:wait_for_running(Site) of
                ok ->
                    Uri = z_context:abs_url(raw_path(Req), z_context:new(Site)),
                    redirect(Uri, IsPermanent, Req);
                {error, _} ->
                    {stop_request, 503}
            end;
        {redirect, Site, NewPathOrURI, IsPermanent} ->
            case z_sites_manager:wait_for_running(Site) of
                ok ->
                    Uri = z_context:abs_url(NewPathOrURI, z_context:new(Site)),
                    redirect(Uri, IsPermanent, Req);
                {error, _} ->
                    {stop_request, 503}
            end;
        {redirect_protocol, Protocol, Host, IsPermanent} ->
            Uri = iolist_to_binary([
                        z_convert:to_binary(Protocol),
                        <<"://">>,
                        Host,
                        raw_path(Req)]),
            redirect(Uri, IsPermanent, Req);
        {stop_request, RespCode} ->
            stop_request(RespCode, Req, Env)
    end.

%% @doc Match the host and path to a dispatch rule.
-spec dispatch(cowboy_req:req()) -> dispatch().
dispatch(Req) ->
    Host = cowmachine_req:host(Req),
    Scheme = cowmachine_req:scheme(Req),
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    DispReq = #dispatch{
                    host=Host,
                    path=Path,
                    method=Method,
                    protocol=Scheme,
                    tracer_pid=undefined
              },
    z_depcache:in_process(true),
    z_memo:enable(),
    dispatch_1(DispReq, Req).

dispatch(Method, Host, Path, IsSsl, TracerPid) when is_boolean(IsSsl) ->
    Protocol = case IsSsl of
                    true -> https;
                    false -> http
               end,
    DispReq = #dispatch{
                    host=z_convert:to_binary(Host),
                    path=z_convert:to_binary(Path),
                    method=Method,
                    protocol=Protocol,
                    tracer_pid=TracerPid
              },
    dispatch_1(DispReq, undefined).


%% @doc Retrieve the fallback site.
-spec get_fallback_site() -> {ok, atom()} | undefined.
get_fallback_site() ->
    get_site_for_hostname('*').

%% @doc Fetch the site handling the given hostname (with optional port) (debug function)
-spec get_site_for_hostname(string()|binary()|'*') -> {ok, atom()} | undefined.
get_site_for_hostname(Hostname) when is_list(Hostname) ->
    get_site_for_hostname(list_to_binary(Hostname));
get_site_for_hostname(Hostname) ->
    case ets:lookup(?MODULE, strip_port(Hostname)) of
        [{_, Site, _Redirect}] -> {ok, Site};
        [] -> undefined
    end.


%%====================================================================
%% Internal functions
%%====================================================================

set_server_header(Req) ->
    cowboy_req:set_resp_header(<<"server">>, cowmachine_response:server_header(), Req).

raw_path(Req) ->
    Path = cowboy_req:path(Req),
    case cowboy_req:qs(Req) of
        <<>> -> Path;
        Qs -> <<Path/binary, $?, Qs/binary>>
    end.

redirect(Uri, IsPermanent, Req) ->
    Req1 = cowboy_req:set_resp_header(<<"location">>, Uri, set_server_header(Req)),
    {stop, cowboy_req:reply(case IsPermanent of true -> 301; false -> 302 end, Req1)}.

%% ---------------------------------------------------------------------------------------

stop_request(RespCode, Req, Env) ->
    Method = cowboy_req:method(Req),
    case ets:lookup(?MODULE, '*') of
        [] ->
            handle_error(RespCode, Method, undefined, Req, Env, [], undefined);
        [{_, Site, _}] ->
            case z_sites_manager:wait_for_running(Site, 1) of
                ok ->
                    Context = z_context:set_reqdata(Req, z_context:new(Site)),
                    handle_error(RespCode, Method, Site, Req, Env, [], Context);
                {error, _} ->
                    handle_error(RespCode, Method, undefined, Req, Env, [], undefined)
            end
    end.


handle_error(RespCode, <<"GET">>, undefined, Req, _Env, _Bindings, _Context) ->
    send_static_response(RespCode, Req);
handle_error(RespCode, _Method, undefined, Req, _Env, _Bindings, _Context) ->
    {stop, cowboy_req:reply(RespCode, set_server_header(Req))};
handle_error(_RespCode, <<"CONNECT">>, _Site, Req, _Env, _Bindings, _Context) ->
    {stop, cowboy_req:reply(400, set_server_header(Req))};
handle_error(RespCode, Method, Site, Req, Env, Bindings, Context) when Method =:= <<"GET">>; Method =:= <<"POST">> ->
    {ok, Req#{
        bindings => Bindings
    }, Env#{
        site => Site,
        controller => controller_http_error,
        controller_options => [ {http_status_code, RespCode} ],
        path_tokens => [],
        bindings => Bindings,
        context => Context
    }};
handle_error(RespCode, _Method, _Site, Req, _Env, _Bindings, _Context) ->
    {stop, cowboy_req:reply(RespCode, set_server_header(Req))}.

send_static_response(RespCode, Req) ->
    ErrorFile = filename:join(z_config:get(html_error_path), integer_to_list(RespCode)++".html"),
    case filelib:is_regular(ErrorFile) of
        true ->
            {ok, Html} = file:read_file(ErrorFile),
            Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/html; charset=utf8">>, Req),
            Req2 = cowboy_req:set_resp_body(Html, Req1),
            {stop, cowboy_req:reply(RespCode, set_server_header(Req2))};
        false ->
            {stop, cowboy_req:reply(RespCode, set_server_header(Req))}
    end.

%% ---------------------------------------------------------------------------------------

%% Always redirect to https
dispatch_1(#dispatch{ protocol = http, host = Hostname } = DispReq, _OptReq) when Hostname =/= undefined ->
    #dispatch{ tracer_pid = TracerPid } = DispReq,
    redirect_protocol(https, Hostname, TracerPid, [], undefined);
dispatch_1(DispReq, OptReq) ->
    case ets:lookup(?MODULE, DispReq#dispatch.host) of
        [] ->
            % Check for fallback sites or other site handling this hostname
            case find_no_host_match(DispReq, OptReq) of
                {ok, Site} ->
                    dispatch_site_if_running(DispReq, OptReq, Site, []);
                {fallback, Site} ->
                    dispatch_site_if_running(DispReq, OptReq, Site, [{http_status_code, 400}]);
                Other ->
                    Other
            end;
        [{_, Site, undefined}] ->
            dispatch_site_if_running(DispReq, OptReq, Site, []);
        [{_, Site, _Redirect}] ->
            case DispReq#dispatch.path of
                <<"/.well-known/", _/binary>> ->
                    dispatch_site_if_running(DispReq, OptReq, Site, []);
                _ ->
                    {redirect, Site, undefined, true}
            end
    end.

dispatch_site_if_running(DispReq, OptReq, Site, ExtraBindings) ->
    case z_sites_manager:wait_for_running(Site) of
        ok ->
            Context = z_context:set_reqdata(OptReq, z_context:new(Site)),
            dispatch_site(DispReq, Context, ExtraBindings);
        {error, timeout} ->
            {stop_request, 503};
        {error, _} ->
            case ets:lookup(?MODULE, '*') of
                [] ->
                    {stop_request, 400};
                [{_Host, Site, _Redirect}] ->
                    {stop_request, 503};
                [{_Host, FallbackSite, undefined}] ->
                    ExtraBindings1 = [
                        {http_status_code, 400}
                        | proplists:delete(http_status_code, ExtraBindings)
                    ],
                    dispatch_site_if_running(DispReq, OptReq, FallbackSite, ExtraBindings1);
                [{_Host, FallbackSite, _Redirect}] ->
                    {redirect, FallbackSite, undefined, true}
            end
    end.

-spec dispatch_site(#dispatch{}, #context{}, list()) -> dispatch().
dispatch_site(#dispatch{tracer_pid = TracerPid, path = Path, host = Hostname} = DispReq, Context, ExtraBindings) ->
    count_request(z_context:site(Context)),
    try
        {Tokens, IsDir} = split_path(Path),
        {TokensRewritten, Bindings} = dispatch_rewrite(Hostname, Path, Tokens, IsDir, TracerPid, Context),
        Bindings1 = [
            {zotonic_dispatch_path, TokensRewritten},
            {zotonic_site, z_context:site(Context)}
            | Bindings
        ],
        Bindings2 = ExtraBindings ++ Bindings1,
        trace(TracerPid, TokensRewritten, try_match, [{bindings, Bindings2}]),
        case dispatch_match(TokensRewritten, Context) of
            {ok, {DispatchRule, MatchBindings}} ->
                Bindings3 = Bindings2++fix_match_bindings(MatchBindings, IsDir),
                trace_final(
                        TracerPid,
                        do_dispatch_rule(DispatchRule, Bindings3, TokensRewritten, IsDir, DispReq, Context));
            fail ->
                trace_final(
                        TracerPid,
                        do_dispatch_fail(Bindings2, TokensRewritten, IsDir, DispReq, Context))
        end
    catch
        throw:{stop_request, RespCode} ->
            {stop_request, RespCode}
    end.

dispatch_match(Tokens, Context) ->
    Module = z_utils:name_for_site(dispatch, z_context:site(Context)),
    try
        dispatch_compiler:match(Module, Tokens, Context)
    catch
        error:undef ->
            fail
    end.


%% @doc Fix bindings: values should be binaries and the '*' should be a binary
%% @todo Allow a match for list, so that we don't need to split again in the controller
fix_match_bindings(Ms, IsDir) ->
    [ fix_match_binding(M, IsDir) || M <- Ms ].

fix_match_binding({'*', List}, IsDir) when is_list(List) ->
    List1 = [ cow_qs:urlencode(B) || B <- List ],
    Path = z_utils:combine($/, List1),
    case IsDir of
        true -> {'*', iolist_to_binary([Path, $/])};
        false -> {'*', iolist_to_binary(Path)}
    end;
fix_match_binding(NV, _IsDir) ->
    NV.

split_path(<<"/", P/binary>>) ->
    split_path(P);
split_path(Path) when is_binary(Path) ->
    Parts = binary:split(Path, <<"/">>, [global]),
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
        _ -> cow_qs:urldecode(P)
    end.

-spec dispatch_rewrite(binary(), binary(), list(), boolean(), pid(), z:context()) -> tuple().
dispatch_rewrite(Hostname, Path, Tokens0, IsDir, TracerPid, Context) ->
    {Tokens, Bindings} = rewrite_zotonic_accept(Tokens0, [], Path, TracerPid),
    {Tokens1, Bindings1} = z_notifier:foldl(
                            #dispatch_rewrite{is_dir = IsDir, path = Path, host = Hostname},
                            {Tokens, Bindings},
                            Context),
    case Tokens1 of
        Tokens -> nop;
        _ -> trace(TracerPid, Path, dispatch_rewrite, [{path,Tokens1},{bindings,Bindings1}])
    end,
    rewrite_zotonic_accept(Tokens1, Bindings1, Path, TracerPid).

rewrite_zotonic_accept([ <<"http-accept">>, Mime | Tokens ], Bindings, Path, TracerPid) ->
    Bindings1 = [ {zotonic_http_accept, Mime} | Bindings ],
    trace(TracerPid, Path, dispatch_rewrite, [ {path, Tokens}, {bindings,Bindings} ]),
    {Tokens, Bindings1};
rewrite_zotonic_accept(Tokens, Bindings, _Path, _TracerPid) ->
    {Tokens, Bindings}.

%% @doc Callback for the dispatch compiler, try to bind a language
is_bind_language(Match, _Context) ->
    z_language:is_valid(Match).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
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
    do_update_hosts(),
    NewRules = collect_dispatchrules(),
    do_compile_modified(State#state.rules, NewRules),
    {noreply, State#state{
        rules=NewRules
    }};

%% @doc Fetch all active hostnames
handle_cast(update_hosts, State) ->
    do_update_hosts(),
    {noreply, State};

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
do_update_hosts() ->
    {ok, SitesHosts} = z_sites_manager:get_sites_hosts(),
    Hosts = maps:fold(
        fun
            (_Site, {stopped, _Hs, _IsRedirect}, Acc) ->
                Acc;
            (_Site, {removing, _Hs, _IsRedirect}, Acc) ->
                Acc;
            (_Site, {_State, [], _IsRedirect}, Acc) ->
                Acc;
            (Site, {_State, [{PrimaryHost, _} | _] = Hs, IsRedirect}, Acc) ->
                PrimaryHost1 = strip_port(PrimaryHost),
                Hs1 = lists:map(
                    fun
                        ({H, Prio}) when H =:= PrimaryHost ->
                            {Prio, strip_port(H), Site, undefined};
                        ({H, Prio}) when IsRedirect ->
                            {Prio, strip_port(H), Site, PrimaryHost1};
                        ({H, Prio}) ->
                            {Prio, strip_port(H), Site, undefined}
                    end,
                    Hs),
                Hs1 ++ Acc
        end,
        [],
        SitesHosts),
    % Keep the highest prio definition per hostname
    Dict = lists:foldr(
        fun({_Prio, Host, Site, Redirect}, Acc) ->
            H1 = map_generic(Host),
            dict:store(H1, {H1, Site, Redirect}, Acc)
        end,
        dict:new(),
        lists:sort(Hosts)),
    HostList = [ HSR || {_,HSR} <- dict:to_list(Dict) ],
    % Fetch current list from ets
    HostSiteNew = lists:sort(HostList),
    HostSiteOld = lists:sort(ets:tab2list(?MODULE)),
    % Insert/delete the changed host definitions
    lists:foreach(
        fun(HostSite) ->
            ets:delete_object(?MODULE, HostSite)
        end,
        HostSiteOld -- HostSiteNew),
    ets:insert(?MODULE, HostSiteNew -- HostSiteOld),
    ok.

map_generic("*") -> '*';
map_generic(<<"*">>) -> '*';
map_generic(H) -> H.

strip_port('*') -> '*';
strip_port(H) ->
    {H1, _} = split_host(H),
    H1.

do_compile_modified(OldDs, NewDs) ->
    Ds = NewDs -- OldDs,
    lists:foreach(fun do_compile/1, Ds).

do_compile(#site_dispatch_list{site=Site, dispatch_list=DL}) ->
    dispatch_compiler:compile_load(z_utils:name_for_site(dispatch, Site), map_z_language(DL)).

map_z_language(DL) ->
    [ map_z_language_1(Disp) || Disp <- DL ].

map_z_language_1({Name, Path, Controller, Opts}) ->
    Path1 = [ map_z_language_2(P) || P <- Path ],
    {Name, Path1, Controller, Opts}.

map_z_language_2(z_language) -> {z_language, {?MODULE, is_bind_language}};
map_z_language_2(X) -> X.


-spec do_dispatch_rule(tuple(), any(), any(), any(), any(), any()) -> #dispatch_controller{}.
do_dispatch_rule({DispatchName, _, Mod, Props}, Bindings, Tokens, _IsDir, DispReq, Context) ->
    Bindings1 = [ {zotonic_dispatch, DispatchName} | Bindings ],
    trace(DispReq#dispatch.tracer_pid,
          Tokens,
          match,
          [ {dispatch, DispatchName},
            {controller, Mod},
            {controller_options, Props},
            {bindings, Bindings1}
          ]),
    #dispatch_controller{
        dispatch_rule = DispatchName,
        controller = Mod,
        controller_options = Props,
        path_tokens = Tokens,
        bindings = Bindings1,
        context = maybe_set_language(Bindings1, Context)
    }.

-spec redirect_protocol(https|http, binary()|undefined, pid()|undefined, list(), #context{} | undefined) ->
            {redirect_protocol, http|https, binary()|undefined, boolean()}.
redirect_protocol(https, Hostname, TracerPid, Tokens, undefined) ->
    NewHostname = add_port(https, Hostname, z_config:get(ssl_port)),
    trace(TracerPid, Tokens, forced_protocol_switch, [{protocol, https}, {host, NewHostname}]),
    {redirect_protocol, https, NewHostname, true};
redirect_protocol(https, Hostname, TracerPid, Tokens, Context) ->
    NewHostname = add_port(https, Hostname, z_config:get(ssl_port)),
    trace(TracerPid, Tokens, forced_protocol_switch, [{protocol, https}, {host, NewHostname}]),
    IsPermanent = z_convert:to_bool(m_config:get(site, ssl_permanent, Context)),
    {redirect_protocol, https, NewHostname, IsPermanent};
redirect_protocol(http, Hostname, TracerPid, Tokens, _Context) ->
    NewHostname = add_port(http, Hostname, z_config:get(port)),
    trace(TracerPid, Tokens, forced_protocol_switch, [{protocol, http}, {host, NewHostname}]),
    {redirect_protocol, http, NewHostname, false}.

-spec do_dispatch_fail(any(), any(), any(), any(), any()) -> #dispatch_controller{} | #dispatch_nomatch{}.
do_dispatch_fail(Bindings, Tokens, _IsDir, DispReq, Context0) ->
    TokenPath = tokens_to_path(Tokens),
    trace(DispReq#dispatch.tracer_pid, DispReq#dispatch.path, notify_dispatch, []),
    Context = maybe_set_language(Bindings, Context0),
    Redirect = z_notifier:first(DispReq#dispatch{path=TokenPath}, Context),
    handle_rewrite(Redirect, DispReq, DispReq#dispatch.host, Tokens, Bindings, Context).

tokens_to_path([]) ->
    <<"/">>;
tokens_to_path(Ts) ->
    iolist_to_binary([ [ $/, T ] || T <- Ts ]).


%% Handle possible request rewrite; used when no dispatch rule matched
handle_rewrite({ok, Id}, DispReq, MatchedHost, NonMatchedPathTokens, Bindings, Context) when is_integer(Id) ->
    %% Retry with the resource's default page uri
    case m_rsc:p_no_acl(Id, default_page_url, Context) of
        undefined ->
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id}]),
            #dispatch_nomatch{
                site = z_context:site(Context),
                host = MatchedHost,
                path_tokens = NonMatchedPathTokens,
                bindings = Bindings,
                context = Context
            };
        DefaultPagePath ->
            trace(DispReq#dispatch.tracer_pid, undefined, rewrite_id, [{id,Id},{path,DefaultPagePath}]),
            {Tokens, IsDir} = split_path(DefaultPagePath),
            {TokensRewritten, BindingsRewritten} = dispatch_rewrite(DispReq#dispatch.host, DefaultPagePath, Tokens, IsDir, DispReq#dispatch.tracer_pid, Context),
            BindingsRewritten1 = [
                {zotonic_dispatch_path, TokensRewritten},
                {zotonic_site, z_context:site(Context)}
                | BindingsRewritten
            ],
            trace(DispReq#dispatch.tracer_pid, TokensRewritten, try_match, [{bindings, BindingsRewritten1}]),
            case dispatch_match(TokensRewritten, Context) of
                {ok, {DispatchRule, MatchBindings}} ->
                    set_dispatch_path(
                        do_dispatch_rule(
                                    DispatchRule,
                                    BindingsRewritten1++fix_match_bindings(MatchBindings, IsDir),
                                    TokensRewritten, IsDir, DispReq,
                                    Context),
                        NonMatchedPathTokens);
                fail ->
                    trace(DispReq#dispatch.tracer_pid, undefined, rewrite_nomatch, []),
                    #dispatch_nomatch{
                        site = z_context:site(Context),
                        host = MatchedHost,
                        path_tokens = NonMatchedPathTokens,
                        bindings = Bindings,
                        context = Context
                    }
            end
    end;
handle_rewrite({ok, #dispatch_match{
                            dispatch_name=SDispatchName,
                            mod=SMod,
                            mod_opts=SModOpts,
                            path_tokens=SPathTokens,
                            bindings=SBindings}},
                DispReq, MatchedSite, _NonMatchedPathTokens, Bindings, Context) ->
    trace(DispReq#dispatch.tracer_pid,
          SPathTokens,
          rewrite_match,
          [ {dispatch,SDispatchName},
            {controller,SMod},
            {controller_options,SModOpts},
            {bindings,SBindings}
          ]),
    Bindings1 = [{zotonic_dispatch, SDispatchName},{zotonic_site, MatchedSite}|SBindings] ++ Bindings,
    #dispatch_controller{
        dispatch_rule=SDispatchName,
        controller=SMod,
        controller_options=SModOpts,
        path_tokens=SPathTokens,
        bindings=Bindings1,
        context=maybe_set_language(Bindings1,Context)
    };
handle_rewrite({ok, #dispatch_redirect{location=Location, is_permanent=IsPermanent}},
               _DispReq, _MatchedHost, _NonMatchedPathTokens, _Bindings, Context) ->
    {redirect, z_context:site(Context), Location, IsPermanent};
handle_rewrite(undefined, DispReq, MatchedHost, NonMatchedPathTokens, Bindings, Context) ->
    trace(DispReq#dispatch.tracer_pid, undefined, rewrite_nomatch, []),
    #dispatch_nomatch{
        site = z_context:site(Context),
        host = MatchedHost,
        path_tokens = NonMatchedPathTokens,
        bindings = Bindings,
        context = Context
    }.


set_dispatch_path(Match, []) ->
    Match;
set_dispatch_path(#dispatch_controller{bindings = Bindings} = DispatchController, DispatchPath) ->
    Bindings1 = [
        {zotonic_dispatch_path, DispatchPath},
        {zotonic_dispatch_path_rewrite, proplists:get_value(zotonic_dispatch_path, Bindings)}
        | proplists:delete(zotonic_dispatch_path, Bindings)
    ],
    DispatchController#dispatch_controller{bindings = Bindings1}.


maybe_set_language(Bindings, Context) ->
    case language_from_bindings(Bindings) of
        {ok, Lang} -> z_context:set_language(Lang, Context);
        {error, _} -> Context
    end.

language_from_bindings(Bindings) ->
    language_from_bindings_1(lists:keyfind(z_language, 1, Bindings)).

language_from_bindings_1({z_language, Language}) ->
    z_language:to_language_atom(Language);
language_from_bindings_1(false) ->
    {error, not_a_language}.

%% @doc Try to find a site which says it can handle the host.
%%      This enables to have special (short) urls for deep pages.
find_no_host_match(DispReq, OptReq) ->
    Sites = z_sites_manager:get_sites(),
    DispHost = #dispatch_host{
                    host=DispReq#dispatch.host,
                    path=DispReq#dispatch.path,
                    method=DispReq#dispatch.method,
                    protocol=DispReq#dispatch.protocol
                },
    case first_site_match(Sites, DispHost, OptReq) of
        no_host_match ->
            % See if there is a site handling wildcard domains
            case ets:lookup(?MODULE, '*') of
                [] ->
                    {stop_request, 400};
                [{_, FallbackSite, undefined}] ->
                    {fallback, FallbackSite};
                [{_, FallbackSite, _Redirect}] ->
                    {redirect, FallbackSite, undefined, true}
            end;
        Redirect->
            Redirect
    end.

first_site_match(Sites, DispHost, OptReq) ->
    maps:fold(
        fun
            (Site, running, no_host_match) ->
                case catch z_notifier:first(DispHost, z_context:set_reqdata(OptReq, z_context:new(Site))) of
                    {ok, #dispatch_redirect{location=PathOrURI, is_permanent=IsPermanent}} ->
                        {redirect, Site, PathOrURI, IsPermanent};
                    undefined ->
                        no_host_match;
                    Unexpected ->
                        lager:error("dispatch_host for ~p returned ~p on ~p", [Site, Unexpected, DispHost]),
                        no_host_match
                end;
            (_Site, _Status, Found) ->
                Found
        end,
        no_host_match,
        Sites).

%% @doc Collect all dispatch rules for all running sites, normalize and filter them.
collect_dispatchrules() ->
    maps:fold(
        fun
            (Site, running, Acc) ->
                [ collect_dispatchrules(Site) | Acc ];
            (_Site, _Status, Acc) ->
                Acc
        end,
        [],
        z_sites_manager:get_sites()).

%% @doc Collect all dispatch rules for all sites, normalize and filter them.
collect_dispatchrules(Site) ->
    case fetch_dispatchinfo(Site) of
        {ok, DispatchInfo} ->
            filter_rules(DispatchInfo, Site);
        {error, _} ->
            #site_dispatch_list{
                site=Site,
                hostname= <<"localhost">>,
                smtphost= <<"localhost">>,
                hostalias=[],
                redirect=false,
                dispatch_list=[]
            }
    end.

%% @doc Fetch dispatch rules for a specific site.
fetch_dispatchinfo(Site) ->
    Name = z_utils:name_for_site(z_dispatcher, Site),
    case z_dispatcher:dispatchinfo(Name) of
        {ok, {Site, Hostname, SmtpHost, Hostalias, Redirect, DispatchList}} ->
            {ok, #site_dispatch_list{
                site=Site, hostname=Hostname, smtphost=SmtpHost, hostalias=Hostalias,
                redirect=Redirect, dispatch_list=DispatchList
            }};
        {error, _} = Error ->
            Error
    end.


% @doc Split the optional port number from the host name
split_host(undefined) -> {<<>>, 80};
split_host(none) -> {<<>>, 80};
split_host("") -> {<<>>, 80};
split_host(<<>>) -> {<<>>, 80};
split_host(Host) when is_list(Host) ->
    split_host(unicode:characters_to_binary(Host));
split_host(Host) when is_binary(Host) ->
    case binary:split(z_string:to_lower(Host), <<":">>) of
        [H,P] -> {H, z_convert:to_integer(P)};
        [H] -> {H, 80}
    end.

% %% @doc Check if the hostname is a hostname suitable to redirect to
% is_hostname(undefined) -> false;
% is_hostname("") -> false;
% is_hostname("localhost") -> false;
% is_hostname("127.0.0.1") -> false;
% is_hostname(_) -> true.

%% @doc Filter all rules, also used to set/reset protocol (https) options.
filter_rules(Rules, Site) ->
    z_notifier:foldl(#dispatch_rules{rules=Rules}, Rules, z_context:new(Site)).

trace(undefined, _PathTokens, _What, _Args) ->
    ok;
trace(TracerPid, PathTokens, What, Args) ->
    TracerPid ! {trace, PathTokens, What, Args}.

trace_final(undefined, R) ->
    R;
trace_final(TracerPid, #dispatch_controller{
    controller = Controller,
    controller_options = ControllerOptions,
    bindings = Bindings
} = Match) ->
    trace(
        TracerPid,
        undefined,
        dispatch,
        [
            {controller, Controller},
            {controller_options, ControllerOptions},
            {bindings, Bindings}
          ]),
    Match;
trace_final(_TracerPid, RedirectOrHandled) ->
    RedirectOrHandled.


add_port(http, Hostname, 80) ->
    Hostname;
add_port(https, Hostname, 443) ->
    Hostname;
add_port(_, Hostname, Port) ->
    PortBin = z_convert:to_binary(Port),
    <<Hostname/binary, $:, PortBin/binary>>.

count_request(Site) ->
    exometer:update([zotonic, Site, webzmachine, requests], 1).
