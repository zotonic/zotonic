%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Manage dispatch lists (aka definitions for url patterns). Constructs named urls from dispatch lists.
%% @end

%% Copyright 2009-2025 Marc Worrell
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

-module(z_dispatcher).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% z_dispatch exports
-export([
    dispatcher_args/0,
    url_for/2,
    url_for/3,
    url_for/4,
    hostname/1,
    hostname_port/1,
    hostname_ssl_port/1,
    abs_url/2,
    dispatchinfo/1,
    update/1,
    reload/1,
    reload/2,
    drop_port/1
]).

-include_lib("zotonic.hrl").

-record(state, {
    dispatch_list :: [ z_sites_dispatcher:dispatch_rule() ],
    page_paths :: #{ binary() | atom() => z_sites_dispatcher:dispatch_rsc_rule() },
    lookup :: #{  },
    context :: z:context(),
    site :: atom(),
    hostname :: binary() | undefined,
    hostname_port :: binary() | undefined,
    hostname_ssl_port :: binary() | undefined,
    smtphost :: binary() | undefined,
    hostalias :: list( binary() ),
    redirect = true
}).

-record(dispatch_url, {url, dispatch_options}).

%%====================================================================
%% API
%%====================================================================

%% @doc A list of dispatch rule arguments that shouldn't be considered with redirects.
%%      Used by controller_file_id and controller_redirect
%% TODO: this behaviour should be changed to an _inclusive_ list instead of a filter list
dispatcher_args() ->
    [
        is_permanent, dispatch, q, qargs,
        zotonic_dispatch, ssl, protocol, session_id, set_session_id,
        zotonic_dispatch_file, zotonic_dispatch_module,
        auth_options, auth_expires, csp_nonce
    ].

%% @doc Starts the dispatch server
start_link(Site) ->
    Name = name(Site),
    gen_server:start_link({local, Name}, ?MODULE, Site, []).

%% @doc Construct an uri from a named dispatch, assuming no parameters. Uses html escape.
-spec url_for(Name, Context) -> Url when
    Name :: atom() | binary(),
    Context :: z:context(),
    Url :: binary() | undefined.
url_for(Name, Context) ->
    return_url(
        opt_abs_url(
            rewrite(make_url_for(Name, [], html, Context),
                    Name, [], Context),
            Name, [], Context)).


%% @doc Construct an uri from a named dispatch and the parameters. Uses html escape.
-spec url_for(Name, Args, Context) -> Url when
    Name :: atom() | binary(),
    Args :: proplists:proplist(),
    Context :: z:context(),
    Url :: binary() | undefined.
url_for(Name, Args, Context) ->
    Args1 = append_extra_args(Args, Context),
    return_url(
        opt_abs_url(
            rewrite(make_url_for(Name, Args1, html, Context),
                    Name, Args1, Context),
            Name, Args1, Context)).


%% @doc Construct an uri from a named dispatch and the parameters
-spec url_for(Name, Args, Escape, Context) -> Url when
    Name :: atom() | binary(),
    Args :: proplists:proplist(),
    Escape :: html | xml | none,
    Context :: z:context(),
    Url :: binary() | undefined.
url_for(Name, Args, Escape, Context) ->
    Args1 = append_extra_args(Args, Context),
    return_url(
        opt_abs_url(
            rewrite(make_url_for(Name, Args1, Escape, Context),
                    Name, Args1, Context),
            Name, Args1, Context)).


%% @doc Fetch the preferred hostname for this site
-spec hostname( z:context() ) -> binary() | undefined.
hostname(#context{ dispatcher = Dispatcher }) ->
    try
        gen_server:call(Dispatcher, 'hostname', infinity)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            undefined
    end.

%% @doc Fetch the preferred hostname, including port, for this site
-spec hostname_port( z:context() ) -> binary() | undefined.
hostname_port(#context{dispatcher=Dispatcher}) ->
    try
        gen_server:call(Dispatcher, 'hostname_port', infinity)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            undefined
    end.

%% @doc Fetch the preferred hostname for SSL, including port, for this site
-spec hostname_ssl_port( z:context() ) -> binary() | undefined.
hostname_ssl_port(#context{dispatcher=Dispatcher}) ->
    try
        gen_server:call(Dispatcher, 'hostname_ssl_port', infinity)
    catch
        exit:{noproc, {gen_server, call, _}} ->
            undefined
    end.

%% @doc Make the url an absolute url
abs_url(Url, Context) ->
    abs_url(Url, undefined, [], Context).

%% @doc Fetch the dispatchlist for the site.
-spec dispatchinfo( z:context() | pid() | atom() ) -> {ok, DispatchInfo} | {error, noproc} when
    DispatchInfo :: #{
        site := atom(),
        hostname := binary() | undefined,
        smtphost := binary() | undefined,
        hostalias := [ binary() ],
        redirect := boolean(),
        dispatch_list := [ z_sites_dispatcher:dispatch_rule() ],
        page_paths := #{ atom() | binary() => z_sites_dispatcher:dispatch_rsc_rule() }
    }.
dispatchinfo(#context{dispatcher=Dispatcher}) ->
    dispatchinfo(Dispatcher);
dispatchinfo(Server) when is_pid(Server) orelse is_atom(Server) ->
    try
        DispatchInfo = gen_server:call(Server, 'dispatchinfo', infinity),
        {ok, DispatchInfo}
    catch
        exit:{noproc, {gen_server, call, _}} ->
            {error, noproc}
    end.


%% @doc Update the dispatch list but don't reload it yet. Used when flushing all sites, see z:flush/0
update(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'reload', infinity).


%% @doc Reload all dispatch lists.  Finds new dispatch lists and adds them to the dispatcher
reload(#context{dispatcher=Dispatcher}) ->
    gen_server:call(Dispatcher, 'reload', infinity),
    z_sites_dispatcher:update_dispatchinfo().

reload(module_ready, Context) ->
    reload(Context).

name(SiteOrContext) ->
    z_utils:name_for_site(?MODULE, SiteOrContext).

%%====================================================================
%% Support routines, called outside the gen_server
%%====================================================================

%% @doc Rewrite the generated urls. Checks for zotonic_http_accept and checks modules.
rewrite({page_url, #dispatch_url{ url = Url } = D}, _Dispatch, Args, _Context) ->
    Url1 = iolist_to_binary(Url),
    D#dispatch_url{
        url = check_http_options(Url1, Args)
    };
rewrite(#dispatch_url{url = undefined} = D, _Dispatch, _Args, _Context) ->
    D;
rewrite(#dispatch_url{url = Url} = D, Dispatch, Args, Context) ->
    Url1 = iolist_to_binary(Url),
    Url2 = check_http_options(Url1, Args),
    D#dispatch_url{
        url = z_notifier:foldl(#url_rewrite{dispatch = Dispatch, args = Args}, Url2, Context)
    }.

check_http_options(Url, Args) ->
    case lists:keyfind(zotonic_http_accept, 1, Args) of
        {zotonic_http_accept, undefined} ->
            Url;
        {zotonic_http_accept, Mime} ->
            Mime1 = cow_qs:urlencode(z_convert:to_binary(Mime)),
            <<"/http-accept/", Mime1/binary, Url/binary>>;
        false ->
            Url
    end.


%% @doc Optionally make the url an absolute url
opt_abs_url(#dispatch_url{url=undefined} = D, _Dispatch, _Args, _Context) ->
    D;
opt_abs_url(#dispatch_url{url=Url, dispatch_options=DispatchOptions} = D, Dispatch, Args, Context) ->
    case use_absolute_url(Args, DispatchOptions, Context) of
        true -> D#dispatch_url{url=abs_url(Url, Dispatch, DispatchOptions, Context)};
        false -> D
    end.

abs_url(Url, Dispatch, DispatchOptions, Context) ->
    case z_notifier:first(#url_abs{dispatch=Dispatch, url=Url, dispatch_options=DispatchOptions}, Context) of
        undefined -> z_convert:to_binary(z_context:abs_url(Url, Context));
        AbsUrl -> z_convert:to_binary(AbsUrl)
    end.

%% @doc Convenience function, return the generated Url as a binary (or undefined if none).
return_url(#dispatch_url{ url = undefined }) -> undefined;
return_url(#dispatch_url{ url = Url }) -> iolist_to_binary(Url).

%% @doc Check if an url should be made an absolute url
use_absolute_url(Args, Options, Context) ->
    case to_bool(proplists:get_value(absolute_url, Args)) of
        false -> false;
        true -> true;
        undefined ->
            case to_bool(proplists:get_value(absolute_url, Options)) of
                false -> false;
                true -> true;
                undefined ->
                    case to_bool(z_context:get(absolute_url, Context)) of
                        false -> false;
                        true -> true;
                        undefined -> false
                    end
            end
    end.


to_bool(undefined) -> undefined;
to_bool(N) -> z_convert:to_bool(N).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server, loads the dispatch list into the webmachine dispatcher
init(Site) ->
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),
    ets:new(name(Site), [named_table, set, {keypos, 1}, protected, {read_concurrency, true}]),
    Context = z_context:new(Site),
    Hostname0 = m_site:get(hostname, Context),
    Hostname = drop_port(Hostname0),
    Smtphost = drop_port(m_site:get(smtphost, Context)),
    HostAlias = case m_site:get(hostalias, Context) of
        undefined -> [];
        HA -> HA
    end,
    Alias = lists:filtermap(
        fun(Alias) ->
            case drop_port(Alias) of
                undefined -> false;
                Alias1 -> {true, Alias1}
            end
        end,
        HostAlias),
    process_flag(trap_exit, true),
    IsRedirect = z_context:is_hostname_redirect_configured(Context),
    State  = #state{
                dispatch_list = [],
                page_paths = #{},
                lookup = #{},
                context = Context,
                site = Site,
                smtphost = Smtphost,
                hostname = Hostname,
                hostname_port = add_port(Hostname, http, z_config:get(port)),
                hostname_ssl_port = add_port(Hostname, https, z_config:get(ssl_port)),
                hostalias = Alias,
                redirect = IsRedirect
    },
    z_notifier:observe(module_ready, {?MODULE, reload}, Context),
    {ok, State}.

%% @doc Return the preferred hostname for the site
handle_call('hostname', _From, State) ->
    {reply, State#state.hostname, State};

%% @doc Return the preferred hostname, and port, for the site
handle_call('hostname_port', _From, State) ->
    {reply, State#state.hostname_port, State};

%% @doc Return the preferred hostname for ssl, and port, for the site
handle_call('hostname_ssl_port', _From, State) ->
    {reply, State#state.hostname_ssl_port, State};

%% @doc Return the dispatchinfo for the site.
handle_call('dispatchinfo', _From, State) ->
    {reply, #{
        site => State#state.site,
        hostname => State#state.hostname,
        smtphost => State#state.smtphost,
        hostalias => State#state.hostalias,
        redirect => State#state.redirect,
        dispatch_list => State#state.dispatch_list,
        page_paths => State#state.page_paths
     },
     State};

%% @doc Reload the dispatch list, signal the sites supervisor that the dispatch list has been changed.
%% The site supervisor will collect all dispatch lists and compile a new dispatcher module using
%% dispatch_compiler.
handle_call('reload', _From, State) ->
    State1 = reload_dispatch_list(State),
    {reply, ok, State1}.

%% @doc Handle casts.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(module_ready, State#state.context),
    ok.

%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


% @doc Drop the portnumber from the hostname
-spec drop_port( undefined | none | string() | binary() ) -> undefined | binary().
drop_port(undefined) -> undefined;
drop_port(none) -> undefined;
drop_port(<<>>) -> undefined;
drop_port(Hostname) when is_binary(Hostname) ->
    hd(binary:split(Hostname, <<":">>));
drop_port(Hostname) when is_list(Hostname) ->
    drop_port(z_convert:to_binary(Hostname)).

-spec add_port( binary() | undefined, http | https, pos_integer() ) -> binary() | undefined.
add_port(undefined, _Protocol, _Port) -> undefined;
add_port(_Hostname, _Protocol, none) -> undefined;
add_port(Hostname, http, 80) -> Hostname;
add_port(Hostname, https, 443) -> Hostname;
add_port(Hostname, _, Port) ->
    iolist_to_binary([ Hostname, $:, integer_to_list(Port) ]).

%% @doc Reload the dispatch list and update the ets table for the URL generation.
reload_dispatch_list(#state{ context = Context } = State) ->
    DispatchRules = try
        collect_dispatch_lists(Context)
    catch
        _:{error, _Msg} ->
           State#state.dispatch_list
    end,
    LookupMap = dispatch_for_uri_lookup(DispatchRules),
    update_ets(LookupMap, Context),
    {DispatchList, RscPathList} = lists:partition(fun is_dispatch/1, DispatchRules),
    PagePathsLookup = page_path_lookup(RscPathList),
    State#state{
        dispatch_list = DispatchList,
        page_paths = PagePathsLookup,
        lookup = LookupMap
    }.

update_ets(LookupMap, Context) ->
    Name = name(Context),
    LookupList = maps:to_list(LookupMap),
    Current = ets:match(Name, {'$1', '_'}),
    DelKeys = lists:flatten(Current) -- maps:keys(LookupMap),
    lists:foreach(fun(K) -> ets:delete(Name, K) end, DelKeys),
    ets:insert(Name, LookupList).

is_dispatch({_Name, Path, _Controller, _Options}) when is_list(Path) -> true;
is_dispatch({_Name, RscName, _Controller, _Options}) when is_atom(RscName) -> false.

page_path_lookup(RscPathList) ->
    lists:foldr(
        fun({_Name, RscName, _Controller, _Options} = Disp, Acc) ->
            NameBin = z_convert:to_binary(RscName),
            Acc#{
                RscName => Disp,
                NameBin => Disp
            }
        end,
        #{},
        RscPathList).

%% @doc Collect all dispatch lists.  Checks priv/dispatch for all dispatch list definitions.
collect_dispatch_lists(Context) ->
    ModDispOnPrio = lists:concat(
        lists:map(
            fun({Mod, ModFiles}) ->
                lists:sort( [ {F, Mod} || F <- ModFiles ] )
            end,
            z_module_indexer:dispatch(Context))
        ),
    Dispatch = lists:map(fun get_file_dispatch/1, ModDispOnPrio),
    lists:flatten(Dispatch).


%% @doc Read a dispatch file, the file should contain a valid Erlang dispatch datastructure.
get_file_dispatch({File, Mod}) ->
    try
        case filelib:is_regular(File)
            andalso not zotonic_filewatcher_handler:is_file_blocked(File)
        of
            true ->
                Basename = filename:basename(File),
                case Basename of
                    "." ++ _ ->
                        [];
                    <<".", _/binary>> ->
                        [];
                    _Other  ->
                        {ok, Disp} = file:consult(File),
                        Disp1 = split_paths(lists:flatten(Disp), Mod, File),
                        add_mod_to_options(Disp1, Mod, filename:basename(File))
                end;
            false ->
                []
        end
    catch
        M:E ->
            ?LOG_ERROR(#{
                text => <<"Dispatch file parse error">>,
                in => zotonic_core,
                result => M,
                reason => E,
                module => Mod,
                file => File
            }),
            throw({error, "Parse error in " ++ z_convert:to_list(File)})
    end.

%% @doc Check all dispatch rules for proper format, complain about non matching ones.
%% Split paths like "/path/to/:var" into a list of path segments.
%% Drop all non conforming dispatch paths, and log errors for those.
split_paths(Disp, Mod, File) ->
    lists:filtermap(
        fun
            ({Name, [ $/ | Path ], Controller, Opts}) when is_atom(Name), is_atom(Controller), is_list(Opts) ->
                Path1 = split_binary_path(unicode:characters_to_binary(Path)),
                {true, {Name, Path1, Controller, Opts}};
            ({Name, <<"/", Path/binary>>, Controller, Opts}) when is_atom(Name), is_atom(Controller), is_list(Opts) ->
                Path1 = split_binary_path(Path),
                {true, {Name, Path1, Controller, Opts}};
            ({Name, Path, Controller, Opts}) when is_atom(Name), is_list(Path), is_atom(Controller), is_list(Opts) ->
                {true, {Name, Path, Controller, Opts}};
            ({Name, RscName, Controller, Opts}) when is_atom(Name), is_atom(Name), is_atom(Controller), is_list(Opts) ->
                {true, {Name, RscName, Controller, Opts}};
            (Other) ->
                ?LOG_ERROR(#{
                    in => zotonic_core,
                    text => <<"Unrecognized dispatch rule">>,
                    result => error,
                    reason => format,
                    rule => Other,
                    module => Mod,
                    file => File
                }),
                false
        end,
        Disp).

split_binary_path(Path) ->
    Ps = binary:split(Path, <<"/">>, [ global ]),
    lists:filtermap(
        fun
            (<<>>) -> false;
            (<<"*">>) -> {true, '*'};
            (<<":", Var/binary>>) -> {true, binary_to_atom(Var, utf8)};
            (P) -> {true, P}
        end,
        Ps).

add_mod_to_options(Disp, Mod, Filename) ->
    F = z_convert:to_binary(Filename),
    lists:map(
        fun({Name, Path, Controller, Opts}) ->
            Opts1 = [
                {zotonic_dispatch_module, Mod},
                {zotonic_dispatch_file, F}
                | Opts
            ],
            {Name, Path, Controller, Opts1}
        end,
        Disp).

%% @doc Transform the dispatchlist into a datastructure for building uris from name/vars
%% Datastructure needed is:   name -> [vars, pattern]
dispatch_for_uri_lookup(DispatchList) ->
    dispatch_for_uri_lookup1(DispatchList, #{}).

dispatch_for_uri_lookup1([], LookupAcc) ->
    LookupAcc;
dispatch_for_uri_lookup1([{Name, RscName, Controller, DispatchOptions}|T], LookupAcc)
    when is_atom(Name), is_atom(RscName), is_atom(Controller), is_list(DispatchOptions) ->
    Current = maps:get(Name, LookupAcc, []),
    LookupAcc1 = LookupAcc#{
        Name => [ {0, [], RscName, DispatchOptions} | Current ]
    },
    dispatch_for_uri_lookup1(T, LookupAcc1);
dispatch_for_uri_lookup1([{Name, Pattern, Controller, DispatchOptions}|T], LookupAcc)
    when is_atom(Name), is_list(Pattern), is_atom(Controller), is_list(DispatchOptions) ->
    Vars  = lists:foldl(fun(A, Acc) when is_atom(A) -> [A|Acc];
                           ({A,_RegExp}, Acc) when is_atom(A) -> [A|Acc];
                           (_, Acc) -> Acc
                        end,
                        [],
                        Pattern),
    Current = maps:get(Name, LookupAcc, []),
    LookupAcc1 = LookupAcc#{
        Name => [ {length(Vars), Vars, Pattern, DispatchOptions} | Current ]
    },
    dispatch_for_uri_lookup1(T, LookupAcc1);
dispatch_for_uri_lookup1([IllegalDispatch|T], LookupAcc) ->
    ?LOG_ERROR(#{
        text => <<"Dispatcher dropping malformed dispatch rule">>,
        in => zotonic_core,
        result => error,
        reason => malformed,
        dispatch_rule => IllegalDispatch
    }),
    dispatch_for_uri_lookup1(T, LookupAcc).


%% @doc Make an uri for the named dispatch with the given parameters
make_url_for(Name, Args, Escape, _Context) when Name =:= none; Name =:= <<"none">> ->
    QueryStringArgs = filter_empty_args(Args),
    Sep = case Escape of
            xml  -> "&amp;";
            html -> "&amp;";
            _    -> $&
          end,
    #dispatch_url{
        url = z_convert:to_binary([$?, urlencode(QueryStringArgs, Sep)]),
        dispatch_options = []
    };
make_url_for(Name, Args, Escape, Context) ->
    Name1 = z_convert:to_atom(Name),
    Args1 = filter_empty_args(Args),
    case ets:lookup(name(Context), Name1) of
        [] ->
            #dispatch_url{};
        [{_, Patterns}] ->
            case make_url_for1(Args1, Patterns, Escape, undefined, Context) of
                #dispatch_url{ url = undefined } = DispUrl when Name =/= image->
                    ?LOG_INFO(#{
                        text => <<"Dispatcher make_url_for failed">>,
                        in => zotonic_core,
                        dispatch_rule => Name1,
                        args => Args1,
                        patterns => Patterns,
                        escape => Escape
                    }),
                    DispUrl;
                DispUrl ->
                    DispUrl
            end
    end.


%% @doc Filter out empty dispatch arguments before making an URL.
filter_empty_args(Args) ->
    lists:filter(
      fun
          ({_, <<>>}) -> false;
          ({_, []}) -> false;
          ({_, undefined}) -> false;
          ({absolute_url, _}) -> false;
          (absolute_url) -> false;
          ({zotonic_http_accept, _}) -> false;
          (_) -> true
      end, Args).


%% @doc Try to match all patterns with the arguments
make_url_for1(_Args, [], _Escape, undefined, _Context) ->
    #dispatch_url{};
make_url_for1(_Args, [], Escape, {QueryStringArgs, RscName, DispOpts}, Context) when is_atom(RscName) ->
    PageUrl = m_rsc:p(RscName, <<"page_url">>, Context),
    case QueryStringArgs of
        [] ->
            {page_url, #dispatch_url{
                url = z_convert:to_binary(PageUrl),
                dispatch_options = DispOpts
            }};
        _  ->
            Sep = case Escape of
                    xml  -> "&amp;";
                    html -> "&amp;";
                    _    -> $&
                  end,
            {page_url, #dispatch_url{
                url = z_convert:to_binary([PageUrl, $?, urlencode(QueryStringArgs, Sep)]),
                dispatch_options = DispOpts
            }}
    end;
make_url_for1(Args, [], Escape, {QueryStringArgs, Pattern, DispOpts}, _Context) ->
    ReplArgs =  fun
                    ('*') -> path_argval('*', Args);
                    (V) when is_atom(V) -> path_argval(V, Args);
                    ({V, _Pattern}) when is_atom(V) ->
                        mochiweb_util:quote_plus(path_argval(V, Args));
                    (S) ->
                        S
                end,
    UriParts = lists:map(ReplArgs, Pattern),
    Uri      = [$/ | lists:join($/, UriParts)],
    case QueryStringArgs of
        [] ->
            #dispatch_url{
                url = z_convert:to_binary(Uri),
                dispatch_options = DispOpts
            };
        _  ->
            Sep = case Escape of
                    xml  -> "&amp;";
                    html -> "&amp;";
                    _    -> $&
                  end,
            #dispatch_url{
                url = z_convert:to_binary([Uri, $?, urlencode(QueryStringArgs, Sep)]),
                dispatch_options = DispOpts
            }
    end;
make_url_for1(Args, [Pattern|T], Escape, Best, Context) ->
    Best1 = select_best_pattern(Args, Pattern, Best),
    make_url_for1(Args, T, Escape, Best1, Context).

path_argval('*', Args) ->
    case proplists:get_value(star, Args) of
        undefined -> <<>>;
        L when is_list(L) ->
            List1 = [ cow_qs:urlencode(z_convert:to_binary(B)) || B <- L ],
            lists:join($/, List1);
        V ->
            z_convert:to_binary(V)
    end;
path_argval(Arg, Args) ->
    B = z_convert:to_binary(proplists:get_value(Arg, Args, <<"-">>)),
    cow_qs:urlencode(B).

select_best_pattern(Args, {0, [], RscName, DispOpts}, Best) when is_atom(RscName) ->
    select_best_pattern1({Args, RscName, DispOpts}, Best);
select_best_pattern(Args, {PCount, PArgs, Pattern, DispOpts}, Best) when is_list(Pattern) ->
    if
        length(Args) >= PCount ->
            %% Check if all PArgs are part of Args
            {PathArgs, QueryStringArgs} = lists:partition(
                                            fun
                                                ({star,_}) -> lists:member('*', PArgs);
                                                ({A,_}) -> lists:member(A, PArgs)
                                            end, Args),
            case length(PathArgs) of
                PCount ->
                    % Could fill all path args, this match satisfies
                    select_best_pattern1({QueryStringArgs,Pattern,DispOpts}, Best);
                _ ->
                    Best
            end;
        true ->
            Best
    end.

select_best_pattern1(A, undefined) ->
    A;
select_best_pattern1({AQS, _APat, _AOpts}=A, {BQS, _BPat, _BOpts}=B) ->
    if
        length(BQS) > length(AQS) -> A;
        true -> B
    end.


%% @spec urlencode([{Key, Value}], Join) -> string()
%% @doc URL encode the property list.
urlencode(Props, Join) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[mochiweb_util:quote_plus(K), $=, mochiweb_util:quote_plus(V)] | Acc]
                           end, [], Props),
    lists:flatten(revjoin(RevPairs, Join, [])).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).


%% @doc Append extra arguments to the url, depending if 'qargs' or 'varargs' is set.
append_extra_args(Args, Context) when is_map(Args) ->
    append_extra_args(maps:to_list(Args), Context);
append_extra_args(Args, Context) ->
    append_qargs(append_varargs(Args, Context), Context).


%% @doc Append all query arguments iff they are not mentioned in the arglist and if qargs parameter is set
append_qargs(Args, Context) ->
    case proplists:get_value(qargs, Args) of
        undefined ->
            Args;
        false ->
            proplists:delete(qargs, Args);
        true ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(z_context:get_qargs(Context), Args1);
        L when is_list(L) ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(L, Args1);
        M when is_map(M) ->
            Args1 = proplists:delete(qargs, Args),
            merge_qargs(maps:to_list(M), Args1)
    end.

merge_qargs([], Args) ->
    Args;
merge_qargs(Qs, Args) ->
    Ks = [ z_convert:to_binary(A) || {A, _} <- Args ],
    lists:foldr(
        fun({A, _} = AV, Acc) ->
            case lists:member(A, Ks) of
                true ->
                    Acc;
                false ->
                    [ AV | Acc ]
            end
        end,
        Args,
        Qs).

%% @doc Append all varargs argument, names are given in a list.
append_varargs(Args, Context) ->
    case proplists:get_value(varargs, Args) of
        undefined ->
            Args;
        Varargs ->
            append_varargs(Varargs, proplists:delete(varargs, Args), Context)
    end.

append_varargs([], Args, _Context) ->
    Args;
append_varargs([{Name, Value}|Varargs], Args, Context) ->
    append_varargs(Varargs, append_vararg(Name, Value, Args), Context);
append_varargs([[Name, Value]|Varargs], Args, Context) ->
    append_varargs(Varargs, append_vararg(Name, Value, Args), Context);
append_varargs([Name|Varargs], Args, Context) ->
    Key = z_convert:to_atom(Name),
    append_varargs(Varargs, append_vararg(Key, z_context:get(Key, Context), Args), Context).

append_vararg(Name, Value, Args) ->
    Key = z_convert:to_atom(Name),
    case proplists:is_defined(Key, Args) of
        true -> Args;
        false -> [{Key, Value}|Args]
    end.
