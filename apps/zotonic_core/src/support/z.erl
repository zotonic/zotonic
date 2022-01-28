%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%%
%% @doc Some easy shortcut functions.

%% Copyright 2009-2022 Marc Worrell
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

-module(z).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    c/1,

    n/2,
    n1/2,
    m/0,
    compile/0,
    flush/0,
    flush/1,
    restart/0,
    restart/1,

    ld/0,
    ld/1,

    reindex/0,

    shell_stopsite/1,
    shell_startsite/1,
    shell_restartsite/1,

    dispatch_url/1,
    dispatch_path/2,
    dispatch_list/1,

    debug_msg/3,

    log/3,

    debug/2,
    debug/3,
    debug/4,
    info/2,
    info/3,
    info/4,
    notice/2,
    notice/3,
    notice/4,
    warning/2,
    warning/3,
    warning/4,
    error/2,
    error/3,
    error/4
]).

-include("zotonic.hrl").

%% DTAP environment
-type environment() :: development
                    | test
                    | acceptance
                    | production
                    | education
                    | backup.

-type context() :: #context{}.
-type validation_error() :: invalid | novalue | {script, iodata()} | novalidator | string().
-type trans() :: #trans{}.
-type qvalue() :: binary() | string() | #upload{} | term().

-type severity() :: debug | info | warning | error | fatal.

-export_type([
    context/0,
    trans/0,
    environment/0,
    validation_error/0,
    severity/0,
    qvalue/0
]).

% @doc Return a new context
-spec c( atom() ) -> z:context().
c(Site) ->
    z_context:new(Site).

%% @doc Send a notification
n(Msg, Context) ->
    z_notifier:notify(Msg, Context).

%% @doc Send a notification to the first observer
n1(Msg, Context) ->
    z_notifier:first(Msg, Context).

%% @doc (Re)make all erlang source modules and reset the caches.
m() ->
    case compile() of
        ok -> flush(), ok;
        Other -> Other
    end.

%% @doc (Re)make all erlang source modules with the supplied compile
%% options. Do not reset the caches.
compile() ->
    zotonic_filehandler:compile_all().

%% @doc Reset all caches, reload the dispatch rules and rescan all modules.
-spec flush() -> ok.
flush() ->
    zotonic_fileindexer:flush(),
    [ flush(C) || C <- z_sites_manager:get_site_contexts() ],
    z_sites_dispatcher:update_dispatchinfo().

-spec flush( atom() | z:context() ) -> ok.
flush(Site) when is_atom(Site) ->
    flush(c(Site));
flush(Context) ->
    z_depcache:flush(Context),
    z_dispatcher:reload(Context),
    n(module_ready, Context).

%% @doc Reindex all sites, find new files.
reindex() ->
    zotonic_fileindexer:flush(),
    z_module_indexer:reindex().

%% @doc Full restart of Zotonic
restart() ->
    application:stop(zotonic_core),
    application:start(zotonic_core).

%% @doc Restart a site
restart(Site) ->
    z_sites_manager:restart(Site).

%% @doc Reload all changed Erlang modules
ld() ->
    zotonic_filehandler:reload_modules().

%% @doc Reload an Erlang module
ld(Module) ->
    zotonic_filehandler:reload_module(Module).

%% @doc Shell commands: start a site
shell_startsite(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            running;
        {ok, _Status} ->
            z_sites_manager:start(Site);
        {error, bad_name} ->
            bad_name
    end.

%% @doc Shell commands: stop a site
shell_stopsite(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, stopped} ->
            stopped;
        {ok, _Status} ->
            z_sites_manager:stop(Site);
        {error, bad_name} ->
            bad_name
    end.

%% @doc Shell commands: stop a site
shell_restartsite(Site) ->
    z_sites_manager:stop(Site),
    shell_startsite(Site).

%% @doc Dispatch an URL - find matching site and dispatch the path
dispatch_url(Url) ->
    case uri_string:parse(Url) of
        #{ host := Host, path := Path } ->
            case z_sites_dispatcher:get_site_for_hostname(Host) of
                {ok, Site} ->
                    dispatch_path(Path, Site);
                undefined ->
                    case z_sites_dispatcher:get_fallback_site() of
                        {ok, FallbackSite} ->
                            dispatch_path(Path, FallbackSite);
                        undefined ->
                            {error, unknown_host}
                    end
            end;
        _ ->
            {error, url}
    end.

%% @doc Shell command: dispatch a path, return trace
dispatch_path(Path, Site) when is_atom(Site), is_binary(Path) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            dispatch_path(Path, z_context:new(Site));
        {ok, Status} ->
            {error, Status};
        {error, bad_name} ->
            {error, bad_name}
    end;
dispatch_path(Path, #context{} = Context) when is_binary(Path) ->
    z_sites_dispatcher:dispatch_trace(Path, Context).

%% @doc Return the complete dispatch information for the site.
dispatch_list(SiteOrContext) ->
    case z_sites_dispatcher:fetch_dispatchinfo(SiteOrContext) of
        {ok, #site_dispatch_list{
                site=Site, hostname=Hostname, smtphost=SmtpHost, hostalias=Hostalias,
                redirect=Redirect, dispatch_list=DispatchList
            }} ->
            DL = lists:map(
                fun({Disp, Path, Controller, Opts}) ->
                    #{
                        dispatch => Disp,
                        path => Path,
                        controller => Controller,
                        controller_options => Opts
                    }
                end,
                DispatchList),
            {ok, #{
                site => Site,
                hostname => Hostname,
                smtphost => SmtpHost,
                hostalias => Hostalias,
                is_redirect => Redirect,
                dispatch_list => DL
            }};
        {error, _} = Error ->
            Error
    end.

%% @doc Echo and return a debugging value
debug_msg(Module, Line, Msg) ->
    logger:log(
        notice,
        "DEBUG: ~p:~p  ~p~n",
        [Module, Line, Msg],
        #{
            file => Module,
            line => Line
        }),
    Msg.

%% @doc Log a debug message, with extra props.
debug(Msg, Context)        -> log(debug, Msg, [], Context).
debug(Msg, Props, Context) -> log(debug, Msg, Props, Context).
debug(Msg, Args, Props, Context) -> log(debug, Msg, Args, Props, Context).

%% @doc Log an informational message.
info(Msg, Context)         -> log(info, Msg, [], Context).
info(Msg, Props, Context)  -> log(info, Msg, Props, Context).
info(Msg, Args, Props, Context)  -> log(info, Msg, Args, Props, Context).

%% @doc Log a  notice.
notice(Msg, Context)         -> log(notice, Msg, [], Context).
notice(Msg, Props, Context)  -> log(notice, Msg, Props, Context).
notice(Msg, Args, Props, Context)  -> log(notice, Msg, Args, Props, Context).

%% @doc Log a warning.
warning(Msg, Context)         -> log(warning, Msg, [], Context).
warning(Msg, Props, Context)  -> log(warning, Msg, Props, Context).
warning(Msg, Args, Props, Context)  -> log(warning, Msg, Args, Props, Context).

%% @doc Log a error.
error(Msg, Context)         -> log(error, Msg, [], Context).
error(Msg, Props, Context)  -> log(error, Msg, Props, Context).
error(Msg, Args, Props, Context)  -> log(error, Msg, Args, Props, Context).


-spec log( severity(), proplists:proplist(), z:context() ) -> ok.
log(Type, Props, Context) when is_atom(Type), is_list(Props) ->
    UserId = case proplists:lookup(user_id, Props) of
        none -> z_acl:user(Context);
        {user_id, UId} -> UId
    end,
    z_notifier:notify(
        #zlog{
            type = Type,
            user_id = UserId,
            timestamp = os:timestamp(),
            props = Props
        },
        Context),
    ok.

-spec log( severity(), string(), list(), proplists:proplist() | map(), z:context() ) -> ok.
log(Type, Msg, Args, Props, Context) ->
    Msg1 = lists:flatten(io_lib:format(Msg, Args)),
    log(Type, Msg1, Props, Context).

-spec log( severity(), iodata(), proplists:proplist() | map(), z:context() ) -> ok.
log(Type, Msg, Props, Context) when is_list(Props) ->
    log(Type, Msg, maps:from_list(Props), Context);
log(Type, Msg, Props, Context) ->
    Msg1 = erlang:iolist_to_binary(Msg),
    Line = maps:get(line, Props, 0),
    Module = maps:get(module, Props, unknown),
    logger(Type, Props, [ z_context:site(Context), Type, Module, Line, Msg1 ]),
    UserId = case maps:get(user_id, Props, none) of
        none -> z_acl:user(Context);
        UId -> UId
    end,
    z_notifier:notify(
        #zlog{
            type = Type,
            user_id = UserId,
            timestamp = os:timestamp(),
            props = #log_message{
                type = Type,
                message = Msg1,
                props = maps:to_list(Props),
                user_id = UserId
            }
        },
        Context),
    ok.

logger(debug, Props, Args) ->
    ?LOG_DEBUG("[~p] ~p @ ~p:~p  ~s~n", Args, Props);
logger(info, Props, Args) ->
    ?LOG_INFO("[~p] ~p @ ~p:~p  ~s~n", Args, Props);
logger(notice, Props, Args) ->
    ?LOG_NOTICE("[~p] ~p @ ~p:~p  ~s~n", Args, Props);
logger(warning, Props, Args) ->
    ?LOG_WARNING("[~p] ~p @ ~p:~p  ~s~n", Args, Props);
logger(_Severity, Props, Args) ->
    ?LOG_ERROR("[~p] ~p @ ~p:~p  ~s~n", Args, Props).
