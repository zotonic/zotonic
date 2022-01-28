%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Zotonic Launcher, launches the Zotonic application server with
%%      the Zotonic Core, file watchers, and port listeners.

%% Copyright 2017 Marc Worrell
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

-module(zotonic_launcher_app).

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1,

    is_root/0,

    load_configs/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

start() ->
    case load_configs(node()) of
        {ok, ZotonicConfigFiles} ->
            %
            % Show the startup message
            %
            ?LOG_NOTICE("Zotonic starting"),
            [ ?LOG_NOTICE("Init file: - ~s", [Cfg]) || Cfg <- zotonic_launcher_config:erlang_config_files( node() ) ],
            [ ?LOG_NOTICE("Config file: ~s", [Cfg]) || Cfg <- ZotonicConfigFiles ],
            %
            % Start the launcher and Zotonic
            %
            ensure_started(zotonic_launcher);
        {error, _} = Error ->
            Error
    end.

start(_StartType, _StartArgs) ->
    case is_root() of
        true ->
            ?LOG_CRITICAL("Not running as root."),
            {error, not_running_as_root};
        false ->
            write_pidfile(),
            zotonic_launcher_sup:start_link()
    end.


-spec is_root() -> boolean().
is_root() ->
    os:getenv("USER") =:= "root".

%%--------------------------------------------------------------------
stop(_State) ->
    remove_pidfile(),
    ok.


%% @doc Load all configurations and initialize Zotonic core.
-spec load_configs( node() ) -> {ok, list( file:filename_all() )} | {error, term()}.
load_configs(Node) ->
    ensure_started(yamerl),
    load_applications(),
    ZotonicCfgs = zotonic_launcher_config:zotonic_config_files( Node ),
    case load_config_files(ZotonicCfgs) of
        ok ->
            zotonic_core:setup(Node),
            {ok, ZotonicCfgs};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Load the applications so that their initial settings are also loaded.
load_applications() ->
    application:load(setup),
    application:load(mnesia),
    application:load(zotonic_core).

load_config_files(ZotonicCfgs) ->
    case zotonic_launcher_config:read_configs( ZotonicCfgs ) of
        {ok, Config} ->
            zotonic_launcher_config:load_configs(Config);
        {error, _} = Error ->
            error_logger:error_msg("Fatal error reading configuration files: ~p", [ Error ]),
            Error
    end.


-spec ensure_started(atom()) -> ok | {error, term()}.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            case ensure_started(Dep) of
                ok -> ensure_started(App);
                {error, _} = Error -> Error
            end;
        {error, {already_started, App}} ->
            ok;
        {error, {Tag, Msg}} when is_list(Tag), is_list(Msg) ->
            {error, lists:flatten(io_lib:format("~s: ~s", [Tag, Msg]))};
        {error, {bad_return, {{M, F, Args}, Return}}} ->
            A = string:join([io_lib:format("~p", [A])|| A <- Args], ", "),
            {error, lists:flatten(
                        io_lib:format("~s failed to start due to a bad return value from call ~s:~s(~s):~n~p",
                                      [App, M, F, A, Return]))};
        {error, Reason} ->
            {error, Reason}
    end.


-spec get_pidfile() -> file:filename().
get_pidfile() ->
    case os:getenv("ZOTONIC_PIDFILE") of
        false ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, "zotonic.pid");
        File ->
            File
    end.

-spec write_pidfile() -> ok | {error, term()}.
write_pidfile() ->
    case file:open(get_pidfile(), [write]) of
        {ok, F} ->
            ok = file:write(F, os:getpid()),
            ok = file:close(F);
        {error, Reason} ->
            ?LOG_ERROR("Could not write ZOTONIC_PIDFILE \"~s\" error: ~p",
                        [get_pidfile(), Reason]),
            {error, Reason}
    end.


-spec remove_pidfile() -> ok | {error, term()}.
remove_pidfile() ->
    case file:delete(get_pidfile()) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Could not delete ZOTONIC_PIDFILE \"~s\" error: ~p",
                        [get_pidfile(), Reason]),
            {error, Reason}
    end.

