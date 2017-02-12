%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%%
%% @doc Some easy shortcut functions.

%% Copyright 2009-2015 Marc Worrell
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
    m/1,
    compile/0,
    compile/1,
    flush/0,
    flush/1,
    restart/0,
    restart/1,

    ld/0,
    ld/1,

    shell_stopsite/1,
    shell_startsite/1,
    shell_restartsite/1,

    debug_msg/3
]).

-include("zotonic.hrl").

-type context() :: #context{}.
-type validation_error() :: invalid | novalue | {script, string()} | novalidator | string().

-export_type([
    context/0,
    validation_error/0
]).

% @doc Return a new context
c(Site) ->
    z_context:new(Site).

%% @doc Send a notification
n(Msg, Context) ->
    z_notifier:notify(Msg, Context).

%% @doc Send a notification to the first observer
n1(Msg, Context) ->
    z_notifier:notify1(Msg, Context).

%% @doc (Re)make all erlang source modules and reset the caches.
m() ->
    m([]).

%% @doc (Re)make all erlang source modules with the supplied compile
%% options and reset the caches.
m(Options) ->
    case compile(Options) of
        ok -> flush();
        error -> error
    end.

%% @doc (Re)make all erlang source modules with the supplied compile
%% options. Do not reset the caches.
compile() ->
    zotonic_compile:all().

compile(Options) ->
    zotonic_compile:all(Options).

%% @doc Reset all caches, reload the dispatch rules and rescan all modules.
flush() ->
    [flush(C) || C <- z_sites_manager:get_site_contexts()],
    z_sites_dispatcher:update_dispatchinfo().

flush(Site) when is_atom(Site) ->
    flush(c(Site));
flush(Context) ->
    z_depcache:flush(Context),
    z_dispatcher:reload(Context),
    n(module_ready, Context).

%% @doc Full restart of Zotonic
restart() ->
    zotonic:stop(),
    zotonic:start().

%% @doc Restart a site
restart(Site) ->
    z_sites_manager:restart(Site).

%% @doc Reload all changed Erlang modules
ld() ->
    zotonic_compile:ld().

%% @doc Reload an Erlang module
ld(Module) ->
    zotonic_compile:ld(Module).

%% @doc Shell commands: start a site
shell_startsite(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, stopped} ->
            z_sites_manager:start(Site);
        {ok, Status} ->
            Status;
        {error, notfound} ->
            notfound
    end.

%% @doc Shell commands: stop a site
shell_stopsite(Site) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, stopped} ->
            stopped;
        {ok, _Status} ->
            z_sites_manager:stop(Site);
        {error, notfound} ->
            notfound
    end.

%% @doc Shell commands: stop a site
shell_restartsite(Site) ->
    z_sites_manager:stop(Site),
    shell_startsite(Site).

%% @doc Echo and return a debugging value
debug_msg(Module, Line, Msg) ->
    error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [Module, Line, Msg]),
    Msg.
