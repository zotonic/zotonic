%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-08
%%
%% @doc Some easy shortcut functions.

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

-module(z).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    c/1,
    
    n/2,
    n1/2,
    m/0,
    flush/0,
    flush/1,
    restart/0,

    debug_msg/3,

    debug/2,
    debug/3,
    debug/4,
    info/2,
    info/3,
    info/4,
    warning/2,
    warning/3,
    warning/4
]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_log.hrl").

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
    make:all([load]), 
    flush().

%% @doc Reset all caches, reload the dispatch rules and rescan all modules.
flush() ->
    [ flush(C) || C <- z_sites_manager:get_site_contexts() ],
    z_sites_dispatcher:update_dispatchinfo().
	
flush(Context) ->
   	z_depcache:flush(Context),
   	z_dispatcher:reload(Context),
   	n(module_ready, Context).

%% @doc Full restart of Zotonic
restart() ->
    zotonic:stop(),
    zotonic:start().


%% @doc Echo and return a debugging value
debug_msg(Module, Line, Msg) ->
	error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [Module, Line, Msg]),
	Msg.

%% @doc Log a debug message, with extra props.
debug(Msg, Context)        -> log(debug, Msg, [], Context).
debug(Msg, Props, Context) -> log(debug, Msg, Props, Context).
debug(Msg, Args, Props, Context) -> log(debug, Msg, Args, Props, Context).

%% @doc Log an informational message.
info(Msg, Context)         -> log(info, Msg, [], Context).
info(Msg, Props, Context)  -> log(info, Msg, Props, Context).
info(Msg, Args, Props, Context)  -> log(info, Msg, Args, Props, Context).

%% @doc Log a warning.
warning(Msg, Context)         -> log(warning, Msg, [], Context).
warning(Msg, Props, Context)  -> log(warning, Msg, Props, Context).
warning(Msg, Args, Props, Context)  -> log(warning, Msg, Args, Props, Context).


log(Type, Msg, Args, Props, Context) ->
    Msg1 = lists:flatten(io_lib:format(Msg, Args)),
    log(Type, Msg1, Props, Context).

log(Type, Msg, Props, Context) ->
    Msg1 = erlang:iolist_to_binary(Msg),
    Line = proplists:get_value(line, Props, 0),
    Module = proplists:get_value(module, Props, unknown),
    error_logger:info_msg("[~p] ~p @ ~p:~p  ~s~n", [Context#context.host, Type, Module, Line, binary_to_list(Msg1)]),
    z_notifier:notify({log, #log_message{type=Type, message=Msg1, props=Props, user_id=z_acl:user(Context)}}, Context).

