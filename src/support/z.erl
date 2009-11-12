%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-08
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
    n/2,
    n1/2,
    m/0,
    flush/0,
	flush/1,
    restart/0
]).

-include_lib("zotonic.hrl").

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
	[ flush(C) || C <- z_sites_sup:get_site_contexts() ],
	z_sites_sup:update_dispatchinfo().
	
flush(Context) ->
   	z_depcache:flush(Context),
   	z_dispatcher:reload(Context),
   	n({module_ready}, Context).

%% @doc Full restart of Zotonic
restart() ->
    zotonic:stop(),
    zotonic:start().

