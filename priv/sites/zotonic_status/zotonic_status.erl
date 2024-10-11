%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell <marc@worrell.nl>
%% Date: 2011-12-23

%% @doc Default Zotonic site, used when no other site can handle the supplied Host.

%% Copyright 2011 Marc Worrell
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

-module(zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Status").
-mod_descr("Default Zotonic site, used when no other site can handle the supplied Host.").
-mod_prio(10).
-mod_depends([base, bootstrap]).

-export([
    observe_user_is_enabled/2,
    observe_acl_logon/2,
    observe_acl_logoff/2,
    observe_tick_10m/2
]).

-include_lib("zotonic.hrl").

%% @doc Check if an user is enabled.
observe_user_is_enabled(#user_is_enabled{id=UserId}, _Context) ->
    UserId == 1.

%% @doc Let the user log on, this is the moment to start caching information.
observe_acl_logon(#acl_logon{id=UserId}, Context) ->
    Context#context{user_id=UserId}.

%% @doc Let the user log off, clean up any cached information.
observe_acl_logoff(#acl_logoff{}, Context) ->
    Context#context{acl=undefined, user_id=undefined}.

%%====================================================================
%% PERIODIC TASKS
%%====================================================================

%% @doc For all running sites, check every 10 minutes if each site
%% module is active and if not (it stopped unexpectedly), restart them.
observe_tick_10m(tick_10m, Context) ->
    ?zDebug("Checking for crashed site modules..", Context),
    lists:foreach(
      fun ([Site, running|_]) -> restart_site_module_if_not_running(Site);
          (_) -> noop
      end,
      z_sites_manager:get_sites_status()
     ).

%% @doc Try to restart the site module if it is not running.
-spec restart_site_module_if_not_running(Module::atom()) -> noop | ok | {error, not_found}.
restart_site_module_if_not_running(Site) ->
    %% An z_module_manager:active/2 also exists, but it turns out
    %% that is not reliable for querying the actual site module
    %% status (due to caching?).
    case lists:member(Site, z_module_manager:active(z:c(Site))) of
        true ->
            noop;
        false ->
            ?zWarning("Restarting site module ~s because it was off while the site is active", [ Site ], z:c(Site)),
            z_module_manager:restart(Site, z:c(Site))
    end.
