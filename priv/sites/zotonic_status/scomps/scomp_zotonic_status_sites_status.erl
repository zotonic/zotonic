%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-24

%% @doc Sites list

%% Copyright 2011 Arjan Scherpenisse
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

-module(scomp_zotonic_status_sites_status).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([vary/2, render/3, updater/2]).

vary(_Params, _Context) -> nocache.

render(_Params, _Vars, Context) ->
    SitesStatus = z_sites_manager:get_sites_status(),
    z_session_page:spawn_link(?MODULE, updater, [SitesStatus, Context], Context),
    {ok, []}.

%% -----------------------------------------------------------------------------------------------
%% Stream process to update the page when data changes
%% -----------------------------------------------------------------------------------------------

% @todo Instead of polling we should observe the system wide notifications (that will be implemented)
updater(SitesStatus, Context) ->
    Context1 = z_auth:logon_from_session(Context),
    timer:sleep(1000),
    z_sites_manager:upgrade(),
    NewStatus = z_sites_manager:get_sites_status(),
    case NewStatus /= SitesStatus of
        true ->
            Context2 = render_update(NewStatus, Context1),
            ?MODULE:updater(NewStatus, Context2);
        false ->
            ?MODULE:updater(SitesStatus, Context1)
    end.


render_update(SitesStatus, Context) ->
    Vars = [
        {has_user, z_acl:user(Context)},
        {configs, [ {Site, z_sites_manager:get_site_config(Site)} || Site <- z_sites_manager:get_sites_all(), Site /= zotonic_status ]},
        {sites, SitesStatus}
    ],
    Vars1 = z_notifier:foldl(zotonic_status_init, Vars, Context),
    Context1 = z_render:update("sites", #render{template="_sites.tpl", vars=Vars1}, Context),
    z_session_page:add_script(Context1).
