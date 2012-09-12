%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Resource to serve the zotonic fallback site templates.

%% Copyright 2010 Marc Worrell
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

-module(resource_zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([
	provide_content/2,
	event/2,
	updater/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").


init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[{"text/html", provide_content}], ReqData, Context};
        Mime -> 
            {[{Mime, provide_content}], ReqData, Context}
    end.

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),

    Template = case z_acl:user(Context2) of
                   undefined -> "logon.tpl";
                   _ -> z_context:get(template, Context2)
               end,
    SitesStatus = z_sites_manager:get_sites_status(),
    Vars = [
        {has_user, z_acl:user(Context2)},
        {configs, [ {Site, z_sites_manager:get_site_config(Site)} || Site <- z_sites_manager:get_sites_all(), Site /= zotonic_status ]},
        {sites, SitesStatus}
        | z_context:get_all(Context2)
    ],
    Vars1 = z_notifier:foldl(zotonic_status_init, Vars, Context),
    Rendered = z_template:render(Template, Vars1, Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    start_stream(SitesStatus, OutputContext),
    ?WM_REPLY(Output, OutputContext).


%% -----------------------------------------------------------------------------------------------
%% Handle all events
%% -----------------------------------------------------------------------------------------------

event(#submit{message=[], form=FormId}, Context) ->
    case z_context:get_q(password, Context) == z_config:get(password) of
        true ->
            {ok, ContextAuth} = z_auth:logon(1, Context),
            z_render:wire({reload, []}, ContextAuth);
        false ->
            z_render:wire([
                        {set_class, [{target,FormId},{class,"error-pw form-inline"}]}, 
                        {set_value, [{target,"password"},{value, ""}]}], Context)
    end;
event(#postback{message={logoff, []}}, Context) ->
    z_render:wire({reload, []}, z_auth:logoff(Context));
event(#postback{message={site_start, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:start(Site),
    Context;
event(#postback{message={site_restart, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:restart(Site),
    Context;
event(#postback{message={site_stop, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:stop(Site),
    Context;
event(#postback{message={site_flush, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z:flush(z_context:new(Site)),
    Context.


%% -----------------------------------------------------------------------------------------------
%% Stream process to update the page when data changes
%% -----------------------------------------------------------------------------------------------

start_stream(SitesStatus, Context) ->
    z_session_page:spawn_link(?MODULE, updater, [SitesStatus, Context], Context).


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
    

