%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Resource to serve the zotonic fallback site templates.

%% Copyright 2010-2017 Marc Worrell
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

-module(controller_zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    content_types_provided/1,
	process/4,
	event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").


content_types_provided(Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[ {<<"text">>, <<"html">>, []} ], Context};
        Mime when is_list(Mime) ->
            {[ z_convert:to_binary(Mime) ], Context};
        Mime ->
            {[ Mime ], Context}
    end.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    case z_context:get(is_fallback_template, Context) of
        true ->
            Context2 = z_context:ensure_qs(Context),
            Template = z_context:get(template, Context2),
            render_page(Template, Context2);
        _ ->
            Context2 = z_context:ensure_qs(Context),
            case z_acl:user(Context2) of
                undefined ->
                    render_page("logon.tpl", Context2);
                _ ->
                    status_page(Context2)
            end
    end.

render_page(Template, Context) ->
    StatusCode = resp_code(Context),
    Vars = [
        {error_code, StatusCode}
        | z_context:get_all(Context)
    ],
    Rendered = z_template:render(Template, Vars, Context),
    {Output, OutputContext} = z_context:output(Rendered, Context),
    Context1 = cowmachine_req:set_response_code(StatusCode, OutputContext),
    Context2 = cowmachine_req:set_resp_body(Output, Context1),
    {{halt, StatusCode}, Context2}.

resp_code(Context) ->
    case z_context:get(is_fallback_template, Context) of
        true ->
            case z_context:get_q(http_status_code, Context) of
                StatusCode when is_integer(StatusCode) ->
                    StatusCode;
                _ ->
                    case cowmachine_req:get_metadata(http_status_code, Context) of
                        undefined -> 200;
                        StatusCode -> StatusCode
                    end
            end;
        undefined ->
            200
    end.

status_page(Context) ->
    Template = z_context:get(template, Context),
    SitesStatus = m_zotonic_status:get_sites_status(),
    Vars = [
        {has_user, z_acl:user(Context)},
        {configs, m_zotonic_status:get_sites_config()},
        {sites, SitesStatus}
        | z_context:get_all(Context)
    ],
    Rendered = z_template:render(Template, Vars, Context),
    z_context:output(Rendered, Context).


%% -----------------------------------------------------------------------------------------------
%% Handle all events
%% -----------------------------------------------------------------------------------------------

event(#submit{message= <<>>, form=FormId}, Context) ->
    case z_context:get_q(<<"password">>, Context) =:= z_convert:to_binary(z_config:get(password)) of
        true ->
            {ok, ContextAuth} = z_auth:logon(1, Context),
            z_render:wire({reload, []}, ContextAuth);
        false ->
            z_render:wire([
                        {add_class, [{target,FormId}, {class,"error-pw"}]},
                        {set_value, [{target,"password"},{value, ""}]}], Context)
    end;
event(#postback{message={logoff, []}}, Context) ->
    z_render:wire({reload, []}, z_auth:logoff(Context));
event(#postback{message={site_start, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:start(Site),
    render_notice(Site, "Successfully started.", Context);
event(#postback{message={site_restart, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:restart(Site),
    render_notice(Site, "Successfully restarted.", Context);
event(#postback{message={site_stop, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:stop(Site),
    render_notice(Site, "Successfully stopped.", Context);
event(#postback{message={site_flush, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z:flush(z_context:new(Site)),
    render_notice(Site, "The cache is flushed and all dispatch rules are reloaded.", Context);
event(#postback{message={site_admin, [{site,Site}]}}, Context) ->
    try
        SiteContext = z_context:new(Site),
        case z_dispatcher:url_for(admin, SiteContext) of
            undefined ->
                z_render:growl_error("This site does not have an admin url.", Context);
            U ->
                Url = z_dispatcher:abs_url(U, SiteContext),
                z_render:wire({redirect, [{location,Url}]}, Context)
        end
    catch
        _:_ -> z_render:growl_error("Could not fetch the admin url, the site might not be running.", Context)
    end.


% %% -----------------------------------------------------------------------------------------------
% %% Stream process to update the page when data changes
% %% -----------------------------------------------------------------------------------------------

% start_stream(SitesStatus, Context) ->
%     z_session_page:spawn_link(?MODULE, updater, [SitesStatus, Context], Context).

% % @todo Instead of polling we should observe the system wide notifications (that will be implemented)
% -spec updater(any(), z:context()) -> z:context().
% updater(SitesStatus, Context) ->
%     Context1 = z_auth:logon_from_session(Context),
%     timer:sleep(1000),
%     z_sites_manager:upgrade(),
%     NewStatus = m_zotonic_status:get_sites_status(),
%     case NewStatus /= SitesStatus of
%         true ->
%             Context2 = render_update(NewStatus, Context1),
%             ?MODULE:updater(NewStatus, Context2);
%         false ->
%             ?MODULE:updater(SitesStatus, Context1)
%     end.

% -spec render_update(list(), z:context()) -> z:context().
% render_update(SitesStatus, Context) ->
%     Vars = [
%         {has_user, z_acl:user(Context)},
%         {configs, m_zotonic_status:get_sites_config()},
%         {sites, SitesStatus}
%     ],
%     Actions = {update, [ {target, "sites"}, {template, "_sites.tpl"} ] ++ Vars },
%     z_notifier:notify( #page_actions{ actions = Actions }, Context ).


render_notice(SiteName, Text, Context) ->
     mod_zotonic_status_vcs:render_notice(SiteName, Text, Context).
