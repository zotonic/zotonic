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

-module(controller_zotonic_status).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    charsets_provided/1,
    content_types_provided/1,
	provide_content/1,
	event/2,
	updater/2
]).

-include_lib("include/zotonic.hrl").


charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    case z_context:get(content_type, Context) of
        undefined ->
            {[{<<"text/html">>, provide_content}], Context};
        Mime ->
            {[{z_convert:to_binary(Mime), provide_content}], Context}
    end.

provide_content(Context) ->
    Context2 = z_context:ensure_all(Context),
    case z_acl:user(Context2) of
        undefined ->
            logon_page(Context2);
        _ ->
            status_page(Context2)
    end.

logon_page(Context) ->
    Rendered = z_template:render("logon.tpl", z_context:get_all(Context), Context),
    {Output, OutputContext} = z_context:output(Rendered, Context),
    Context1 = cowmachine_req:set_response_code(503, OutputContext),
    Context2 = cowmachine_req:set_resp_body(Output, Context1),
    {{halt, 503}, Context2}.

status_page(Context) ->
    Template = z_context:get(template, Context),
    SitesStatus = get_sites_status(),
    Vars = [
        {has_user, z_acl:user(Context)},
        {configs, site_configs()},
        {sites, SitesStatus}
        | z_context:get_all(Context)
    ],
    Vars1 = z_notifier:foldl(zotonic_status_init, Vars, Context),
    Rendered = z_template:render(Template, Vars1, Context),
    {Output, OutputContext} = z_context:output(Rendered, Context),
    case Template of
        "home.tpl" -> start_stream(SitesStatus, OutputContext);
        _ -> ok
    end,
    {Output, OutputContext}.


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
    notice(Site, "Successfully started.", Context);
event(#postback{message={site_restart, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:restart(Site),
    notice(Site, "Successfully restarted.", Context);
event(#postback{message={site_stop, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z_sites_manager:stop(Site),
    notice(Site, "Successfully stopped.", Context);
event(#postback{message={site_flush, [{site, Site}]}}, Context) ->
    true = z_auth:is_auth(Context),
    z:flush(z_context:new(Site)),
    notice(Site, "The cache is flushed and all dispatch rules are reloaded.", Context);
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
    NewStatus = get_sites_status(),
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
        {configs, site_configs()},
        {sites, SitesStatus}
    ],
    Vars1 = z_notifier:foldl(zotonic_status_init, Vars, Context),
    Context1 = z_render:update("sites", #render{template="_sites.tpl", vars=Vars1}, Context),
    z_session_page:add_script(Context1).

notice(SiteName, Text, Context) ->
     mod_zotonic_status_vcs:notice(SiteName, Text, Context).


site_configs() ->
    [ {Site, site_config(Site)} || Site <- get_sites() ].

site_config(Site) ->
    case z_sites_manager:get_site_config(Site) of
        {ok, Config} -> Config;
        {error, _} = Error -> [{host,Site}, Error]
    end.

get_sites() ->
    lists:filter(fun(Site) ->
                    not lists:member(Site, z_sites_manager:get_builtin_sites())
                 end,
                 z_sites_manager:get_sites_all()).

get_sites_status() ->
    SitesStatus = z_sites_manager:get_sites_status(),
    lists:filter(fun(Status) ->
                    not lists:member(hd(Status), z_sites_manager:get_builtin_sites())
                 end,
                 SitesStatus).
