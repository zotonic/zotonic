%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Site management module

%% Copyright 2016 Marc Worrell
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

-module(mod_zotonic_site_management).

-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Site Management").
-mod_description("Manage Zotonic sites.").
-mod_prio(500).

-include("zotonic.hrl").

-export([
    event/2,

    progress/3
    ]).

event(#submit{message=addsite, form=Form}, Context) ->
    true = z_auth:is_auth(Context),
    Sitename = z_context:get_q_validated(<<"sitename">>, Context),
    Options = [
        {hostname, z_context:get_q_validated(<<"hostname">>, Context)},
        {skeleton, z_context:get_q_validated(<<"skel">>, Context)},
        {dbdatabase, z_context:get_q_validated(<<"dbdatabase">>, Context)},
        {dbschema, case z_context:get_q_validated(<<"dbschema">>, Context) of
                        <<>> -> Sitename;
                        Schema -> Schema
                   end},
        {dbhost, z_context:get_q_validated(<<"dbhost">>, Context)},
        {dbport, z_context:get_q_validated(<<"dbport">>, Context)},
        {dbuser, z_context:get_q(<<"dbuser">>, Context)},
        {dbpassword, z_context:get_q(<<"dbpassword">>, Context)}
    ],
    lager:info("[zotonic_status] Creating site ~s with ~p", [Sitename, Options]),
    case zotonic_status_addsite:addsite(Sitename, Options, Context) of
        {ok, {Site, FinalOptions}} ->
            progress(Sitename, ?__("Starting the new site ...", Context), Context),
            ok = z_sites_manager:upgrade(),
            ok = z_sites_manager:start(Site),
            ok = z_sites_manager:await_startup(Site),
            lager:info("[zotonic_status] Success creating site ~s", [Site]),
            case await(Site) of
                ok ->
                    lager:info("[zotonic_status] Site ~s is running", [Site]),
                    timer:sleep(2000),
                    SiteContext = z_context:new(Site),
                    Vars = [
                        {admin_url, abs_url_for(admin, SiteContext)},
                        {site_url, z_context:abs_url(<<"/">>, SiteContext)},
                        {site_dir, z_path:site_dir(SiteContext)}
                        | FinalOptions
                    ],
                    Context1 = notice(Form, Site, ?__("Succesfully created the site.", Context), Context),
                    z_render:replace(Form, #render{vars=Vars, template="_addsite_success.tpl"}, Context1);
                error ->
                    lager:error("[zotonic_status] Newly created site ~s is NOT running", [Site]),
                    notice(Form, Site, ?__("Something is wrong, site is not starting. Please check the logs.", Context), Context)
            end;
        {error, Msg} when is_list(Msg); is_binary(Msg) ->
            notice(Form, Sitename, Msg, Context);
        {error, Msg} ->
            notice(Form, Sitename, io_lib:format("~p", [Msg]), Context)
    end.

%% @todo we need a better way to know if a site is up and running
await(Site) ->
    timer:sleep(1000),
    await(Site, 0).

await(_Site, Tries) when Tries > 100 ->
    error;
await(Site, Tries) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} -> ok;
        _Other ->
            timer:sleep(100),
            await(Site, Tries+1)
    end.

abs_url_for(Dispatch, Context) ->
    case z_dispatcher:url_for(Dispatch, Context) of
        undefined -> undefined;
        Url -> z_context:abs_url(Url, Context)
    end.

% @doc Show a notice on the current webpage.
% show_notice(SiteName, Text, Context) ->
%     z_session_page:add_script(notice(SiteName, Text, Context)).

% @doc Render a notice.
notice(Form, Sitename, Text, Context) ->
    Context1 = render_notice(Sitename, Text, Context),
    z_render:wire({unmask, [{target, Form}]}, Context1).

progress(Sitename, Text, Context) ->
    z_session_page:add_script(render_notice(Sitename, Text, Context)).

render_notice(Sitename, Text, Context) ->
    Context1 = z_render:appear_top(
                        "notices",
                        #render{template="_notice.tpl", vars=[{site,Sitename},{notice,Text}]},
                        Context),
    z_render:wire({fade_out, [{selector, "#notices > div:gt(0)"}, {speed, 2000}]}, Context1).

