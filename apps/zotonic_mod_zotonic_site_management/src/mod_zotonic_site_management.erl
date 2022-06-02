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

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").

-export([event/2,
         progress/3]).

event(#submit{
          message = addsite,
          form = Form
      },
      Context) ->
    true = z_auth:is_auth(Context),
    Sitename = z_context:get_q_validated(<<"sitename">>, Context),
    Options =
        [{hostname, z_context:get_q_validated(<<"hostname">>, Context)},
         {skeleton, z_context:get_q_validated(<<"skel">>, Context)},
         {dbdatabase, z_context:get_q_validated(<<"dbdatabase">>, Context)},
         {dbschema,
          case z_context:get_q_validated(<<"dbschema">>, Context) of
              <<>> ->
                  Sitename;
              Schema ->
                  Schema
          end},
         {dbhost, z_context:get_q_validated(<<"dbhost">>, Context)},
         {dbport, z_context:get_q_validated(<<"dbport">>, Context)},
         {dbuser, z_context:get_q(<<"dbuser">>, Context)},
         {dbpassword, z_context:get_q(<<"dbpassword">>, Context)}],
    ?LOG_NOTICE("[zotonic_site_status] Creating site ~s with ~p", [Sitename, Options]),
    case zotonic_status_addsite:addsite(Sitename, Options, Context) of
        {ok, {Site, FinalOptions}} ->
            progress(Sitename, ?__("Starting the new site ...", Context), Context),
            ok = z_sites_manager:upgrade(),
            _ = z_sites_manager:start(Site),
            ?LOG_NOTICE("[zotonic_site_status] Success creating site ~s", [Site]),
            case await(Site) of
                ok ->
                    ?LOG_NOTICE("[zotonic_site_status] Site ~s is running", [Site]),
                    SiteContext = z_context:new(Site),
                    z_module_manager:upgrade_await(SiteContext),
                    Vars =
                        [{admin_url, abs_url_for(admin, SiteContext)},
                         {site_url, z_context:abs_url(<<"/">>, SiteContext)},
                         {site_dir, z_path:site_dir(SiteContext)}
                         | FinalOptions],
                    Context1 = notice(Form, Site, ?__("Succesfully created the site.", Context), Context),
                    z_render:replace(Form,
                                     #render{
                                         vars = Vars,
                                         template = "_addsite_success.tpl"
                                     },
                                     Context1);
                {error, StartError} ->
                    ?LOG_ERROR("[zotonic_site_status] Newly created site ~s is NOT running (~p)", [Site, StartError]),
                    notice(Form,
                           Site,
                           ?__("Something is wrong, site is not starting. Please check the logs.", Context),
                           Context)
            end;
        {error, Msg} when is_list(Msg); is_binary(Msg) ->
            notice(Form, Sitename, Msg, Context);
        {error, Msg} ->
            notice(Form, Sitename, io_lib:format("~p", [Msg]), Context)
    end.

%% @doc Wait till the site is up and running. Timeout after 100 seconds.
await(Site) ->
    await(Site, 0).

await(_Site, Tries) when Tries > 100 ->
    {error, timeout};
await(Site, Tries) ->
    timer:sleep(1000),
    case z_sites_manager:get_site_status(Site) of
        {ok, running} ->
            ok;
        {ok, starting} ->
            timer:sleep(1000),
            await(Site, Tries + 1);
        {ok, new} ->
            timer:sleep(1000),
            await(Site, Tries + 1);
        {ok, retrying} ->
            timer:sleep(1000),
            await(Site, Tries + 1);
        {ok, Other} ->
            {error, Other};
        {error, _} = Error ->
            Error
    end.

abs_url_for(Dispatch, Context) ->
    case z_dispatcher:url_for(Dispatch, Context) of
        undefined ->
            undefined;
        Url ->
            z_context:abs_url(Url, Context)
    end.

% @doc Render a notice.
notice(Form, Sitename, Text, Context) ->
    Actions = notice_actions(Sitename, Text) ++ [{unmask, [{target, Form}]}],
    z_render:wire(Actions, Context).

progress(Sitename, Text, Context) ->
    case erlang:get(is_zotonic_command) of
        true ->
            io:format("~s~n", [Text]);
        _ ->
            z_notifier:notify(#page_actions{ actions = notice_actions(Sitename, Text) }, Context)
    end.

notice_actions(Sitename, Text) ->
    [{insert_top, [{target, "notices"}, {template, "_notice.tpl"}, {site, Sitename}, {notice, Text}]},
     {fade_out, [{selector, "#notices > div:gt(0)"}, {speed, 2000}]}].
