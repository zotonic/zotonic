%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2020 Marc Worrell
%% @doc Git and Mercurial support for zotonic sites

%% Copyright 2010-2020 Marc Worrell
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

-module(mod_site_update).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Site update").
-mod_description("Update the site to a new version from Git or other version control systems.").
-mod_prio(500).

-export([
    event/2
]).

-export([
    async_notice/3,
    render_notice/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").


-spec event(#postback{}, z:context()) -> z:context().
event(#postback{ message = vcs_up }, Context) ->
    Site = z_context:site(Context),
    case m_site_update:vcs_site(Site) of
        {_, _} = VCS ->
            z_notifier:notify(
                #page_actions{
                    actions = [
                        {growl, [
                            {text, ?__("Fetching updates from the version control...", Context)}
                        ]}
                    ]}, Context),
            case m_site_update:update_vcs(VCS, Context) of
                {ok, _Result} ->
                    z_render:growl(?__("Ready with fetch from version control.", Context), Context);
                {error, _} ->
                    z_render:growl(?__("Error during fetch from version control.", Context), Context)
            end;
        false ->
            z_render:growl(?__("Sorry, this site is not from a known version control system", Context), Context)
    end;
event(#postback{ message = {vcs_up, Args} }, Context) ->
    case z_context:site(Context) of
        zotonic_site_status ->
            true = z_acl:is_admin(Context),
            case proplists:get_value(zotonic, Args) of
                true ->
                    case m_site_update:vcs_zotonic() of
                        {_, _} = VCS ->
                            async_notice('Zotonic', ?__("Fetching updates...", Context), Context),
                            case m_site_update:update_vcs(VCS, Context) of
                                {ok, Result} ->
                                    render_notice('Zotonic', Result, Context);
                                {error, _} ->
                                    render_notice('Zotonic', ?__("Error running Zotonic update", Context), Context)
                            end;
                        false ->
                            render_notice('Zotonic', ?__("Zotonic has not been checked out using version control.", Context), Context)
                    end;
                undefined ->
                    Site = proplists:get_value(site, Args, z_context:site(Context)),
                    case m_site_update:vcs_site(Site) of
                        {_, _} = VCS ->
                            async_notice(Site, ?__("Fetching updates...", Context), Context),
                            case m_site_update:update_vcs(VCS, Context) of
                                {ok, Result} ->
                                    render_notice(Site, Result, Context);
                                {error, _} ->
                                    render_notice(Site, ?__("Error running site update", Context), Context)
                            end;
                        false ->
                            render_notice(Site, ?__("Unknown site or no version control folder present.", Context), Context)
                    end
            end;
        _ ->
            z_render:growl_error(?__("Sorry this can only be run from the Zotonic status site", Context), Context)
    end;
event(#postback{ message = make }, Context) ->
    case z_context:site(Context) of
        zotonic_site_status ->
            true = z_acl:is_admin(Context),
            z_proc:spawn_md(fun() ->
                    z:m(),
                    async_notice('Zotonic', ?__("Finished rebuilding Zotonic.", Context), Context)
                  end),
            render_notice('Zotonic', ?__("Building Zotonic in the background...", Context), Context);
        _ ->
            z_render:growl_error(?__("Sorry this can only be run from the Zotonic status site", Context), Context)
    end.


% @doc Wire a notice to the current context
render_notice(Site, Notice, #context{} = Context) ->
    z_render:wire( notice(Site, Notice), Context ).

% @doc Send a notice to the current webpage.
-spec async_notice(atom(), iodata(), z:context()) -> ok | {error, term()}.
async_notice(Sitename, Text, Context) ->
    z_notifier:notify(
        #page_actions{ actions = notice(Sitename, Text) },
        Context).

% @doc Actions to show a notice.
-spec notice(atom(), iodata()) -> list().
notice(SiteName, Text) ->
    [
        {insert_top, [
            {target, "notices"},
            {template, "_notice.tpl"},
            {site, SiteName},
            {notice, Text}
        ]},
        {fade_out, [
            {selector, "#notices > div:gt(0)"},
            {speed, 2000}
        ]}
    ].


% %% @doc Check which sites have a .hgrc/.git directory. Add this info to the template vars.
% observe_zotonic_status_init(zotonic_status_init, Vars, _Context) ->
%     case proplists:get_value(has_user, Vars) of
%         undefined ->
%             Vars;
%         N when is_integer(N) ->
%             Sites = [ SiteName || {SiteName,_Props} <- proplists:get_value(configs, Vars) ],
%             Vcs = [ {SiteName, has_vcs(SiteName)} || SiteName <- Sites ],
%             [{vcs, Vcs}, {vcs_zotonic, has_vcs()} | Vars]
%     end.

