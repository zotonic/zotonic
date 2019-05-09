%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-09-13
%% @doc Mercurial support for zotonic sites

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

-module(mod_zotonic_status_vcs).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Mercurial support").
-mod_description("Allows to update and rebuild sites and zotonic installs from git or mercurial.").
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
event(#postback{ message = {vcs_up, Args} }, Context) ->
    true = z_acl:is_admin(Context),
    case proplists:get_value(zotonic, Args) of
        true ->
            case m_zotonic_status_vcs:vcs_zotonic() of
                {_, _} = VCS ->
                    async_notice('Zotonic', <<"Fetching updates…"/utf8>>, Context),
                    render_notice('Zotonic', update_vcs(VCS), Context);
                false ->
                    render_notice('Zotonic', <<"Zotonic hasn’t been checked out using version control."/utf8>>, Context)
            end;
        undefined ->
            Site = proplists:get_value(site, Args, z_context:site(Context)),
            case m_zotonic_status_vcs:vcs_site(Site) of
                {_, _} = VCS ->
                    async_notice(Site, <<"Fetching updates…"/utf8>>, Context),
                    render_notice(Site, update_vcs(VCS), Context);
                false ->
                    render_notice(Site, <<"Unknown site or no version control folder present.">>, Context)
            end
    end;
event(#postback{ message = make }, Context) ->
    true = z_acl:is_admin(Context),
    spawn(fun() ->
            z:m(),
            async_notice('Zotonic', <<"Finished rebuilding Zotonic.">>, Context)
          end),
    render_notice('Zotonic', <<"Building Zotonic in the background…"/utf8>>, Context).

%% @doc Run the VCS update command for a directory.
update_vcs({hg, Path}) ->
    Command = lists:flatten(["(cd ", z_utils:os_filename(Path), "; hg pull -u)"]),
    os:cmd(Command);
update_vcs({git, Path}) ->
    Command = lists:flatten(["(cd ", z_utils:os_filename(Path), "; git pull)"]),
    os:cmd(Command).


% @doc Wire a notice to the current context
render_notice(Site, Notice, Context) ->
    z_render:wire( notice(Site, Notice), Context ).

% @doc Send a notice to the current webpage.
-spec async_notice(atom(), string(), z:context()) -> ok.
async_notice(Sitename, Text, Context) ->
    z_notifier:notify(
        #page_actions{ actions = notice(Sitename, Text) },
        Context).

% @doc Actions to show a notice.
-spec notice(atom(), string()) -> list().
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

