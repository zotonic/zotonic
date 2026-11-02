%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2025 Marc Worrell
%% @doc Git and Mercurial support for zotonic sites
%% @end

%% Copyright 2010-2025 Marc Worrell
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
-moduledoc(#{
    zotonic_keywords => ["reference", "operator", "module", "site_management", "database", "migrate"]
}).
-moduledoc("
This module pulls updates of a site‘s code from a remote version control system. The supported version control systems
are git and mercurial.

After enabling this module you will see a **Deployment** page in the System menu in the admin.

This button is only available to users with the *use.mod_site_update* right. It opens a
preview of the current commit and incoming remote history. Fetching this preview does
not change checked-out files; deployment starts only after pressing **Deploy**.

The update uses the same deployment barrier as `bin/zotonic deploy`: restore the tracked Git lock file,
pull code, compile and activate the updated modules before allowing schema upgrades. File changes are
buffered until activation completes, then caches and template indexes are refreshed.

This update of the system can take a while.



Webhook
-------

In GitHub and other systems it is possible to set a *webhook* that is called when new updates are pushed to the repository.

In System > Modules there is a *Config* button next to the mod_site_update module. Click this and configure a secure
token to be passed to the webhook.

The URL for GitHub et al is:


```erlang
https://yoursite.test/api/model/site_update/post/webhook/<token>
```

Where `<token>` should be replaced with your configured token.

The token is saved in the config key `mod_site_update.webhook_token`.

Ensure that the external system performs a HTTP POST to the webhook, the posted body is dropped.



Zotonic status site
-------------------

The `mod_site_update` module is also used by the zotonic status site.

Here the module adds buttons to update sites and a button to update the complete Zotonic system.

Site updates get their datamodel fixed by automatic restarts of modules, ensure that you increment the
`-mod_schema(...)` version number.

Zotonic updates are a bit more complicated. Small updates will be fine. Large updates that also change the datamodel or
add new Erlang dependencies need more compilation and a system restart. So be careful with using this update button.

Accepted Events
---------------


Delegate callbacks:

- `event/2` with `postback` messages: `make`, `vcs_up` (open dialog), `vcs_preview` (fetch preview), `vcs_deploy` (confirmation).

").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Site update").
-mod_description("Update the site to a new version from Git or other version control systems.").
-mod_prio(500).
-mod_config([
        #{
            key => webhook_token,
            type => string,
            default => "",
            description => "The token to use for the webhook used to fetch new code from the version control system. "
                           "If empty the webhook is disabled. This must be kept secret."
        }
    ]).

-export([
    event/2,
    observe_admin_menu/3
]).

-export([
    async_notice/3,
    render_notice/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_wires/include/mod_wires.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


%% @doc Add the deployment page only when the site update module is enabled.
-spec observe_admin_menu(#admin_menu{}, list(), z:context()) -> list().
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [#menu_item{
        id = admin_deployment,
        sort = 400,
        parent = admin_system,
        label = ?__("Deployment", Context),
        url = admin_deployment,
        visiblecheck = {acl, use, mod_site_update}
    } | Acc].

-spec event(#postback{}, z:context()) -> z:context().
event(#postback{message = vcs_up}, Context) ->
    preview(current, Context);
event(#postback{message = {vcs_up, Args}}, Context) ->
    Target = case {proplists:get_value(zotonic, Args), proplists:get_value(module, Args)} of
        {true, _} -> zotonic;
        {_, undefined} -> {site, proplists:get_value(site, Args)};
        {_, Module} -> {module, Module}
    end,
    preview(Target, Context);
event(#postback{message = {vcs_preview, Args}}, Context) ->
    fetch_preview(proplists:get_value(target, Args), proplists:get_value(element_id, Args), Context);
event(#postback{message = {vcs_deploy, Args}}, Context) ->
    deploy(proplists:get_value(target, Args), Context);
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


%% Resolve repositories on the server, and recheck permissions on confirmation.
resolve_target(current, Context) ->
    case z_acl:is_allowed(use, mod_site_update, Context) of
        true ->
            Site = z_context:site(Context),
            {ok, Site, m_site_update:vcs_site(Site)};
        false -> {error, eacces}
    end;
resolve_target({module, Module}, Context) when is_atom(Module); is_binary(Module) ->
    %% A module repository can affect all sites on the node.
    case z_acl:is_admin(Context) of
        true -> {ok, Module, m_site_update:vcs_module(Module)};
        false -> {error, eacces}
    end;
resolve_target(Target, #context{site = zotonic_site_status} = Context) ->
    case z_acl:is_admin(Context) of
        true -> status_target(Target);
        false -> {error, eacces}
    end;
resolve_target(_, _Context) ->
    {error, eacces}.

status_target(zotonic) ->
    {ok, 'Zotonic', m_site_update:vcs_zotonic()};
status_target({site, Site}) when is_atom(Site), Site =/= undefined ->
    {ok, Site, m_site_update:vcs_site(Site)};
status_target(_) ->
    {error, eacces}.

%% Show the dialog first. Its wire fetches the preview in a separate postback.
preview(Target, Context) ->
    case resolve_target(Target, Context) of
        {ok, Name, _VCS} ->
            z_render:dialog(?__("Deploy latest version", Context),
                "_dialog_site_deploy_loading.tpl",
                [{target, Target}, {name, Name}], Context);
        {error, eacces} ->
            z_render:growl_error(?__("You are not allowed to deploy this code.", Context), Context)
    end.

fetch_preview(Target, ElementId, Context) ->
    {Name, Preview} = case resolve_target(Target, Context) of
        {ok, ResolvedName, VCS} ->
            Info = case m_site_update:preview_vcs(VCS) of
                {ok, Result} -> Result;
                {error, Reason} -> #{error => Reason}
            end,
            {ResolvedName, Info};
        {error, eacces} ->
            {undefined, #{error => eacces}}
    end,
    %% A unique container prevents a late fetch from reopening a cancelled dialog
    %% or replacing a newer preview.
    z_render:update(ElementId, #render{
        template = "_dialog_site_deploy.tpl",
        vars = [{target, Target}, {name, Name}, {preview, Preview},
                {is_module, is_module_target(Target)}]
    }, Context).

is_module_target({module, _}) -> true;
is_module_target(_) -> false.

deploy(Target, Context) ->
    case resolve_target(Target, Context) of
        {ok, Name, VCS} ->
            z_notifier:notify(#page_actions{actions = [
                {dialog_close, []},
                {growl, [{text, ?__("Deploying the latest version...", Context)}]}
            ]}, Context),
            Result = m_site_update:update_vcs(VCS, Context),
            deployment_notice(Name, Result, Context);
        {error, eacces} ->
            z_render:growl_error(?__("You are not allowed to deploy this code.", Context), Context)
    end.

deployment_notice(Name, {ok, Output}, #context{site = zotonic_site_status} = Context) ->
    render_notice(Name, Output, Context);
deployment_notice(_Name, {ok, _}, Context) ->
    z_render:growl(?__("Deployment completed.", Context), Context);
deployment_notice(_Name, {error, update_in_progress}, Context) ->
    z_render:growl_error(?__("Another deployment is in progress. Please try again when it finishes.", Context), Context);
deployment_notice(_Name, {error, _}, Context) ->
    z_render:growl_error(?__("Deployment failed. Check the server log for details.", Context), Context).


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
