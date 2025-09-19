%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2025 Arjan Scherpenisse
%% @doc Functions for admin tasks in the /admin/status panel. Flushing caches, reindexing,
%% reloading configs and more.
%% Example usage for in a wire: ```action={admin_tasks task="flush"}'''
%% @end

-module(action_admin_admin_tasks).
-moduledoc("
Action module which provides postback handlers for the “status” view of the admin:

*   Rebuild search index
*   Flush cache
*   Renumber categories

Todo

Extend documentation
").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include_lib("zotonic_core/include/zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback = {admin_tasks, [{task, proplists:get_value(task, Args)}]},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

%% @doc Flush the caches of all sites.
event(#postback{message={admin_tasks, [{task, <<"flush_all_sites">>}]}}, Context) ->
    do(fun z:flush/0,
       ?__(<<"All sites have been flushed.">>, Context),
       Context);

%% @doc Flush the caches of the site referred to in the supplied context.
event(#postback{message={admin_tasks, [{task, <<"flush">>}]}}, Context) ->
    do(fun() ->
          zotonic_fileindexer:flush(),
          z:flush(Context)
       end,
       ?__(<<"Caches have been flushed.">>, Context),
       Context);

%% @doc Reset templates.
event(#postback{message={admin_tasks, [{task, <<"templates_reset">>}]}}, Context) ->
    do(fun() -> z_template:reset(Context) end,
       ?__(<<"Templates will be recompiled.">>, Context),
       Context);

%% @doc Pivot everything
event(#postback{message={admin_tasks, [{task, <<"pivot_all">>}]}}, Context) ->
    do(fun() -> z_pivot_rsc:queue_all(Context) end,
       ?__(<<"The search index is rebuilding. Depending on the database size, this can take a long time."/utf8>>, Context),
       Context);

%% @doc Reinstall site datamodel
event(#postback{message={admin_tasks, [{task, <<"site_reinstall">>}]}}, Context) ->
    do(fun() -> z_module_manager:reinstall(z_context:site(Context), Context) end,
       ?__(<<"The site’s data is being reinstalled. Watch the console for messages"/utf8>>, Context),
       Context);

%% @doc Reinstall zotonic datamodel
event(#postback{message={admin_tasks, [{task, <<"zotonic_reinstall">>}]}}, Context) ->
    do(fun() -> z_install:install_datamodel(Context) end,
       ?__(<<"The Zotonic datamodel is being reinstalled. Watch the console for messages"/utf8>>, Context),
       Context);

%% @doc Renumber the category tree
event(#postback{message={admin_tasks, [{task, <<"renumber_categories">>}]}}, Context) ->
    do(fun() -> m_category:renumber(Context) end,
       ?__(<<"The category tree is rebuilding. This can take a long time."/utf8>>, Context),
       Context);

%% @doc Reload the site configuration.
event(#postback{message={admin_tasks, [{task, <<"reload_config">>}]}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            case m_site:reload_config(Context) of
                ok ->
                    z_render:growl(?__(<<"The site configuration has been reloaded.">>, Context), Context);
                {error, _} ->
                    z_render:growl_error(?__(<<"Could not reload the site configuration, check the logs.">>, Context), Context)
            end;
        false ->
            z_render:growl_error(?__(<<"You don’t have permission to perform this action."/utf8>>, Context), Context)
    end.

do(Fun, OkMsg, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            Fun(),
            z_render:growl(OkMsg, Context);
        false ->
            z_render:growl_error(?__(<<"You don’t have permission to perform this action."/utf8>>, Context), Context)
    end.
