%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Marc Worrell
%% Date: 2010-04-11
%% @doc Flush system cache


-module(action_admin_admin_tasks).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include("zotonic.hrl").

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
event(#postback{message={admin_tasks, [{task, <<"flush">>}]}}, Context) ->
    do(fun z:flush/0, 
       ?__(<<"Caches have been flushed."/utf8>>, Context), 
       Context);

%% @doc Reset templates.
event(#postback{message={admin_tasks, [{task, <<"templates_reset">>}]}}, Context) ->
    do(fun() -> z_template:reset(Context) end, 
       ?__(<<"Templates will be recompiled."/utf8>>, Context), 
       Context);


%% @doc Pivot everything
event(#postback{message={admin_tasks, [{task, <<"pivot_all">>}]}}, Context) ->
    do(fun() -> z_pivot_rsc:queue_all(Context) end, 
       ?__(<<"The search index is rebuilding. Depending on the database size, this can take a long time."/utf8>>, Context), 
       Context);

%% @doc Renumber the category tree
event(#postback{message={admin_tasks, [{task, <<"site_reinstall">>}]}}, Context) ->
    do(fun() -> z_module_manager:reinstall(z_context:site(Context), Context) end,
       ?__(<<"The site’s data is being reinstalled. Watch the console for messages"/utf8>>, Context),
       Context);

%% @doc Renumber the category tree
event(#postback{message={admin_tasks, [{task, <<"renumber_categories">>}]}}, Context) ->
    do(fun() -> m_category:renumber(Context) end, 
       ?__(<<"The category tree is rebuilding. This can take a long time."/utf8>>, Context),
       Context).


do(Fun, OkMsg, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            Fun(),
            z_render:growl(OkMsg, Context);
        false ->
            z_render:growl_error(?__(<<"You don’t have permission to perform this action."/utf8>>, Context), Context)
    end.
