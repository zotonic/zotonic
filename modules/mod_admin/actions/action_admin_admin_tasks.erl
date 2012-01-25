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
event(#postback{message={admin_tasks, [{task, "flush"}]}}, Context) ->
    do(fun z:flush/0, "Caches have been flushed.", Context);

%% @doc Reset templates.
event(#postback{message={admin_tasks, [{task, "templates_reset"}]}}, Context) ->
    do(fun() -> z_template:reset(Context) end, "Templates will be recompiled.", Context);


%% @doc Pivot everything
event(#postback{message={admin_tasks, [{task, "pivot_all"}]}}, Context) ->
    do(fun() -> z_pivot_rsc:queue_all(Context) end, 
       "The search index is rebuilding. Depending on the database size, this can take a long time.", Context);

%% @doc Renumber the category tree
event(#postback{message={admin_tasks, [{task, "renumber_categories"}]}}, Context) ->
    do(fun() -> m_category:renumber(Context) end, 
       "The category tree is rebuilding. This can take a long time.", Context).


do(Fun, OkMsg, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            Fun(),
            z_render:growl(OkMsg, Context);
        false ->
            z_render:growl_error("You don't have permission to perform this action.", Context)
    end.
