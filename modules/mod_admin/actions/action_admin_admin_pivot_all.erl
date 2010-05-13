%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-04-11
%% @doc Rebuild the search index

-module(action_admin_admin_pivot_all).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
    Postback = {admin_flush},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {admin_flush}, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            z_pivot_rsc:queue_all(Context),
            z_render:growl("The search index is rebuilding. Depending on the database size, this can take a long time.", Context);
        false ->
            z_render:growl_error("Sorry, you have no permission to rebuild the search index.", Context)
    end.
 
