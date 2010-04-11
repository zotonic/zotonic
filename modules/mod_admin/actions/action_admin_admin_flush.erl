%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Marc Worrell
%% @date 2010-04-11
%% @doc Flush system cache


-module(action_admin_admin_flush).
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
    case z_acl:has_role(admin, Context) of
        true ->
            z:flush(),
            z_render:growl("Caches have been flushed.", Context);
        false ->
            z_render:growl_error("Sorry, you have no permission to flush.", Context)
    end.
 
