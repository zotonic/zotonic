%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Marc Worrell
%% Date: 2010-04-11
%% @doc Flush system cache


-module(action_admin_adminwidget_toggle).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
    Postback = {toggle},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @doc Flush the caches of all sites.
event(#postback{message={toggle}}, Context) ->
    State = z_context:get_q("showing", Context),
    Id = z_context:get_q("id", Context),
    New = z_utils:prop_replace(Id, State, z_session:get("admin_widgets", Context, [])),
    z_session:set("admin_widgets",
                  New,
                  Context),
    ?DEBUG(New),

    Context.
