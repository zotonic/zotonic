%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-09-07
%% @doc Stream template updates to the user agent


-module(action_development_development_templates_stream).
-author("Marc Worrell <marc@worrell.nl>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
    Postback = {development_templates_stream, [{target, TargetId}]},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, none, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @doc Flush the caches of all sites.
event({postback, {development_templates_stream, [{target, Target}]}, _TriggerId, _TargetId}, Context) ->
    z_notifier:notify1(#debug_stream{target=Target, what=template}, Context),
    Context.
