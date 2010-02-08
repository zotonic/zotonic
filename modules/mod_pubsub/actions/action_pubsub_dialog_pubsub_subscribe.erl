%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Publish/subscribe admin interface: subscribe dialog

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

-module(action_pubsub_dialog_pubsub_subscribe).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, _Args, Context) ->
    Postback = {pubsub_subscribe_dialog},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {pubsub_subscribe_dialog}, _TriggerId, _TargetId}, Context) ->
    z_render:dialog("Subscribe to URL", "_action_dialog_pubsub_subscribe.tpl", [], Context);


event({submit, subscribe_new, _TriggerId, _TargetId}, Context) ->
    Url    = z_context:get_q("subscribe_url", Context),

    {ok, _Id, subscribed} = mod_pubsub:subscribe_to_url(Url, Context),

    timer:sleep(1000), %% give notification some time to arrive, so we can show an updated list.

    % Close the dialog
    Context1 = z_render:growl("Subscription successful!", Context),
    Context2 = z_render:wire({dialog_close, []}, Context1),

    {Html, Context3} = z_template:render_to_iolist("_admin_pubsub_subscriptions_list.tpl", [], Context2),
    z_render:update("pubsub-subscriptions", Html, Context3).
