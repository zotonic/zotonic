%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-21
%% @doc Open a dialog for sending a broadcast message.

%% Copyright 2009 Marc Worrell
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

-module(action_broadcast_dialog_broadcast).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(dialog_broadcast, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the broadcast form
%% @spec event(Event, Context1) -> Context2
event({postback, dialog_broadcast, _TriggerId, _TargetId}, Context) ->
	Vars = [
		{delegate, atom_to_list(?MODULE)}
	],
    z_render:dialog("Broadcast a message.", "_action_dialog_broadcast.tpl", Vars, Context);


%% @doc Send a broadcast to all users.
event({submit, broadcast, _TriggerId, _TargetId}, Context) ->
	case z_acl:has_role(admin, Context) of
		true ->
		    Title = z_context:get_q_validated("title", Context),
		    Message = z_context:get_q_validated("message", Context),
			z_notifier:notify1(#broadcast{title=Title, message=Message}, Context),
			z_render:dialog_close(Context);
		false ->
			z_render:growl_error("Only admins can broadcast messages.", Context)
	end.
