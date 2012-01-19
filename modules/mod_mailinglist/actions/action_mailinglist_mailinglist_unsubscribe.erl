%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-29
%% @doc Cancel a mailing list subscription.

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

-module(action_mailinglist_mailinglist_unsubscribe).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
	event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    RecipientId = proplists:get_value(id, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    OnError = proplists:get_all_values(on_error, Args),
    Postback = {mailinglist_unsubscribe, RecipientId, OnSuccess, OnError},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event(#postback{message={mailinglist_unsubscribe, RecipientId, OnSuccess, OnError}}, Context) ->
	case m_mailinglist:recipient_delete(RecipientId, Context) of
		ok ->
			z_render:wire(OnSuccess, Context);
		{error, _Reason} ->
			z_render:wire(OnError, Context)
	end.
