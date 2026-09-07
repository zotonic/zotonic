%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Confirm a mailing list subscription.
%% @end

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

-module(action_mailinglist_mailinglist_confirm).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "mailing_lists", "edit"]
}).
-moduledoc("
Confirm a mailinglist subscription. Required argument is the `confirm_key`.

Other arguments:

*   `on_success` - actions which get executed when the subscription is confirmed.
*   `on_error` - actions which get executed when the subscription fails (e.g. wrong confirm key).

Both action arguments can be repeated. The signed postback passes `confirm_key`
to `m_mailinglist:recipient_confirm/2` and runs exactly one of the two action
lists.

```django
{% wire action={mailinglist_confirm confirm_key=q.key on_success={show target=\"confirmed\"} on_error={show target=\"invalid\"}} %}
```
").
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
	event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    ConfirmKey = proplists:get_value(confirm_key, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    OnError = proplists:get_all_values(on_error, Args),
    Postback = {mailinglist_confirm, ConfirmKey, OnSuccess, OnError},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event(#postback{message={mailinglist_confirm, ConfirmKey, OnSuccess, OnError}}, Context) ->
	case m_mailinglist:recipient_confirm(ConfirmKey, Context) of
		{ok, _RecipientId} ->
			z_render:wire(OnSuccess, Context);
		{error, _Reason} ->
			z_render:wire(OnError, Context)
	end.
