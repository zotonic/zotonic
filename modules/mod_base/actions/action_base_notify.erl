%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman Worrell
%%
%% @doc Send a notification with an action. Modules can pick up the message and make things happen.

%% Copyright 2012 Maas-Maarten Zeeman
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

-module(action_base_notify).

-include("zotonic.hrl").

-export([render_action/4, event/2]).

render_action(TriggerId, TargetId, Args, Context) ->
    Message = proplists:get_value(message, Args),
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({notify, Message}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

event(#postback{message={notify, Message}}, Context) ->
    z_notifier:notify(Message, Context),
    Context.
