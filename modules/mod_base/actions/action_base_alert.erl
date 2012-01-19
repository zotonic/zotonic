%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code copyright (c) 2008-2009 Rusty Klophaus

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

-module(action_base_alert).
-include("zotonic.hrl").
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) -> 
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({alert, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the resource
%% @spec event(Event, Context1) -> Context2
event(#postback{message={alert, Args}}, Context) ->
    Text = proplists:get_value(text, Args, ""),
    Title = proplists:get_value(title, Args, ?__(<<"Alert">>, Context)),
    Button = proplists:get_value(button, Args),
    Action = proplists:get_all_values(action, Args),
    Vars = [
        {title, Title},
        {text, Text},
        {button, Button},
        {action, Action}
    ],
    z_render:dialog(Title, "_action_dialog_alert.tpl", Vars, Context).

