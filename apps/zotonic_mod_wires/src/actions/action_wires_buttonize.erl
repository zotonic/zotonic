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

-module(action_wires_buttonize).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "user_interface_and_interaction", "javascript"]
}).
-moduledoc("
Add button interaction classes to an element.

The action adds `hover` during pointer hover and `clicked` while the pointer is
pressed, removing each class when its corresponding interaction ends.

For example:

```django
{% wire id=\"preview\" action={buttonize} %}
```
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, _Record, Context) ->
	Actions = [
		{event, [
		            {type,mouseover},
		            {actions, {add_class, [{class,hover}]} }
		        ]},
		{event, [
		            {type,mouseout},
		            {actions, {remove_class, [{class,hover}]} }
		        ]},
		{event, [
		            {type,mousedown},
		            {actions, {add_class, [{class,clicked}]} }
		        ]},
		{event, [
		            {type,mouseup},
		            {actions, {remove_class, [{class,clicked}]} }
		        ]}
	],
	z_render:render_actions(TriggerId, TargetId, Actions, Context).
