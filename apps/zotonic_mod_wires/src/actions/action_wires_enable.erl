%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Enable an element.  Removes the 'disabled' attribute and removes the class 'disabled'.

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

-module(action_wires_enable).
-moduledoc("
See also

action [disable](/id/doc_template_action_action_disable).

Resets the “disabled” attribute of a HTML tag and removes the CSS class “disabled”.

Example:


```django
<input id=\"myid\" disabled=\"disabled\" class=\"disabled\" type=\"text\" value=\"hello\" />
{% button text=\"enable\" action={enable target=\"myid\"} %}
```

After clicking the button the input will be:


```django
<input id=\"myid\" class=\"\" type=\"text\" value=\"hello\" />
```
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_wires_jquery_effect:render_action(TriggerId, TargetId, [{type,enable}|Args], Context).
