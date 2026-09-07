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

-module(action_wires_animate).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "user_interface_and_interaction", "javascript"]
}).
-moduledoc("
Add a `$(..).animate` jQuery call to the target element.

Arguments:

* `target` selects the element to animate.
* `options` is a list or map of CSS properties and target values passed to
  jQuery's `animate` function.
* `speed` is the duration in milliseconds, or a jQuery duration such as
  `\"slow\"`. The default is 350 milliseconds.
* `easing` is the name of the jQuery easing function.

For example, this fades an element to half opacity:

```django
{% button text=\"Dim\" action={animate target=\"panel\" options=[{opacity, 0.5}] speed=250} %}
```
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_wires_jquery_effect:render_action(TriggerId, TargetId, [{type,animate}|Args], Context).
