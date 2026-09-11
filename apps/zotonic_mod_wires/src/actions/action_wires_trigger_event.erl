%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%%
%% @doc Trigger a name {% wire %} with an action.  All args will be args to the named wire.
%% @end

%% Copyright 2010 Marc Worrell
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

-module(action_wires_trigger_event).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "messaging_and_pubsub", "javascript"]
}).
-moduledoc("
Trigger a named `{% wire %}` event in the browser. The required `name` argument
identifies the wire; every other action argument is passed to the named event.

```django
{% wire name=\"refresh-preview\" action={update target=\"preview\" template=\"_preview.tpl\"} %}
{% button text=\"Refresh\" action={trigger_event name=\"refresh-preview\" id=id} %}
```

In this example the named wire receives `id` as an event argument when the
button is clicked.
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Name = z_utils:js_escape(proplists:get_value(name, Args, "")),
    {[<<"z_event(\"">>,Name,<<"\", ">>, z_utils:js_object(Args), $), $;], Context}.
