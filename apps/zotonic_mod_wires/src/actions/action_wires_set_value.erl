%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2025 Marc Worrell
%% @doc Set the value of an input element
%% @end

%% Copyright 2009-2025 Marc Worrell
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

-module(action_wires_set_value).
-moduledoc("
Set the value of a form field.

Example:


```erlang
<input type=\"text\" id=\"x\" name=\"xyz\" value=\"\">
{% button text=\"fill\" action={set_value target=\"x\" value=\"etaoinstrdlu\"} %}
```

Clicking on the button will set the value of the input element to the most interesting string `etaoinstrdlu`.

This action can set the value of any input element, select or text area. It uses the jQuery `val()` method to set the value.

Optionally the argument `trigger_event` can be passed to trigger a change event:


```erlang
{% button text=\"fill\" action={set_value target=\"x\" value=\"...\" trigger_event} %}
```

To trigger a custom event:


```erlang
{% button text=\"fill\" action={set_value target=\"x\" value=\"...\" trigger_event=\"myevent\"} %}
```
").

-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    CssSelector = z_render:css_selector(proplists:get_value(id, Args, TargetId), Args),
    Attr = proplists:get_value(value_arg, Args, value),
    Value = proplists:get_value(Attr, Args, <<>>),
    Script = iolist_to_binary([
         $$, $(, $', CssSelector, $', $),
         <<".val(\"">>, z_utils:js_escape(Value), $", $)
       ]),
   Script1 = case proplists:get_value(trigger_event, Args) of
      undefined -> Script;
      false -> Script;
      true ->
         [
            Script,
            <<".trigger(\"changed\")">>
         ];
      Event ->
         [
            Script,
            <<".trigger(\"">>, z_utils:js_escape(Event), $", $)
         ]
   end,
   {[Script1, $;], Context}.
