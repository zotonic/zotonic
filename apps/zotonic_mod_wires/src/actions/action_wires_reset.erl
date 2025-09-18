%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Resets a form.  Either a targeted form or form closest to an id or the trigger.

%% Copyright 2011 Marc Worrell
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

-module(action_wires_reset).
-moduledoc("
Resets the enclosing form, a specifically targeted form or the closest form to an element.

Example:


```erlang
<form method=\"get\" action=\"/search\">
    <input type=\"text\" name=\"q\" value=\"\" />
    {% button text=\"search\" action={submit} %}
    {% button text=\"cancel\" action={reset} %}
</form>
```

Another example:


```erlang
<form id=\"search-form\" method=\"get\" action=\"/search\">
   <input type=\"text\" id=\"q\" name=\"q\" value=\"\" />
</form>
{% button text=\"search\" action={submit closest=\"q\"}
{% button text=\"cancel\" action={reset closest=\"q\"} %}
```

Clicking on the button will reset the form search-form as it is the closest form to the element with id q.

The reset form action is mostly used in the result of event handlers.

This action takes two possible arguments, when neither is defined then the form enclosing the trigger element will be reset.

| Argument | Description                                                                      | Example              |
| -------- | -------------------------------------------------------------------------------- | -------------------- |
| target   | The id of the form to be reset.                                                  | target=”search-form” |
| closest  | The id of an element that is close to the form to be reset. When no argument value is supplied then it defaults to the id of the trigger element (for example the button the action is coupled to). | closest              |
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(closest, Args) of
        undefined ->
            case proplists:get_value(id, Args, proplists:get_value(target, Args)) of
                undefined ->
                    {["$(\"#", TriggerId, "\").closest(\"form\")[0].reset();"], Context};
                Id ->
                    {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(Id)), <<"\")[0].reset();">> ], Context}
            end;
        true ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(TriggerId)), <<"\").closest(\"form\")[0].reset();">> ], Context};
        CloseId ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(CloseId)), <<"\").closest(\"form\")[0].reset();">> ], Context}
    end.
