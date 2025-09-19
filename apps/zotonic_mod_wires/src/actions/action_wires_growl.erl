%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

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

-module(action_wires_growl).
-moduledoc("
See also

actions [alert](/id/doc_template_action_action_alert) and [confirm](/id/doc_template_action_action_confirm); and
[Enabling Growl Notifications](/id/doc_cookbook_frontend_growl#guide-cookbook-frontend-growl).

Show a message in the upper right corner of the browser window. The message will automatically disappear after some time.

Example:


```django
{% button action={growl text=\"hello world\"} %}
```

Shows a message with the text “hello world”.

Growl accepts the following arguments:

| Argument | Description                                                           | Example      |
| -------- | --------------------------------------------------------------------- | ------------ |
| text     | The text to be displayed.                                             | text=”Hola!” |
| stay     | When true then the message does not disappear automatically           | stay         |
| type     | Type of the message, one of “notice” or “error”. Default is “notice”. | type=”error” |
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Text   = proplists:get_value(text, Args, ""),
    Stay   = proplists:get_value(stay, Args, 0),
    Type   = proplists:get_value(type, Args, "notice"),

    TextJS = z_utils:js_escape(Text, Context),
    StayJS = case z_convert:to_bool(Stay) of
                true  -> $1;
                false -> $0
             end,
	TypeJS = z_utils:js_escape(Type, Context),
	Script = [<<"z_growl_add(\"">>,TextJS,<<"\", ">>, StayJS,<<",\"">>, TypeJS, $", $), $;],
	{Script, Context}.
