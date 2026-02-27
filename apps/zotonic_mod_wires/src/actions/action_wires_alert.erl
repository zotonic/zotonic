%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Show an alert dialog. Uses the template '_action_dialog_alert.tpl' to
%% render the contents of the alert.
%% Based on code copyright (c) 2008-2009 Rusty Klophaus
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(action_wires_alert).
-moduledoc("
Show an alert dialog.

Example:


```django
{% button action={alert text=\"hello world\"} %}
```

Shows an alert dialog with the text “hello world”.

Alert accepts the following arguments:

| Argument    | Description                                                                      | Example         |
| ----------- | -------------------------------------------------------------------------------- | --------------- |
| title       | Title of the alert.                                                              | title=”Alert”   |
| text        | The text to be displayed.                                                        | text=”Hola!”    |
| button      | Text for the button. Defaults to “OK”                                            | button=”Bummer” |
| only\\\\_text | Set this to not show the “OK” button.                                            | only\\\\_text     |
| action      | Action to be done when the user clicks on the OK button. There can be multiple actions. |                 |
| backdrop    | Show backdrop: true, false, or the string “static”                               | backdrop=false  |
| level       | Nesting of the dialog. Non negative integer, higher numbered levels are displayed above lower levels. Special level `\"top\"` to force display on top. | level=”top”     |

The alert dialog is rendered using the `_action_dialog_alert.tpl` template. Overrule this template to change the
contents of the alert dialog.

See also

actions [growl](/id/doc_template_action_action_growl) and [confirm](/id/doc_template_action_action_confirm).").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({alert, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

-spec event(Event, Context) -> Context2 when
    Event :: #postback{},
    Context :: z:context(),
    Context2 :: z:context().
event(#postback{message={alert, Args}}, Context) ->
    Title = proplists:get_value(title, Args, ?__(<<"Alert">>, Context)),
    Vars = [
        {title, Title},
        {action, proplists:get_all_values(action, Args)}
        | Args
    ],
    z_render:dialog(Title, "_action_dialog_alert.tpl", Vars, Context).

