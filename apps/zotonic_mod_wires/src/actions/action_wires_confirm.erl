%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Display a confirmation dialog.
%% @end

%% Based on code copyright (c) 2008-2009 Rusty Klophaus

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

-module(action_wires_confirm).
-moduledoc("
Show a JavaScript confirm message and on confirmation triggers one or more actions and/or sends a postback to the server.

Example:


```django
{% button action={confirm
                  text=\"Format hard disk?\"
                  ok=\"Go Ahead!\"
                  action={growl text=\"Better not\"}
                  is_danger}
%}
```

Shows a JavaScript dialog with the question “Format hard disk?”. If this dialog is confirmed then the growl message
“Better not” will appear. If the dialog is denied or canceled then nothing happens.

If there is a postback defined then the event handler for the postback will be called like:


```erlang
event(#postback{message=Message, trigger=TriggerId, target=TargetId}, Context).
```

Confirm accepts the following arguments:

| Argument        | Description                                                                      | Example                                        |
| --------------- | -------------------------------------------------------------------------------- | ---------------------------------------------- |
| text            | The text to be displayed.                                                        | text=\\\\_”The answer to life and the rest?”     |
| title           | Title above the alert, defaults to `_\"Confirm\"`                                  | title=\\\\_”Rescue the world, with an answer”    |
| ok              | The text of the ok button, defaults to `_\"OK\"`                                   | text=”42”                                      |
| cancel          | The text of the cancel button, defaults to `_\"Cancel\"`                           | text=”No, thanks for the fish”                 |
| text\\\\_template | Template used to render the text, all action arguments are passed to the template. | text\\\\_template=”\\\\_fancy\\\\_confirm.tpl”       |
| action          | One or more actions to be executed on confirmation. This argument can be repeated. | action=\\\\{alert text=”you said ok”\\\\}          |
| on\\\\_cancel     | One or more actions to be executed on cancelation                                | on\\\\_cancel=\\\\{alert text=”you said cancel”\\\\} |
| postback        | Event to be sent back to the server if the ok button is clicked.                 | postback=”clicked\\\\_confirm”                   |
| delegate        | Erlang module handling the postback. Defaults to the controller generating the page. | delegate=”my\\\\_event\\\\_module”                 |
| is\\\\_danger     | If the ‘ok’ button should be flagged as dangerous.                               | is\\\\_danger                                    |
| level           | Nesting of the dialog. Non negative integer, higher numbered levels are displayed above lower levels. Special level `\"top\"` to force display on top. | level=”top”                                    |

See also

actions [alert](/id/doc_template_action_action_alert) and [growl](/id/doc_template_action_action_growl).").

-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({confirm, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @doc Fill the dialog with the confirmation template.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={confirm, Args}}, Context) ->
    Title = proplists:get_value(title, Args, ?__(<<"Confirm">>, Context)),
    {IsTemplate, Text,Context1} = case proplists:get_value(text_template, Args) of
        undefined ->
            {false, proplists:get_value(text, Args), Context};
         Template ->
            {Txt, Ctx} = z_template:render_to_iolist(Template, Args, Context),
            {true, Txt, Ctx}
    end,
    Vars = [
        {title, Title},
        {text, Text},
        {is_template, IsTemplate},
        {is_danger, proplists:get_value(is_danger, Args, false)},
        {ok, proplists:get_value(ok, Args)},
        {cancel, proplists:get_value(cancel, Args)},
        {action, proplists:get_all_values(action, Args)},
        {on_cancel, proplists:get_all_values(on_cancel, Args)},
        {postback, proplists:get_value(postback, Args)},
        {delegate, proplists:get_value(delegate, Args)},
        {level, proplists:get_value(level, Args)}
    ],
    z_render:dialog(Title, "_action_dialog_confirm.tpl", Vars, Context1).
