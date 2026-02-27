%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-27

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

-module(action_wires_dialog).
-moduledoc("
Opens a dialog with a predefined HTML content and title.

Example:


```django
{% button action={dialog title=\"Wisdom\" text=\"<p>The world is a pancake.</p>\"} %}
```

This opens a dialog with the title “Wisdom”. The dialog is empty except for the text “The world is a pancake”.

Normally, instead of this action, the action [dialog_open](/id/doc_template_action_action_dialog_open) is used. The
action [dialog_open](/id/doc_template_action_action_dialog_open) shows a dialog that is rendered on the server.

There can be many levels of dialogs open, they are designated by a *level*, the default dialog opens at level 0. Higher
levels are displayed above lower levels. There is a special level `\"top\"` which ensures that a dialog is always opened
above any other open dialog.

| Argument | Required | Description                                                                      |
| -------- | -------- | -------------------------------------------------------------------------------- |
| title    | required | Dialog header title                                                              |
| text     | required | Dialog body text                                                                 |
| width    | optional | Dialog width in pixels. Use `\"large\"` for a wide dialog and `\"small\"` for a small dialog. |
| addclass | optional | classname will be appended to default dialog class                               |
| backdrop | optional | boolean (0, 1), or the string `\"static\"` for a modal dialog (does not close on backdrop click); default 1 |
| center   | optional | boolean (0, 1) default 1; set to 0 to align the dialog at the top                |
| keyboard | optional | boolean (true, false) default: true; if true, closes when escape keys is pressed |
| level    | optional | Nesting of the dialog. Non negative integer, higher numbered levels are displayed above lower levels. Special level `\"top\"` to force display on top. |

See also

actions [dialog_open](/id/doc_template_action_action_dialog_open),
[dialog_close](/id/doc_template_action_action_dialog_close) and [overlay_open](/id/doc_template_action_action_overlay_open).").
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(_TriggerId, _TargetId, Args, Context) ->
	Script = [<<"z_dialog_open(">>, z_utils:js_object(Args, Context), $), $; ],
	{Script, Context}.
