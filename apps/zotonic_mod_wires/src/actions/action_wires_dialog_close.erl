%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Close the dialog, optionally pass the level of the dialog to be closed.
%% If level 0 is passed then all dialogs are closed.  If no level is passed then
%% the top-most dialog is closed.
%% @end.

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

-module(action_wires_dialog_close).
-moduledoc("
See also

actions [dialog\\_open](/id/doc_template_action_action_dialog_open) and [dialog](/id/doc_template_action_action_dialog).

Closes a dialog. When there is no dialog open then nothing happens.

Example, closing the top-most dialog:


```django
{% button text=\"cancel\" action={dialog_close} %}
```

This button closes any open dialog when clicked.

There can be many levels of dialogs open, they are designated by a *level*, the default dialog opens at level 0. Higher
levels are displayed above lower levels.

To close all open dialogs, pass level 0:


```django
{% button text=\"cancel\" action={dialog_close level=0} %}
```
").
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

render_action(_TriggerId, _TargetId, Args, Context) ->
	Level = case proplists:get_value(level, Args) of
		undefined -> <<"undefined">>;
		<<"top">> -> <<"'top'">>;
		Lvl -> z_convert:to_binary(z_convert:to_integer(Lvl))
	end,
	{<<"z_dialog_close({level:", Level/binary ,"});">>, Context}.
