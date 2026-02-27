%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc Open an overlay with content from a template.

%% Copyright 2016 Marc Worrell
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

-module(action_wires_overlay_open).
-moduledoc("
Renders a template on the server and opens a full screen overlay with the HTML output of the template.

Example:


```django
{% button text=\"show story\" action={overlay_open template=\"_story.tpl\" id=1234} %}
```

This opens an overlay over the current content. The template `_story.tpl` will be rendered with the argument `id` (and
possibly any other arguments). The rendered html will then be shown inside the overlay.

The overlay template is a `div` with the class `modal-overlay`. Extra classes can be added using the `class` argument:


```django
{% wire action={overlay_open template=\"_splash.tpl\" class=\"splash\"} %}
```

The overlay action has the following arguments:

| Argument | Description                                                                      | Example                   |
| -------- | -------------------------------------------------------------------------------- | ------------------------- |
| template | Template to render in the overlay                                                | template=”\\\\_overlay.tpl” |
| class    | Extra CSS class(es) to add to the overlay diff.                                  | class=”myclass other”     |
| level    | Nesting of the overlay. Non negative integer, higher numbered levels are displayed above lower levels. Special level `\"top\"` to force display on top, above all dialogs and other overlays. | level=”top”               |

All (extra) arguments are passed to the rendered template.

See also

actions [overlay_close](/id/doc_template_action_action_overlay_close),
[dialog_open](/id/doc_template_action_action_dialog_open) and [dialog](/id/doc_template_action_action_dialog).").
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback({overlay, Args}, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.

%% @spec event(Event, Context1) -> Context2
event(#postback{message={overlay, Args}}, Context) ->
    {template, Template} = proplists:lookup(template, Args),
    z_render:overlay(Template, Args, Context).

