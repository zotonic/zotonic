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

-module(action_wires_toggle).
-moduledoc("
See also

actions [show](/id/doc_template_action_action_show), [hide](/id/doc_template_action_action_hide),
[fade\\_in](/id/doc_template_action_action_fade_in), [fade\\_out](/id/doc_template_action_action_fade_out),
[slide\\_down](/id/doc_template_action_action_slide_down), [slide\\_up](/id/doc_template_action_action_slide_up),
[slide\\_fade\\_in](/id/doc_template_action_action_slide_fade_in) and [slide\\_fade\\_out](/id/doc_template_action_action_slide_fade_out).

Toggle the visibility of an element.

Example:


```django
{% button action={toggle target=\"myid\"} %}
```

Shows the element with id myid if it was hidden, otherwise hide it.
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_wires_jquery_effect:render_action(TriggerId, TargetId, [{type,toggle}|Args], Context).
