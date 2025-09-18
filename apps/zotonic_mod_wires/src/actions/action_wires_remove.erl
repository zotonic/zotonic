%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Remove an element from the page.

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

-module(action_wires_remove).
-moduledoc("
See also

[Actions](/id/template_action#actions), [button](/id/doc_template_scomp_scomp_button#scomp-button)

Remove an element from the page.

For example, the following removes the foo div from the page:


```django
<div id=\"foo\">I am the foo div</div>
{% button text=\"Remove foo\" action={remove target=\"foo\"} %}
```

Without target, the action removes its triggering element:


```django
{% button text=\"Click me to remove me\" action={remove} %}
```
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
    action_wires_jquery_effect:render_action(TriggerId, TargetId, [{type,remove}|Args], Context).
