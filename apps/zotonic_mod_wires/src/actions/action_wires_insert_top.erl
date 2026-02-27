%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-11-06
%% @doc Insert the result of a render action at the top of an HTML element.

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

-module(action_wires_insert_top).
-moduledoc("
Inserts HTML before the contents of an HTML element.

Adds a template or a literal HTML text before the existing content.

Example:


```django
<div id=\"mydiv\"><p>Bye Bye.</p></div>
{% button text=\"hello\" action={insert_top target=\"mydiv\" text=\"<p>Hello World!</p>\"} %}
```

After the button is clicked, the contents of the div will be &lt;p>Hello World!</p>&lt;p>Bye Bye.</p>.

Another example, now rendering a template:


```django
<ul id=\"mylist\"><li>Some item</li></li>
{% button text=\"hello\" action={insert_top target=\"mylist\" template=\"_list_item.tpl\" id=42} %}
```

This insert the output of the template \\_list_item.tpl above the existing &lt;li/>. All arguments to the update
action are also arguments to the template.

See also

actions [insert_after](/id/doc_template_action_action_insert_after),
[insert_before](/id/doc_template_action_action_insert_before),
[insert_bottom](/id/doc_template_action_action_insert_bottom) and [update](/id/doc_template_action_action_update).").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4
]).

render_action(TriggerId, TargetId, Args, Context) ->
    action_wires_update:render_update(insert_top, TriggerId, TargetId, Args, Context).
