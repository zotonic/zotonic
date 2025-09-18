%% @author Marc Worrell
%% @copyright 2015 Marc Worrell
%% @doc Insert the result of a render action into an iframe.

%% Copyright 2015 Marc Worrell
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

-module(action_wires_update_iframe).
-moduledoc("
See also

action [update](/id/doc_template_action_action_update).

Note

This action is only used to update an `iframe` element. Use the [update](/id/doc_template_action_action_update) action
for updating the contents of a normal HTML element.

Updates the content of an iframe with a template or a literal HTML text.

Example:


```django
<iframe id=\"preview\"></iframe>
{% button text=\"Show Email\" action={update_iframe target=\"preview\" template=\"email.tpl\" recipient_id=m.acl.user} %}
```

When clicked, the contents of the iframe will be set to the rendered `email.tpl` template. This replaces any content present.

| Argument   | Description                                                                      | Example                    |
| ---------- | -------------------------------------------------------------------------------- | -------------------------- |
| target     | The id of the iframe receiving the rendered HTML document.                       | target=”my-view”           |
| text       | Literal HTML doc to be inserted, no escaping will be done.                       | text=”&lt;html>...</html>” |
| template   | Name of the template to be rendered.                                             | template=”page.tpl”        |
| catinclude | Add this argument to use a [catinclude](/id/doc_template_tag_tag_catinclude) instead of a normal include of the template. The id argument *must* be present for a catinclude to work. | catinclude id=1            |

All other arguments are passed as-is to the included template(s).
").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([
    render_action/4
]).

render_action(TriggerId, TargetId, Args, Context) ->
    action_wires_update:render_update(iframe, TriggerId, TargetId, Args, Context).
