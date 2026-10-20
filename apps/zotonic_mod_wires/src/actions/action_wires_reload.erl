%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2026 Marc Worrell
%% @doc Refresh the current page.
%% @end

%% Copyright 2009-2026 Marc Worrell
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

-module(action_wires_reload).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "wire_action", "routing_and_redirects", "javascript"]
}).
-moduledoc("
Reload the current page.

Example:


```erlang
{% button text=\"refresh\" action={reload} %}
```

Clicking on the button will reload the page.
The page is reloaded by calling the Javascript function `z_reload()`.
You can also pass optional arguments to the function.

Optional arguments:

z_language: The language to reload the page in. If omitted or empty, the current language is preserved. An existing `z.lang` cookie is updated only in the top-level window.
z_rewrite_url: When changing language, set this to true to put the language in the URL path. Otherwise the language is added as a `z_language` query parameter.

Other arguments are added as query variables, replacing existing values with the same name.
For example, passing `foo=bar` on `/current/path` reloads `/current/path?foo=bar`.
Unrelated existing query parameters are preserved.
").

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    case Args of
        [] -> {"z_reload();", Context};
        _ -> {["z_reload(",z_utils:js_object(Args),$),$;], Context}
    end.
