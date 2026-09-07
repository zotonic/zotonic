%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Diff two resources
%% @end

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

-module(filter_admin_merge_diff).
-moduledoc(#{
    zotonic_keywords => ["reference", "frontend_developer", "template_filter", "content_authoring", "resource", "compare"]
}).
-moduledoc("
Compare two resources and return a formatted list of their differences.

Both inputs can be resource ids or names. The result is intended for the
resource merge interface, where editors select which values to retain.

For example:

```django
{% for difference in id|admin_merge_diff:other_id %}
    {{ difference }}
{% endfor %}
```
").

-export([
    admin_merge_diff/3
    ]).

admin_merge_diff(Id1, Id2, Context) ->
    Rsc1 = m_rsc:get(Id1, Context),
    Rsc2 = m_rsc:get(Id2, Context),
    admin_rsc_diff:format(Rsc1, Rsc2, Context).
