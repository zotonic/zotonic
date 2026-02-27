%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'is_visible' filter, filters a list of ids or tuples {id, score}

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

-module(filter_is_visible).
-moduledoc("
Filter a list of resource ids so that only the visible ids remain.

This filter can be applied to a list of resource ids or a single resource id.

This filter can be applied to a list of resource ids. Only those resource ids that are visible for the current user
remain. Optionally the filter only returns the first n matches.

An example:


```django
<ul>
{% for part_id in m.rsc[id].o.haspart|is_visible %}
    <li>{{ m.rsc[part_id].title }}</li>
{% endfor %}
</ul>
```

This will list all collection members that are visible, preventing empty list items.

Whilst:


```django
{% for part_id in m.rsc[id].o.haspart|is_visible:3 %}
    {{ m.rsc[part_id].title }}
{% endfor %}
```

Lists only the first three collection members that are visible.

See also

[is_a](/id/doc_template_filter_filter_is_a), [is_not_a](/id/doc_template_filter_filter_is_not_a), [filter](/id/doc_template_filter_filter_filter)").
-export([is_visible/2, is_visible/3]).


is_visible(Arg, Context) ->
    z_list_of_ids_filter:filter(Arg, fun(Id) -> is_rsc_visible(Id, Context) end, Context).

is_visible(List, N, Context) ->
    z_list_of_ids_filter:filter(List, fun(Id) -> is_rsc_visible(Id, Context) end, N, Context).


is_rsc_visible({Id, _Score}, Context) when is_integer(Id) ->
    z_acl:rsc_visible(Id, Context);
is_rsc_visible(Id, Context) ->
    z_acl:rsc_visible(Id, Context).