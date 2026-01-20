%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Retrieve a property from a map, proplist or resource.

%% Copyright 2026 Maas-Maarten Zeeman
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


-module(filter_extract).
-moduledoc("
Extract a specific property from each item in a list.

This filter extracts a named property from maps, proplists, or resources in a list,
returning a new list containing only those values.

For example:

```django
    {{ items | extract:\"price\" }}
```

When `items` is a list like `[#{price => 10, name => \"A\"}, #{price => 20, name => \"B\"}],
this returns `[10, 20]`.

This is particularly useful when combined with the sum filter for analytics:

```django
    {{ orders | extract:\"amount\" | sum }}
```

When extracting from resources, you can use any valid resource property:

```django
    {{ m.search[{query cat=\"product\"}] | extract:\"title\" }}
```

If applied to a single item instead of a list, it extracts and returns the property value
from that item.

If the input is `undefined`, the filter returns an empty list `[]`.
").
-export([extract/3]).

extract(undefined, _Prop, _Context) ->
    [];
extract(List, Prop, Context) when is_list(List) ->
	[ extract_value(Elt, Prop, Context) || Elt <- List ];
extract(Item, Prop, Context) ->
    extract_value(Item, Prop, Context).

extract_value(Item, Prop, Context) ->
    z_template_compiler_runtime:find_value(Prop, Item, #{}, Context).
