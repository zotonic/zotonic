%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016-2023 Arjan Scherpenisse
%% @doc 'without' filter, removes values from the given list
%% @end

%% Copyright 2016-2023 Arjan Scherpenisse
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

-module(filter_without).
-moduledoc("
Remove the items given in the argument from the filter value.

For example:


```erlang
{% print [1,2,3]|without:[2] %}
```

prints:


```erlang
[1,3]
```

This filter also works on list-like values like resource edges:


```erlang
{% for id in id.o.tags|without:some_id.o.tags %}
```

Iterates of all tags edges of the given id, for each id that is not also an edge of some_id.
").

-export([without/3]).

without(List, OtherList, Context) ->
    z_template_compiler_runtime:to_list(List, Context) -- z_template_compiler_runtime:to_list(OtherList, Context).
