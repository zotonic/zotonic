%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc Force in range.

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

-module(filter_minmax).
-moduledoc("
See also

[max](/id/doc_template_filter_filter_max), [min](/id/doc_template_filter_filter_min)

Force the given value in the given range.

This clamps the filter value between the two filter arguments.

Example:


```django
{% print 3|to_integer|minmax:10:20 %}
```

This will print `10`, since that is the minimum value allowed.

Passing in `undefined` will not clamp the value but return `undefined`.
").
-export([minmax/4]).

minmax(undefined, _Min, _Max, _Context) ->
    undefined;
minmax(Value, Min, Max, Context) ->
    filter_max:max(filter_min:min(Value, Max, Context), Min, Context).


