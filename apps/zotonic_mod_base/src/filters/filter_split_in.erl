%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'split_in' filter, split a list in a number of rows

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

-module(filter_split_in).
-moduledoc("
See also

[chunk](/id/doc_template_filter_filter_chunk), [vsplit\\_in](/id/doc_template_filter_filter_vsplit_in)

This filter split a list in shorter lists. It splits an array in N sub-arrays of more or less equal length. This is
useful when displaying a list of items in columns.

For example:


```django
{% with [1,2,3,4,5,6]|split_in:3 as a,b,c %}
    {% for n in a %}{{ n|format_number }} {% endfor %}
{% endwith %}
```

This displays `1 4`. The variable b will be `[2,5]` and the variable c will be `[3,6]`.
").
-export([split_in/3]).


split_in(undefined, _N, _Context) ->
    undefined;
split_in(In, N, Context) ->
    z_utils:split_in(z_template_compiler_runtime:to_list(In, Context), N).
