%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2023 Marc Worrell
%% @doc 'vsplit_in' filter, split a list in a number of columns
%% @end

%% Copyright 2010-2023 Marc Worrell
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

-module(filter_vsplit_in).
-moduledoc("
This filter splits a list in shorter lists. It splits an array in N sub-arrays of more or less equal length. This is
useful when displaying a list of items in columns.

Note that it splits the array in a different way than [split_in](/id/doc_template_filter_filter_split_in) does: The
filter split_in takes alternating elements from the array, where vsplit_in takes complete runs at a time. See the
example below.

For example:


```django
{% with [1,2,3,4,5,6]|vsplit_in:3 as a,b,c %}
    {% for n in a %}{{ n|format_number }} {% endfor %}
{% endwith %}
```

This displays `1 2`. The variable b will be `[3,4]` and the variable c will be `[5,6]`.

See also

[chunk](/id/doc_template_filter_filter_chunk), [split_in](/id/doc_template_filter_filter_split_in)").
-export([vsplit_in/3]).


vsplit_in(undefined, _N, _Context) ->
    undefined;
vsplit_in(In, N, Context) ->
    z_utils:vsplit_in(z_template_compiler_runtime:to_list(In, Context), N).
