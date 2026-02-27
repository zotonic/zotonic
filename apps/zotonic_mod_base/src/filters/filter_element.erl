%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'element' filter, return the nth element

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

-module(filter_element).
-moduledoc("
Select an element from a tuple or list of tuples.

For example:


```django
{{ value|element:1 }}
```

When value is a list of tuples `[{312,0.34}, {200,0.81}]` then the output is the list `[312,200]`.

When value is just a tuple, `{123, 22, 11}`, the output of `|element:1` is `123`.

See also

[before](/id/doc_template_filter_filter_before), [after](/id/doc_template_filter_filter_after)").
-export([element/3]).


element(List, N, _Context) when is_list(List) ->
	[ element(N,Tuple) || Tuple <- List, is_tuple(Tuple) ];
element(Tuple, N, _Context) when is_tuple(Tuple) ->
	element(N, Tuple);
element(_Value, _N, _Context) ->
	undefined.
