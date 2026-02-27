%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'chunk' filter, split a list in sublists of a maximum length

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

-module(filter_chunk).
-moduledoc("
This filter splits a list in shorter lists. It splits an array in sub-arrays of at most a given length. This is useful
when displaying a list of items in columns or rows.

For example:


```django
{% for s in [1,2,3,4,5]|chunk:2 %}
    {% for n in s %}{{ n|format_number }} {% endfor %} *
{% endfor %}
```

This displays `1 2 * 3 4 * 5 *`, as the array is split in three chunks. The last chunk is not filled to the maximum
length. Then number of chunks depends on the length of the input list, this in contrary to the split_in filters where
the number of splits is fixed and the length per split is variable.

See also

[split_in](/id/doc_template_filter_filter_split_in), [vsplit_in](/id/doc_template_filter_filter_vsplit_in)").
-export([chunk/3]).


chunk(undefined, _N, _Context) ->
    undefined;
chunk(In, N, Context) ->
    chunk1(z_template_compiler_runtime:to_list(In, Context), N, []).

chunk1([], _, Acc) ->
    lists:reverse(Acc);
chunk1(List, N, Acc) ->
    {Chunk, List1} = chunk_take(List, N, []),
    chunk1(List1, N, [Chunk|Acc]).

chunk_take([], _, Acc) ->
    {lists:reverse(Acc), []};
chunk_take(L, 0, Acc) ->
    {lists:reverse(Acc), L};
chunk_take([H|T], N, Acc) ->
    chunk_take(T, N-1, [H|Acc]).
