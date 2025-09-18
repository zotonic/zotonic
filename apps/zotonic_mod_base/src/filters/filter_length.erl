%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc 'length' filter, return the number of elements or length of a string
%% or other erlang term.

%% Copyright 2010-2022 Marc Worrell
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

-module(filter_length).
-moduledoc("
Returns the length of the value.

The length of a list is the number of elements in the list, the length of a binary is the number of bytes in the binary.

For example:


```django
{{ value|length }}
```

When value is the list “hello” then the output will be “5”.

**Note:** With multi-byte values this function does not return the number of characters, it returns the number of bytes. This may change in a future release.
").
-export([length/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

length(undefined, _Context) -> undefined;
length([], _Context) -> 0;
length(<<>>, _Context) -> 0;
length(#trans{} = Tr, Context) ->
    length(z_trans:lookup_fallback(Tr, Context), Context);
length(#search_result{ result = R }, _Context) when is_list(R) ->
    length(R);
length(Input, _Context) when is_binary(Input) -> z_string:len(Input);
length(Input, _Context) when is_list(Input) -> erlang:length(Input);
length(Input, _Context) when is_tuple(Input) -> erlang:tuple_size(Input);
length(Input, _Context) when is_map(Input) -> erlang:map_size(Input);
length(Input, Context) ->
    erlang:length(z_template_compiler_runtime:to_list(Input, Context)).
