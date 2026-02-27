%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'last' filter, return the last element of a list or the last character of a string

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

-module(filter_last).
-moduledoc("
Returns the last character or element.

Returns the last element of the value. When the value is a list then the last element of the list is returned, when the
value is a binary then the last byte of the binary is returned.

For example:


```django
{{ value|last }}
```

When value is the list `hello` then the output will be `o`.

**Note:** This function is not safe to use with multibyte character values, use with care.

See also

[first](/id/doc_template_filter_filter_first)").
-export([last/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

last(undefined, _Context) ->
    undefined;
last(Input, _Context) when is_binary(Input) ->
    case size(Input) of
        0 -> Input;
        N ->
            Offset = N - 1,
            <<_:Offset/binary, Byte>> = Input,
            Byte
    end;
last(Other, Context) ->
    case z_template_compiler_runtime:to_list(Other, Context) of
        [] -> <<>>;
        L -> lists:last(L)
    end.
