%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'center' filter, center a string inside a certain amount of characters

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

-module(filter_center).
-moduledoc("
See also

[rjust](/id/doc_template_filter_filter_rjust), [ljust](/id/doc_template_filter_filter_ljust)

Centers the value in a field of a certain width using spaces.

For example:


```django
{{ value|center:7 }}
```

When value is `hello` then the output is `_hello_` (with spaces).

Centering only works for single byte character values. At this moment there is no support for multi-byte unicode characters.
").
-export([center/3]).

center(undefined, _Number, _Context) ->
    undefined;
center(Input, Number, Context) when is_binary(Input) ->
    list_to_binary(center(binary_to_list(Input), Number, Context));
center(Input, Number, _Context) when is_list(Input) ->
    string:centre(Input, z_convert:to_integer(Number)).
