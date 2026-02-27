%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2025 Marc Worrell
%% @doc Convert a value to boolean
%% @end

%% Copyright 2025 Marc Worrell
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

-module(filter_to_boolean).
-moduledoc("
Convert the input to a boolean value.

Example:


```django
{{ \"123\"|to_boolean }}
```

Results in the boolean value `true`.

This filter uses the `z_convert:to_bool/1` function.

See also

[to_binary](/id/doc_template_filter_filter_to_binary), [format_number](/id/doc_template_filter_filter_format_number), [format_integer](/id/doc_template_filter_filter_format_integer)").
-export([to_boolean/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

to_boolean(undefined, _Context) ->
    undefined;
to_boolean(#trans{} = Tr, Context) ->
    to_boolean(z_trans:lookup_fallback(Tr, Context), Context);
to_boolean(B, _Context) when is_binary(B) ->
    try
        z_convert:to_bool(z_string:trim(B))
    catch
        _:_ -> undefined
    end;
to_boolean(N, _Context) ->
    z_convert:to_bool(N).

