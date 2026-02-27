%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc Convert a value to integer
%% @end

%% Copyright 2010-2024 Marc Worrell
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

-module(filter_to_integer).
-moduledoc("
Convert the input to an integer value.

Example:


```django
{{ \"123\"|to_integer }}
```

Results in the integer value `123`.

This filter uses the `z_convert:to_integer/1` function.

See also

[to_binary](/id/doc_template_filter_filter_to_binary), [format_number](/id/doc_template_filter_filter_format_number), [format_integer](/id/doc_template_filter_filter_format_integer)").
-export([to_integer/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

to_integer(undefined, _Context) ->
    undefined;
to_integer(#trans{} = Tr, Context) ->
    to_integer(z_trans:lookup_fallback(Tr, Context), Context);
to_integer(B, _Context) when is_binary(B) ->
	try
	    z_convert:to_integer(z_string:trim(B))
	catch
		_:_ -> undefined
	end;
to_integer(N, _Context) ->
	try
	    z_convert:to_integer(N)
	catch
		_:_ -> undefined
	end.

