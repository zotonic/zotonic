%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'rand' filter, return a random number

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

-module(filter_rand).
-moduledoc("
See also

[randomize](/id/doc_template_filter_filter_randomize), [random](/id/doc_template_filter_filter_random)

Generates a random number. The number is from, and including, 1 up to, and including, the input value.

Example:


```django
{{ 100|rand|format_integer }}
```

Might output “42”.

The rand filter can also generate a floating point value by given it a floating point number or a string representing a
floating point number as input. It will generate a number from, but not including, 0 to, and including, the input value:


```django
{{ \"4.0\"|rand|format_number }}
```

Might output “3.1415926536”
").
-export([rand/1, rand/2]).


rand(_Context) ->
	z_ids:number().

rand(Max, Context) when is_binary(Max) ->
	rand(binary_to_list(Max), Context);
rand(Max, Context) when is_list(Max) ->
	case lists:member($., Max) of
		true ->  rand(erlang:list_to_float(Max), Context);
		false -> rand(z_convert:to_integer(Max), Context)
	end;
rand(Max, _Context) when is_float(Max) ->
	z_ids:number(1000000000000) / 1000000000000 * Max;
rand(Max, _Context) when is_integer(Max) ->
	z_ids:number(z_convert:to_integer(Max));
rand(_, _Context) ->
	undefined.
