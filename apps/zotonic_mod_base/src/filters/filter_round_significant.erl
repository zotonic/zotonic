%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022-2023 Marc Worrell
%% @doc Round a value to the give significant digits. If you need to
%% round to a number of digits after the decimal point then use the
%% "round" filter.

%% Copyright 2022-2023 Marc Worrell
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

-module(filter_round_significant).
-moduledoc("
Round a number value to a number of significant digits. The significance defaults to two digits.

Example:


```django
{{ 1256|round_significant }}
{{ 1256|round_significant:1 }}
{{ 1256|round_significant:3 }}
```

Results in:


```django
1300
1000
1260
```

Floating point values are also rounded to a number of digits. For example if `n` is set to `1256.78` then:


```django
{{ n|round_significant }}
{{ n|round_significant:3 }}
{{ n|round_significant:5 }}
```

Results in:


```django
1300.0
1260.0
1256.8
```

Input values that are not a number are converted to a floating point value. If the conversion fails then `undefined` is returned.
").

-export([
    round_significant/2,
    round_significant/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

round_significant(Value, Context) ->
    round_significant(Value, 2, Context).

round_significant(undefined, _Significance, _Context) ->
    undefined;
round_significant(N, Significance, _Context) when is_integer(N) ->
    Digits = digits(N),
    if
        Digits =< Significance ->
            N;
        true ->
            Delta = Digits - Significance,
            Divider = math:pow(10, Delta),
            erlang:trunc(erlang:round(N / Divider) * Divider)
    end;
round_significant(N, Significance, _Context) ->
    try
        case z_convert:to_float(N) of
            undefined ->
                undefined;
            F ->
                Digits = digits(N),
                Delta = abs(Digits - Significance),
                Divider = math:pow(10, Delta),
                if
                    Digits =< Significance ->
                        erlang:round(F * Divider) / Divider;
                    true ->
                        erlang:round(F / Divider) * Divider
                end
        end
    catch
        _:_ -> undefined
    end.

digits(0) ->
    1;
digits(N) ->
    1 + erlang:trunc(math:log10(N)).

