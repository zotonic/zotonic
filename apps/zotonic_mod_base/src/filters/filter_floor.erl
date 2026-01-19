%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Floor a value to integer

%% Copyright 2026 Maas-Maarten Zeeman
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

-module(filter_floor).
-moduledoc("
Round down a floating point value.

Example:


```django
{{ 3.5 | floor }}
```

Results in the integer value `3`.
").

-export([floor/2]).

floor(undefined, _Context) ->
    undefined;
floor(N, _Context) when is_integer(N) ->
    N;
floor(N, _Context) ->
    try
        F = z_convert:to_float(N),
        erlang:floor(F)
    catch
        _:_ -> undefined
    end.

