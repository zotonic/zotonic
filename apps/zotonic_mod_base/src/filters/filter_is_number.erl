%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell
%% @doc is_number filter, test if a value is a number

%% Copyright 2016 Marc Worrell
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

-module(filter_is_number).
-moduledoc("
Test if a value is a number (integer or floating point):


```django
{% if 1|is_number %}Yes, this is a number{% endif %}
```
").
-export([is_number/2]).

is_number(V, _Context) when is_number(V) -> true;
is_number(_, _Context) -> false.

