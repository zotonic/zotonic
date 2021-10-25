%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2021 Maas-Maarten Zeeman
%% @doc 'add_hour' filter, add hours to a date

%% Copyright 2021 Maas-Maarten Zeeman
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

-module(filter_add_hour).
-export([add_hour/2, add_hour/3]).

add_hour(undefined, _Context) ->
	undefined;
add_hour(Date, Context) ->
	add_hour(Date, 1, Context).

add_hour(undefined, _N, _Context) ->
	undefined;
add_hour(Date, 0, _Context) ->
	Date;
add_hour(Date, N, Context) when is_integer(N), N > 0 ->
	add_hour(z_datetime:next_hour(Date), N-1, Context);
add_hour(Date, N, Context) when is_integer(N), N < 0 ->
	add_hour(z_datetime:prev_hour(Date), N+1, Context).
