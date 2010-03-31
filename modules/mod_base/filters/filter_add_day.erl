%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'add_day' filter, add days to a date

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

-module(filter_add_day).
-export([add_day/2, add_day/3]).

add_day(undefined, _Context) ->
	undefined;
add_day(Date, Context) ->
	add_day(Date, 1, Context).

add_day(undefined, _N, _Context) ->
	undefined;
add_day(Date, 0, _Context) ->
	Date; 
add_day(Date, N, Context) when is_integer(N), N > 0 -> 
	add_day(z_datetime:next_day(Date), N-1, Context);
add_day(Date, N, Context) when is_integer(N), N < 0 -> 
	add_day(z_datetime:prev_day(Date), N+1, Context).
