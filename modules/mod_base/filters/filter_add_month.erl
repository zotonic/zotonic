%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'add_month' filter, add one or more months to a date

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

-module(filter_add_month).
-export([add_month/2, add_month/3]).

add_month(undefined, _Context) ->
	undefined;
add_month(Date, Context) ->
	add_month(Date, 1, Context).

add_month(undefined, _N, _Context) ->
	undefined;
add_month(Date, 0, _Context) ->
	Date; 
add_month(Date, N, Context) when is_integer(N), N > 0 -> 
	add_month(z_datetime:next_month(Date), N-1, Context);
add_month(Date, N, Context) when is_integer(N), N < 0 -> 
	add_month(z_datetime:prev_month(Date), N+1, Context).

