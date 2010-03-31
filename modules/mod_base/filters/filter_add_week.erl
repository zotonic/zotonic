%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'add_week' filter, add one or more weeks to a date

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

-module(filter_add_week).
-export([add_week/2, add_week/3]).

add_week(undefined, _Context) ->
	undefined;
add_week(Date, Context) ->
	filter_add_day:add_day(Date, 7, Context).
add_week(undefined, _N, _Context) ->
	undefined;
add_week(Date, N, Context) ->
	filter_add_day:add_day(Date, 7*N, Context).

