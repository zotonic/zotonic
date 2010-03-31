%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'in_future' filter, test if a date is in the future.

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

-module(filter_in_future).
-export([in_future/2]).


in_future({_,_,_} = Date, _Context) ->
	{LocalDate, _} = erlang:localtime(),
	Date > LocalDate;
in_future({{_,_,_}, {_,_,_}} = DateTime, _Context) ->
	DateTime > erlang:localtime();
in_future(_, _Context) ->
	undefined.
