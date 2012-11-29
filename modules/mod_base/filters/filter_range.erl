%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%% @doc 'range' filter, create a list with integers.

%% Copyright 2012 Maas-Maarten Zeeman
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

-module(filter_range).
-export([range/3, range/4]).

range(Start, End, _Context) when Start =:= undefined; End =:= undefined->
    [];
range(Start, End, Context) ->
    range(Start, End, 1, Context).

range(Start, End, Step, _Context) ->
	range1(z_convert:to_integer(Start), z_convert:to_integer(End), z_convert:to_integer(Step)).

range1(Start, End, Step) 
	when not is_integer(Start); 
		 not is_integer(End); 
		 not is_integer(Step);
		 Step =:= 0 ->
	[];
range1(Start, End, Step) when End >= Start, Step > 0 ->
    lists:seq(Start, End, Step);
range1(Start, End, Step) when End =< Start, Step < 0 ->
    lists:reverse(lists:seq(End, Start, 0-Step));
range1(_Start, _End, _Step) ->
	[].
