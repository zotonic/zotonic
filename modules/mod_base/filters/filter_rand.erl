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
