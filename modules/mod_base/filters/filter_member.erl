%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'member' filter, test if an element is part of a list

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

-module(filter_member).
-export([member/3]).


member(_S, undefined, _Context) ->
    false;
member(S, [H|_] = L, _Context) when is_list(S) andalso is_binary(H) ->
    lists:member(list_to_binary(S), L);
member(S, [H|_] = L, _Context) when is_list(S) andalso is_integer(H) ->
    try
        lists:member(list_to_integer(S), L)
    catch
        _:_ -> false
    end;
member(S, L, _Context) when is_list(L) ->
    lists:member(S, L);
member(S, Value, Context) ->
	case erlydtl_runtime:to_list(Value, Context) of
		L when is_list(L) -> member(S, L, Context);
		_ -> undefined
	end.


