%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'first' filter, return the first element in a string or list

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

-module(filter_first).
-export([
    first/2,
    first/3
]).

-include("zotonic.hrl").

first(undefined, _Context) ->
    undefined;
first(<<First, _/binary>>, _Context) ->
    First;
first(Other, Context) ->
    case erlydtl_runtime:to_list(Other, Context) of
        [] -> <<>>;
        [H|_] -> H
    end.

first(undefined, _Length, _Context) ->
    undefined;
first(Value, Length, Context) ->
    first1(erlydtl_runtime:to_list(Value, Context), Length, []).


first1([], _N, Acc) ->
    lists:reverse(Acc);
first1(_L, 0, Acc) ->
    lists:reverse(Acc);
first1([H|T], N, Acc) ->
    first1(T, N-1, [H|Acc]).

