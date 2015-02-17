%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Filter a {trans, [...]} record, only pass the filled fields.

%% Copyright 2015 Marc Worrell
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

-module(filter_trans_filter_filled).

-export([
    trans_filter_filled/2,
    trans_filter_filled/3
]).

trans_filter_filled(Var, _Context) ->
    filled_in(Var).

trans_filter_filled(Var, Key, Context) ->
    filled_in(erlydtl_runtime:find_value(Key, Var, Context)).

filled_in({trans, Tr}) ->
    Tr1 = lists:filter(fun({_,T}) -> is_filled_in(T) end, Tr),
    {trans, Tr1};
filled_in(A) ->
    io:format("~p~n", [A]),
    A.

is_filled_in(<<>>) ->
    false;
is_filled_in(<<C, T/binary>>) when C =:= 32, C=:= 9 ->
    is_filled_in(T);
is_filled_in(<<"<p></p>">>) ->
    false;
is_filled_in(_) ->
    true.
