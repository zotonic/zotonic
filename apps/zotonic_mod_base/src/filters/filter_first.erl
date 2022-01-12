%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2022 Marc Worrell
%% @doc 'first' filter, return the first element in a string, list or tuple.

%% Copyright 2010-2022 Marc Worrell
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

-include_lib("zotonic_core/include/zotonic.hrl").

first(undefined, _Context) ->
    undefined;
first(#trans{} = Tr, Context) ->
    first(z_trans:lookup_fallback(Tr, Context), Context);
first(<<First/utf8, _/binary>>, _Context) ->
    First;
first(<<First, _/binary>>, _Context) ->
    First;
first(Tuple, _Context) when is_tuple(Tuple) ->
    erlang:element(1, Tuple);
first([ H | _ ], _Context) ->
    H;
first(Other, Context) ->
    case z_template_compiler_runtime:to_list(Other, Context) of
        [] -> <<>>;
        [H|_] -> H
    end.

first(undefined, _N, _Context) ->
    undefined;
first(_, N, _Context) when N < 1 ->
    undefined;
first(V, 1, Context) ->
    first(V, Context);
first(B, N, _Context) when is_binary(B) ->
    first_bin(B, N, <<>>);
first(Value, N, Context) ->
    first1(z_template_compiler_runtime:to_list(Value, Context), N, []).


first1([], _N, Acc) ->
    lists:reverse(Acc);
first1(_L, 0, Acc) ->
    lists:reverse(Acc);
first1([H|T], N, Acc) ->
    first1(T, N-1, [H|Acc]).


first_bin(_, 1, Acc) ->
    Acc;
first_bin(<<C/utf8, Rest/binary>>, N, Acc) ->
    first_bin(Rest, N-1, <<Acc/binary, C/utf8>>);
first_bin(<<C, Rest/binary>>, N, Acc) ->
    first_bin(Rest, N-1, <<Acc/binary, C>>).
