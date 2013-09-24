%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell
%% @doc Convert a value to a binary string.

%% Copyright 2013 Marc Worrell
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

-module(filter_stringify).
-export([stringify/2]).

stringify([], _Context) ->
    <<>>;
stringify(N, _Context) when is_integer(N) ->
	z_convert:to_binary(N); 
stringify(L, Context) when is_list(L) ->
    iolist_to_binary(stringify_1(L, Context));
stringify(X, Context) ->
	stringify_1(X, Context).

%% Recursive stringify - safe to be applied in lists (where integers are assumed to be characters)
stringify_1(undefined, _Context) ->
    <<>>;
stringify_1({{_Y,_M,_D},{_H,_I,_S}} = Date, Context) ->
	erlydtl_dateformat:format(Date, "Y-m-d H:i:s", Context);
stringify_1(B, _Context) when is_binary(B) ->
    B;
stringify_1(C, _Context) when C >= 0, C =< 255 ->
    C;
stringify_1({trans, Tr}, Context) ->
    z_trans:lookup_fallback(Tr, Context);
stringify_1(N, _Context) when is_integer(N); is_float(N); is_atom(N) ->
    z_convert:to_binary(N);
stringify_1(L, Context) when is_list(L) ->
    [ stringify_1(X, Context) || X <- L ].

