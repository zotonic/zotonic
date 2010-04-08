%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'modulo' filter, return remainder of division

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

-module(filter_modulo).
-export([modulo/3]).

modulo(undefined, _Number, _Context) ->
    undefined;
modulo(Input, Number, Context) when is_binary(Input) ->
    z_convert:to_binary(modulo(binary_to_list(Input), Number, Context));
modulo(Input, Number, Context) when is_list(Input) ->
    z_convert:to_list(modulo(list_to_integer(Input), Number, Context));
modulo(Input, Number, _Context) when is_integer(Input) ->
    case z_convert:to_integer(Number) of
        undefined -> undefined;
        0 -> undefined;
        N -> Input rem N
    end;
modulo(Input, Number, _Context) when is_float(Input) ->
    case z_convert:to_integer(Number) of
        undefined -> undefined;
        0 -> undefined;
        N -> z_convert:to_integer(Input) rem N
    end.
