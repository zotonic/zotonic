%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'negate' filter

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

-module(filter_negate).
-export([negate/2]).

negate(Input, _Context) when is_list(Input); is_binary(Input) ->
    case z_convert:to_integer(Input) of
        undefined -> undefined;
        N -> integer_to_list(0 - N)
    end;
negate(Input, _Context) when is_float(Input) ->
    0.0 - Input;
negate(Input, _Context) when is_integer(Input) ->
    0 - Input.
