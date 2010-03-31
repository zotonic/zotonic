%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'length_is' filter, test the length of a list or string

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

-module(filter_length_is).
-export([length_is/3]).


length_is(undefined, _Number, _Context) ->
    undefined;
length_is(Input, Number, Context) when is_list(Input), is_integer(Number) ->
    length_is(Input, integer_to_list(Number), Context);
length_is(Input, Number, Context) when is_list(Input), is_list(Number) ->
    filter_length:length(Input, Context) =:= Number;
length_is(_Input, Number, _Context) ->
    1 =:= z_convert:to_integer(Number).
