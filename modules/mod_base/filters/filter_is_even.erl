%% @author Michael Connors
%% @copyright 2011 Michael Connors
%% @doc Test if an integer value is even.

%% Copyright 2011 Michael Connors
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

-module(filter_is_even).
-export([is_even/2]).

is_even(Number, _Context) when is_integer(Number) ->
    case Number rem 2 of
        0 -> true;
        _ -> false
    end;
is_even(_Number, _Context) ->
    false.
