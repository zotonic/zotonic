%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Randomize the order of elements in a list

%% Copyright 2011 Marc Worrell
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

-module(filter_randomize).
-export([randomize/2]).


randomize([], _Context) ->
    undefined;
randomize(undefined, _Context) ->
    undefined;
randomize(In, _Context) when is_list(In) ->
    z_utils:randomize(In);
randomize(In, Context) ->
    randomize(erlydtl_runtime:to_list(In, Context), Context).
