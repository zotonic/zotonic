%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc 'replace' filter, replace substrings matching a regular expression

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

-module(filter_replace).
-export([replace/3]).


replace(undefined, _, _Context) ->
    undefined;
replace(In, [A,B], _Context) when is_binary(A) orelse is_list(A) ->
    iolist_to_binary(re:replace(In, A, B, [global]));
replace(In, A, _Context) ->
    iolist_to_binary(re:replace(In, A, [], [global])).
