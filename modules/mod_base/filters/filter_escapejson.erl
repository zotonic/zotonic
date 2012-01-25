%% @author François Cardinaux
%% @copyright 2011 François Cardinaux
%% @doc Zotonic filter to escape JSON strings as specified on http://www.json.org/. 
%% Inspired by Marc Worrell's filter_escapejs module 

%% Copyright 2011 Zotonic
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

-module(filter_escapejson).
-export([escapejson/2]).

%% @doc Escape a string for JSON
escapejson(undefined, _Context) ->
    <<>>;
escapejson(Input, _Context) when is_binary(Input) ->
    z_utils:json_escape(Input);
escapejson(Input, _Context) when is_list(Input) ->
    z_utils:json_escape(Input).

