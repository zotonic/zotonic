%% @author Andreas Stenius <kaos@astekk.se>
%% @copyright 2013 Andreas Stenius
%% @doc Returns a list of tokens from input string, separated by the characters in the argument.

%% Copyright 2013 Andreas Stenius
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

-module(filter_tokens).
-export([tokens/3]).


tokens(String, Sep, _Context) ->
    string:tokens(z_convert:to_list(String), z_convert:to_list(Sep)).
