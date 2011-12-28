%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-27

%% @doc Value of the statistics

%% Copyright 2011 Arjan Scherpenisse
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

-module(filter_statsvalue).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([statsvalue/3]).

statsvalue(undefined, _, _Context) ->
    "n/a";

statsvalue(V, {_, _, requests}, _Context) ->
    io_lib:format("~w rq", [V]);
statsvalue(V, {_, _, duration}, _Context) ->
    io_lib:format("~.1f ms", [V/10.0]);
statsvalue(V, {_, webzmachine, in}, _Context) ->
    io_lib:format("~.2f Kb", [V/1000.0]);
statsvalue(V, {_, webzmachine, out}, _Context) ->
    io_lib:format("~.2f Kb", [V/1000.0]);
statsvalue(_V, _K, _) ->
    ?DEBUG(_K),
    "?".

