%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-27

%% @doc Name of the statistics

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

-module(filter_statsname).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-export([statsname/2]).


statsname({_, db, requests}, _Context) ->
    "Database - nr. of requests";
statsname({_, db, duration}, _Context) ->
    "Database - query time";
statsname({_, webzmachine, duration}, _Context) ->
    "Web server - request time";
statsname({_, webzmachine, requests}, _Context) ->
    "Web server - nr. of requests";
statsname({_, webzmachine, in}, _Context) ->
    "Web server - Kb in";
statsname({_, webzmachine, out}, _Context) ->
    "Web server - Kb out";
statsname(_K, _) ->
    ?DEBUG(_K),
    "?".

