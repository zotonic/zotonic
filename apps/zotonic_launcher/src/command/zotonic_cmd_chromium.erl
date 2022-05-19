%% @author William Fank Thomé <williamthome@hotmail.com>
%% @copyright 2022 Marc Worrell
%% @doc Chromium CLI command

%% Copyright 2022 Marc Worrell
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

-module(zotonic_cmd_chromium).
-author("William Fank Thomé <williamthome@hotmail.com>").

%% API
-export([
    info/0,
    usage/0,
    run/1
]).

info() ->
    zotonic_cmd_chrome:info("Chromium").

usage() ->
    zotonic_cmd_chrome:usage("chromium").

run(Args) ->
    zotonic_cmd_chrome:run(chromium, Args).
