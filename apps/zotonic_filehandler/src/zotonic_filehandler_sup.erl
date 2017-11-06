%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Supervisor for the file handler

%% Copyright 2017 Marc Worrell
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

-module(zotonic_filehandler_sup).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = children(),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.


children() ->
    [
        {zotonic_filehandler_terminal,
          {zotonic_filehandler_terminal, start_link, []},
          permanent, 5000, worker, [zotonic_filehandler_terminal]},
        {zotonic_filehandler_handler,
          {zotonic_filehandler_handler, start_link, []},
          permanent, 5000, worker, [zotonic_filehandler_handler]}
    ].

