%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc Supervisor for the file indexer

%% Copyright 2018 Marc Worrell
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

-module(zotonic_fileindexer_sup).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    init/1
]).

%% @doc API for starting the site supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the notifier gen_server(s) to be used.
init([]) ->
    {ok, {
        {one_for_one, 5, 10},
        [
            spec(zotonic_fileindexer_cache)
        ]
    }}.

spec(Name) ->
    {Name,
        {zotonic_fileindexer_cache, start_link, []},
        permanent, 5000, worker, [zotonic_fileindexer_cache]}.
