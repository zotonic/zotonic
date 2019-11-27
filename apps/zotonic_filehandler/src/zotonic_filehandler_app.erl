%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Filehandler - handles changes on files, starts recompiles

%% Copyright 2019 Marc Worrell
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

-module(zotonic_filehandler_app).

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
]).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:ensure_all_started(zotonic_filehandler).

start(_StartType, _StartArgs) ->
    ensure_job_queues(),
    zotonic_filehandler_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


%% @doc Ensure all job queues
ensure_job_queues() ->
    ensure_job_queue(
        zotonic_filehandler_single_job,
        [
            {regulators, [
                {counter, [
                    {limit, 1},
                    {modifiers, [{cpu, 1}]}
                ]}
            ]}
        ]).

ensure_job_queue(Name, Options) ->
    case jobs:queue_info(Name) of
        undefined -> jobs:add_queue(Name, Options);
        {queue, _Props} -> ok
    end.
