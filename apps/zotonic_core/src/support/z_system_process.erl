%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Dynamic supervisor for shared Zotonic system processes.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_system_process).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(supervisor).

-export([
    start_link/0,
    start_child/1
]).

-export([
    init/1
]).

-spec start_link() -> supervisor:startlink_ret().
%% @doc Start the dynamic supervisor for shared system processes.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(ChildSpec) -> supervisor:startchild_ret() when
    ChildSpec :: supervisor:child_spec().
%% @doc Start a child process from a complete supervisor child spec.
start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @doc Initialize an empty one_for_one dynamic supervisor.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1000,
        period => 10
    },
    {ok, {SupFlags, []}}.
