%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%% @doc Supervisor for the syslog socket process

%% Copyright 2021 Marc Worrell
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

-module(z_syslog_sup).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

%% @doc API for starting the site supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the notifier gen_server(s) to be used.
init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    Children = [
        #{
            id => z_syslog_logger,
            start => {z_syslog_logger, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.
