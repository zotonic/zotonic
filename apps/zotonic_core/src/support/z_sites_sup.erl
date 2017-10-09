%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Supervisor for sites supervisors

%% Copyright 2017 Maas-Maarten Zeeman
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

-module(z_sites_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    start_site/1,
    stop_site/1
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc API for starting the sites dispatcher and manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_site(Site::atom()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_site(Site) ->
    supervisor:start_child(?MODULE, [Site]).

-spec stop_site(pid()) -> ok | {error, not_found}.
stop_site(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

-spec init(list()) -> {ok, {supervisor:sup_flags(), supervisor:child_spec()}}.
init([]) ->
    {ok, {
        #{
            strategy => simple_one_for_one,
            intensity => 5000,
            period => 1
        },
        [
            #{
                id => none,
                start => {z_site_sup, start_link, []},
                restart => temporary,
                type => supervisor,
                modules => [z_site_sup]
            }
        ]
    }}.


