%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Supervisor for a site's modules

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

-module(z_module_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([
    start_link/1,
    start_module/3,
    stop_module/2
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc API for starting the sites dispatcher and manager
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Site) ->
    Name = z_utils:name_for_site(z_module_sup, Site),
    supervisor:start_link({local, Name}, ?MODULE, []).

-spec start_module(atom(), supervisor:child_spec(), Site::atom()) ->
        {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_module(Application, ChildSpec, Site) ->
    case application:ensure_all_started(Application) of
        {ok, _Started} ->
            Name = z_utils:name_for_site(z_module_sup, Site),
            supervisor:start_child(Name, ChildSpec);
        {error, _} = Error ->
            Error
    end.

-spec stop_module(supervisor:child_id(), atom()) -> ok | {error, not_found}.
stop_module(ChildId, Site) ->
    Name = z_utils:name_for_site(z_module_sup, Site),
    supervisor:terminate_child(Name, ChildId).

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {
        #{
            strategy => one_for_one,
            intensity => 100,
            period => 5
        },
        [
        ]
    }}.


