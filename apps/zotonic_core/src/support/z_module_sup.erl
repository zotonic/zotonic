%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2025 Marc Worrell
%% @doc Supervisor for a site's modules
%% @end

%% Copyright 2017-2025 Marc Worrell
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
    stop_module/2,
    gc/1
]).

%% supervisor callbacks
-export([init/1]).

-include_lib("../../include/zotonic.hrl").

%% @doc API for starting the sites dispatcher and manager
-spec start_link(atom()) -> gen_server:start_ret().
start_link(Site) ->
    Name = z_utils:name_for_site(z_module_sup, Site),
    supervisor:start_link({local, Name}, ?MODULE, []).

-spec start_module(atom(), supervisor:child_spec(), Site::atom()) ->
        supervisor:startchild_ret() | {error, term()}.
start_module(Application, ChildSpec, Site) ->
    case application:ensure_all_started(Application, temporary) of
        {ok, _Started} ->
            Name = z_utils:name_for_site(z_module_sup, Site),
            supervisor:start_child(Name, ChildSpec);
        {error, _} = Error ->
            Error
    end.

-spec stop_module(ChildId, Site) -> ok | {error, not_found} when
    ChildId :: term(),
    Site :: atom().
stop_module(ChildId, Site) ->
    Name = z_utils:name_for_site(z_module_sup, Site),
    supervisor:terminate_child(Name, ChildId).

-spec gc( atom() ) -> any().
gc(Site) ->
    Name = z_utils:name_for_site(z_module_sup, Site),
    erlang:garbage_collect(whereis(Name)).

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


