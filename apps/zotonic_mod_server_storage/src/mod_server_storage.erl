%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Server side sessions and data storage.

%% Copyright 2020 Marc Worrell
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

-module(mod_server_storage).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Server Session Storage").
-mod_description("Server side data storage in sessions.").
-mod_prio(500).
-mod_depends([base]).
-mod_provides([server_storage]).

-behaviour(supervisor).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_auth_ping/2,
    start_session/2,
    start_link/1,
    init/1
    ]).


%% @doc Periodic ping for the session, done by the client.
-spec observe_auth_ping(#auth_ping{}, z:context()) -> ok.
observe_auth_ping(#auth_ping{}, Context) ->
    m_server_storage:ping(Context),
    ok.

%% @doc Start the session with the given Id
-spec start_session( binary(), z:context() ) -> {ok, pid()} | {error, term()}.
start_session(SessionId, Context) when is_binary(SessionId) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    case supervisor:start_child(Name, [SessionId, z_context:new(Context) ]) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> Pid;
        {error, _} = Error -> Error
    end.

%%% ------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%% ------------------------------------------------------------------------------------

start_link(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Name = z_utils:name_for_site(?MODULE, Context),
    supervisor:start_link({local, Name}, ?MODULE, []).

init([]) ->
    Type = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },
    ChildSpecs = [
        #{
            id => session_process,
            start => {z_server_storage, start_link, []},
            type => worker,
            restart => temporary
        }
    ],
    {ok, {Type, ChildSpecs}}.
