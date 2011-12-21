%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc Simple in-memory k/v store for zynamo. Every zynamo node has this service enabled.

%% Copyright 2010-2011 Marc Worrell
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

-module(zynamo_kv).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
]).

-include_lib("zynamo.hrl").

-record(state, {data, handoff}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> 
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init(list()) -> {ok, undefined}.
init(_Args) ->
    zynamo_manager:set_service(zynamo, kv, self(), undefined),
    {ok, #state{
            data=ets:new(zynamo_kv, [set,protected]),
            handoff=ets:new(zynamo_handoff, [set,protected])
    }}.


handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(#zynamo_service_command{
                ref=Ref,
                is_primary=_IsPrimary,
                from=From,
                handoff=Handoff,
                command=Command
            }, State) ->
    #zynamo_command{command=Cmd, key=Key, value=Value, version=Version} = Command,
    Reply = case Cmd of
                put ->
                    % TODO: handle handoffs
                    % TODO: check iff the version is a follow-up from the one we have.
                    ets:insert(State#state.data, [{Key, {Version,Value}}]),
                    ok;
                get ->
                    case ets:lookup(State#state.data, Key) of
                        [] -> {ok, not_found, undefined};
                        [{_Key,{Vers,Val}}] -> {ok, Vers, Val}
                    end
            end,
    case is_pid(From) of
        true -> From ! {ok, node(), Ref, Reply};
        false -> nop
    end,
    {noreply, State};
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, _NewVersion, State) ->
    {ok, State}.
