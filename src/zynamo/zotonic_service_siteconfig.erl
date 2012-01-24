%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc Zynamo service for site configuration, works with m_config

%% Copyright 2012 Marc Worrell
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

-module(zotonic_service_siteconfig).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
]).

-include_lib("deps/zynamo/include/zynamo.hrl").
-include_lib("zotonic.hrl").

-record(state, {host}).

% Minimum seconds between full config syncs. 
% At least 10 minutes between full syncs.
-define(SYNC_PERIOD, 600).


%%====================================================================
%% API
%%====================================================================

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init(list()) -> {ok, undefined}.
init(Args) ->
    {host, Host} = proplists:lookup(host, Args),
    zynamo_manager:set_service(Host, config, self(), undefined),
    {ok, #state{host=Host}}.


handle_call({handoff_check, Node}, _From, State) ->
    do_handoff_check(Node, State);

handle_call({is_sync_wanted, startup}, _From, State) ->
    {reply, {ok, true}, State};
handle_call({is_sync_wanted, SecsAgo}, _From, State) ->
    {reply, {ok, SecsAgo >= ?SYNC_PERIOD}, State};

handle_call(sync_n, _From, State) ->
    {reply, {ok, all}, State};

handle_call(Message, _From, State) ->
    {reply, {error, {unknown_call, Message}}, State}.

handle_cast(#zynamo_service_command{
                command=Command,
                handoff=Handoff
            }=SC, State) ->
    Reply = case Command#zynamo_command.command of
                get ->
                    do_get(Command, State);
                list ->
                    #zynamo_service_result{
                        value=m_config:zynamo_list(State#state.host, Command#zynamo_command.value)
                    };
                list_hash ->
                    List = m_config:zynamo_list(State#state.host, Command#zynamo_command.value),
                    #zynamo_service_result{
                        value=zynamo_hash:hash_sync_list(List)
                    };
                Upd when Upd =:= put; Upd =:= delete ->
                    do_update(Command, Handoff, State);
                _Other ->
                    #zynamo_service_result{status=operation_not_supported}
            end,
    zynamo_request:reply(Reply, SC),
    {noreply, State};

handle_cast({handoff_done, Node, Command}, State) ->
    do_handoff_done(Node, Command, State),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, _NewVersion, State) ->
    {ok, State}.


%% ====================================================================
%% internal support routines
%% ====================================================================


do_get(Command, State) ->
    case Command#zynamo_command.key of
        {_, _} = Key -> 
            m_config:zynamo_get(State#state.host, Key);
        _ -> #zynamo_service_result{status=unknown_key_format}
    end.
    
do_update(Command, Handoff, #state{host=Host}) ->
    #zynamo_command{command=Cmd, key=Key, version=Version, value=Value} = Command,
    case Key of
        {_, _} -> 
            Result = case Cmd of
                put -> m_config:zynamo_put(Host, Key, Version, Value);
                delete -> m_config:zynamo_delete(Host, Key, Version)
            end,
            m_handoff:handle_handoff(Handoff, 
                                     Host, config, m_config, 
                                     Key, Version, Cmd),
            Result;
        _ -> 
            {error, unknown_key_format}
    end.

do_handoff_check(Node, State) ->
    Result = m_handoff:handoff_check(Node, State#state.host, config, m_config, fun m_config:zynamo_get/2),
    {reply, Result, State}.

do_handoff_done(_Node, Command, State) ->
    #zynamo_command{
        handoff_ref=Id
    } = Command,
    m_handoff:delete_handoff(State#state.host, Id).

