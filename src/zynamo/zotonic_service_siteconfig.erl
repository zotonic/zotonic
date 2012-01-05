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

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(#zynamo_service_command{
                command=Command,
                handoff=Handoff
            }=SC, State) ->
    Reply = case Command#zynamo_command.command of
                get ->
                    do_get(Command, State);
                list ->
                    do_list(Command, State);
                Upd when Upd =:= put; Upd =:= delete ->
                    do_update(Command, Handoff, State);
                _Other ->
                    {error, operation_not_supported}
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
        {_, _} = Key -> m_config:zynamo_get(State#state.host, Key);
        _ -> {error, unknown_key_format}
    end.
    
do_update(Command, Handoff, #state{host=Host} = State) ->
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

do_list(Comand, State) ->
    {ok, '$end_of_table'}.



do_handoff_check(Node, State) ->
    Host = State#state.host,
    case m_handoff:next_handoff(Node, Host, config, m_config) of
        {ok, done} ->
            {reply, {ok, done}, State};
        {ok, Id, Key, Version, delete} ->
            Command = #zynamo_command{
                command=delete,
                key=Key,
                version=Version,
                handoff_ref=Id
            },
            {reply, {ok, Command}, State};
        {ok, Id, Key, Version, put} ->
            case m_config:zynamo_get(Host, Key) of
                {ok, gone, _} ->
                    m_handoff:delete_handoff(Host, Id),
                    do_handoff_check(Node, State);
                    
                {ok, StoredVersion, Data} ->
                    case zynamo_version:is_equal(StoredVersion, Version) of
                        true ->
                            Command = #zynamo_command{
                                command=put,
                                key=Key,
                                version=Version,
                                value=Data,
                                handoff_ref=Id
                            },
                            {ok, Command};
                        false ->
                            m_handoff:delete_handoff(Host, Id),
                            do_handoff_check(Node, State)
                    end;
                
                {error, _} ->
                    m_handoff:delete_handoff(Host, Id),
                    do_handoff_check(Node, State)
            end
        
    end.


do_handoff_done(Node, Command, State) ->
    #zynamo_command{
        handoff_ref=Id
    } = Command,
    m_handoff:delete_handoff(State#state.host, Id).

