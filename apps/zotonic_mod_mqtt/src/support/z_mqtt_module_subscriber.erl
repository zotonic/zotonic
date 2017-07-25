%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Proxy process linking a mqtt subscription to a module mqtt-callback function.

%% Copyright 2013 Marc Worrell
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

-module(z_mqtt_module_subscriber).

-behaviour(gen_server).

-record(state, {
        topic,
        qos,
        mfa,
        pid,
        site
    }).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

start_link({_Topic, _Qos, _MFA, _Pid, _Site} = Args) ->
    gen_server:start_link(?MODULE, Args, []).


init({Topic, Qos, MFA, Pid, Site}) ->
    erlang:process_flag(trap_exit, true),
    maybe_link(Pid),
    ok = emqtt_router:subscribe({Topic,Qos}, self()),
    {ok, #state{
        topic=Topic,
        qos=Qos,
        mfa=MFA,
        pid=Pid,
        site=Site
    }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_cast(_Msg, State) ->
    {reply, State}.

handle_info({route, Message}, State) ->
    case State#state.mfa of
        {M,F} ->
            case State#state.pid of
                undefined -> _ = M:F(Message, z_context:new(State#state.site));
                Pid -> _ = M:F(Message, Pid, z_context:new(State#state.site))
            end;
        {M,F,A} ->
            _ = M:F(Message, A, z_context:new(State#state.site))
    end,
    {noreply, State};
handle_info({'EXIT', _From, _Reason}, State) ->
    %% The module for which we handle the mqtt subscription has either crashed
    %% or stopped. Stop normally we don't have to route messages anymore.
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



maybe_link(undefined) ->
    true;
maybe_link(Pid) ->
    erlang:link(Pid).
