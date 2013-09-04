%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Manage last will messages for a process. Max 100 wills are kept.

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

-module(z_mqtt_lastwill).

-behaviour(gen_server).

-record(state, {
        wills = [],
        site
    }).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([add/3, del/2]).

-define(MAX_WILLS, 100).


add(WillPid, WillId, Msg) ->
    gen_server:cast(WillPid, {add, WillId, Msg}).

del(WillPid, WillId) ->
    gen_server:cast(WillPid, {del, WillId}).


start_link({_WillPid, _WillId, _Msg, _Site} = Args) ->
    gen_server:start_link(?MODULE, Args, []).


init({Pid, undefined, undefined, Site}) ->
    erlang:monitor(process, Pid),
    {ok, #state{
        wills=[],
        site=Site
    }};
init({Pid, WillId, Msg, Site}) ->
    erlang:monitor(process, Pid),
    {ok, #state{
        wills=[{WillId,Msg}],
        site=Site
    }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_cast({add, WillId, Msg}, State) ->
    State1 = State#state{wills=prune([{WillId,Msg} | maybe_delete_will(WillId, State#state.wills)])},
    {noreply, State1};
handle_cast({del, WillId}, State) ->
    State1 = State#state{wills=maybe_delete_will(WillId, State#state.wills)},
    {noreply, State1};
handle_cast(_Msg, State) ->
    {reply, State}.

handle_info({'DOWN', _MRef, _, _, _}, State) ->
    Context = z_context:new(State#state.site),
    lists:foreach(fun({_,Msg}) ->
                        z_mqtt:publish(Msg, Context)
                  end,
                  State#state.wills), 
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


maybe_delete_will(<<>>, Wills) ->
    Wills;
maybe_delete_will(Id, Wills) ->
    lists:keydelete(Id, 1, Wills). 

prune(Wills) ->
    case length(Wills) > ?MAX_WILLS of
        true -> lists:sublist(Wills, ?MAX_WILLS);
        false ->  Wills
    end.
