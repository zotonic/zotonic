%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Link MQTT messaging into Zotonic modules and processes

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

-module(mod_mqtt).

-mod_title("MQTT").
-mod_description("MQTT messaging, connecting server and browser.").
-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_prio(1000).

-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

-export([
    pid_observe_module_activate/3,
    pid_observe_mqtt_subscribe/3,
    pid_observe_mqtt_unsubscribe/3,
    event/2
    ]).

-export([
    start_link/1,
    init/1
    ]).

-include("zotonic.hrl").
-include("emqtt/include/emqtt.hrl").


-export([
    'mqtt:/test'/3
]).

'mqtt:/test'(Message, Pid, Context) ->
    lager:debug("mqtt:/test received: ~p", [{Message, Pid, z_context:site(Context)}]),
    ok.

pid_observe_module_activate(MyPid, #module_activate{module=Module, pid=ModulePid}, Context) ->
    Exports = erlang:get_module_info(Module, exports),
    Fs = [ {z_convert:to_binary(F),F} || {F,3} <- Exports ],
    lists:foreach(fun(F) ->
                    maybe_subscribe(F, Module, ModulePid, MyPid, Context)
                  end,
                  Fs).

pid_observe_mqtt_subscribe(MyPid, #mqtt_subscribe{topic=Topic, mfa=MFA}, Context) ->
    subscribe_topic(Topic, MFA, undefined, MyPid, Context).

pid_observe_mqtt_unsubscribe(MyPid, #mqtt_unsubscribe{topic=Topic, mfa=MFA}, _Context) ->
    unsubscribe_topic(Topic, MFA, MyPid).

event(#postback_notify{message=Message}, Context) ->
    handle_event(z_convert:to_binary(Message),
                 z_convert:to_binary(z_context:get_q("topic", Context)),
                 z_context:get_q("msg", Context),
                 Context).


start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(_Args) ->
    {ok, {{one_for_one, 20, 10}, []}}.


%% ==========================================================================
%% Internal functions.
%% ==========================================================================


%% TODO: add access control checks to the topics!!!!
%% Idea: only admins can subscribe to wildcards

handle_event(<<"lastwill">>, <<>>, _Data, Context) ->
    WillId = z_context:get_q(<<"will_id">>, Context, ""),
    del_lastwill(Context#context.page_pid, WillId, Context),
    Context;
handle_event(<<"lastwill">>, Topic, Data, Context) ->
    Msg = msg_from_event(Topic, Data, Context),
    WillId = z_context:get_q(<<"will_id">>, Context, ""),
    add_lastwill(Context#context.page_pid, WillId, Msg, Context),
    Context;
handle_event(<<"publish">>, Topic, Data, Context) ->
    Msg = msg_from_event(Topic, Data, Context),
    z_mqtt:publish(Msg, Context),
    Context;
handle_event(<<"subscribe">>, Topic, _Data, Context) ->
    % Subscribe the page process, it will forward any messages
    z_mqtt:subscribe(Topic, Context#context.page_pid, Context),
    Context.


msg_from_event(Topic, Data, Context) ->
    #mqtt_msg{
        topic=Topic,
        payload=z_mqtt:make_postback_payload(Data, Context)
    }.


maybe_subscribe({<<"mqtt:", Topic/binary>>, F}, M, ModulePid, MyPid, Context) ->
    subscribe_topic(z_mqtt:maybe_context_topic(Topic, Context), {M,F}, ModulePid, MyPid, Context);
maybe_subscribe(_, _M, _Pid, _MyPid, _Context) ->
    nop.


% Add middleman process to this supervisor
% Middleman is linked to the module ModulePid.
% Middleman subscribes to emqtt with its own Pid
% Middleman calls M:F(Message, ModulePid, Context) when it receives a message.
subscribe_topic(Topic, MFA, ModulePid, MyPid, Context) ->
    Sub = {
        {Topic,MFA},
        {z_mqtt_module_subscriber, start_link, [{Topic, MFA, ModulePid, z_context:site(Context)}]},
        temporary, 5000, worker, [z_mqtt_module_subscriber]
    },
    lager:debug("MQTT subscribe ~p to ~p", [MFA, Topic]),
    case supervisor:start_child(MyPid, Sub) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

unsubscribe_topic(Topic, MFA, MyPid) ->
    supervisor:delete_child(MyPid, {Topic,MFA}). 


add_lastwill(Pid, WillId, Msg, Context) when is_pid(Pid) ->
    Pid = Context#context.page_pid,
    case is_pid(Pid) of
        true ->
            WillId1 = z_convert:to_binary(WillId), 
            case start_lastwill_proc(Pid, WillId1, Msg, Context) of
                {ok, _} ->
                    ok;
                {error, {already_started, ChildPid}} ->
                    z_mqtt_lastwill:add(ChildPid, WillId1, Msg)
            end;
        false ->
            ok
    end;
add_lastwill(_Pid, _WillId, Msg, Context) ->
    z_mqtt:publish(Msg, Context).


del_lastwill(Pid, WillId, Context) when is_pid(Pid) ->
    WillId1 = z_convert:to_binary(WillId), 
    case start_lastwill_proc(Pid, undefined, undefined, Context) of
                {ok, _} ->
                    ok;
                {error, {already_started, ChildPid}} ->
                    z_mqtt_lastwill:del(ChildPid, WillId1)
    end;
del_lastwill(_Pid, _WillId, _Context) ->
    ok.


start_lastwill_proc(Pid, WillId, Msg, Context) ->
    Sub = {
        {lastwill, Pid},
        {z_mqtt_lastwill, start_link, [{Pid, WillId, Msg, z_context:site(Context)}]},
        transient, 5000, worker, [z_mqtt_lastwill]
    },
    lager:debug("MQTT lastwill ~p to ~p", [Pid, Msg#mqtt_msg.topic]),
    supervisor:start_child(z_module_manager:whereis(?MODULE, Context), Sub).

