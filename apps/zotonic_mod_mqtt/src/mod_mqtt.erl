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
    observe_z_mqtt_cmd/2,

    observe_rsc_update_done/2,
    observe_media_replace_file/2,
    observe_edge_delete/2,
    observe_edge_insert/2,
    observe_edge_update/2,

    observe_action_event_type/2
    ]).

-export([
    start_link/1,
    init/1
    ]).

-record(z_mqtt_cmd, {
        cmd,
        topic,
        payload,
        extra
    }).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").


-export([
    'mqtt:~site/test'/3
]).

'mqtt:~site/test'(Message, Pid, Context) ->
    lager:debug("mqtt:~~site/test received: ~p", [{Message, Pid, z_context:site(Context)}]),
    ok.

pid_observe_module_activate(MyPid, #module_activate{module=Module, pid=ModulePid}, Context) ->
    Exports = erlang:get_module_info(Module, exports),
    Fs = [ {z_convert:to_binary(F),F} || {F,3} <- Exports ],
    lists:foreach(fun(F) ->
                    maybe_subscribe(F, Module, ModulePid, MyPid, Context)
                  end,
                  Fs).

pid_observe_mqtt_subscribe(MyPid, #mqtt_subscribe{topic=Topic, qos=Qos, mfa=MFA}, Context) ->
    subscribe_topic(Topic, Qos, MFA, undefined, MyPid, Context).

pid_observe_mqtt_unsubscribe(MyPid, #mqtt_unsubscribe{topic=Topic, mfa=MFA}, _Context) ->
    unsubscribe_topic(Topic, MFA, MyPid).

observe_z_mqtt_cmd(#z_mqtt_cmd{topic=Topic} = Cmd, Context) ->
    Cmd1 = Cmd#z_mqtt_cmd{topic=local_topic(Topic)},
    handle_cmd(Cmd1, Context).

local_topic(<<"/", Topic/binary>>) -> Topic;
local_topic(Topic) -> Topic.


observe_rsc_update_done(#rsc_update_done{id=Id} = UpdateDone, Context) ->
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(Id))/binary>>,
        UpdateDone#rsc_update_done{pre_props=[], post_props=[]},
        Context).

observe_media_replace_file(#media_replace_file{id=Id} = MediaReplace, Context) ->
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(Id))/binary, "/medium">>,
        MediaReplace#media_replace_file{medium=[]},
        Context).

observe_edge_delete(#edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeDelete, Context) ->
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
        EdgeDelete,
        Context),
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
        EdgeDelete,
        Context).

observe_edge_insert(#edge_insert{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeInsert, Context) ->
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
        EdgeInsert,
        Context),
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
        EdgeInsert,
        Context).

observe_edge_update(#edge_update{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeUpdate, Context) ->
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
        EdgeUpdate,
        Context),
    z_mqtt:publish(
        <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
        EdgeUpdate,
        Context).

%% @doc Handle the <tt>{live ...}</tt> event type.
observe_action_event_type(#action_event_type{event={mqtt, _Args}} = Ev, Context) ->
    scomp_mqtt_live:event_type_mqtt(Ev, Context);
observe_action_event_type(_, _Context) ->
    undefined.

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    lager:md([
        {site, z_context:site(Context)},
        {module, ?MODULE}
      ]),
    {ok, {{one_for_one, 20, 10}, []}}.


%% ==========================================================================
%% Internal functions.
%% ==========================================================================


% handle_cmd(#z_mqtt_cmd{cmd= <<"lastwill">>, topic=undefined, extra=WillId}, Context) ->
%     del_lastwill(Context#context.page_pid, WillId, Context);
% handle_cmd(#z_mqtt_cmd{cmd= <<"lastwill">>, topic=Topic, payload=Data, extra=WillId}, Context) ->
%     Msg = msg_from_event(Topic, Data, Context),
%     add_lastwill(Context#context.page_pid, WillId, Msg, Context);
handle_cmd(#z_mqtt_cmd{cmd= <<"publish">>, topic=Topic, payload=Data}, Context) ->
    Msg = msg_from_event(Topic, Data, Context),
    z_mqtt:publish(Msg, Context).
% handle_cmd(#z_mqtt_cmd{cmd= <<"subscribe">>, topic=Topic}, Context) ->
%     % ?DEBUG({subscribe, Topic, Context#context.page_pid}),
%     z_mqtt:subscribe(Topic, Context#context.page_pid, Context);
% handle_cmd(#z_mqtt_cmd{cmd= <<"unsubscribe">>, topic=Topic}, Context) ->
%     % ?DEBUG({unsubscribe, Topic, Context#context.page_pid}),
%     z_mqtt:unsubscribe(Topic, Context#context.page_pid, Context).


msg_from_event(Topic, Data, Context) ->
    #mqtt_msg{
        topic=Topic,
        payload=z_mqtt:wrap_payload(Data, Context),
        encoder=fun(B) -> z_mqtt:encode_packet_payload(B) end
    }.


maybe_subscribe({<<"mqtt:", Topic/binary>>, F}, M, ModulePid, MyPid, Context) ->
    subscribe_topic(z_mqtt:expand_context_topic(Topic, Context), ?QOS_0, {M,F}, ModulePid, MyPid, Context);
maybe_subscribe(_, _M, _Pid, _MyPid, _Context) ->
    nop.


% Add middleman process to this supervisor
% Middleman is linked to the module ModulePid.
% Middleman subscribes to emqtt with its own Pid
% Middleman calls M:F(Message, ModulePid, Context) when it receives a message.
subscribe_topic(Topic, Qos, MFA, ModulePid, MyPid, Context) ->
    Sub = {
        {Topic, MFA, ModulePid},
        {z_mqtt_module_subscriber, start_link, [{Topic, Qos, MFA, ModulePid, z_context:site(Context)}]},
        transient, 5000, worker, [z_mqtt_module_subscriber]
    },
    lager:debug("MQTT subscribe ~p to ~p", [MFA, Topic]),
    case supervisor:start_child(MyPid, Sub) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

unsubscribe_topic(Topic, MFA, MyPid) ->
    supervisor:delete_child(MyPid, {Topic,MFA}).


add_lastwill(Pid, WillId, Msg, Context) when is_pid(Pid) ->
    case is_pid(Pid) of
        true ->
            WillId1 = z_convert:to_binary(WillId),
            case start_lastwill_proc(Pid, WillId1, Msg, Context) of
                {ok, _} ->
                    ok;
                {error, {already_started, ChildPid}} ->
                    z_mqtt_lastwill:add(ChildPid, WillId1, Msg, Context)
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
                    z_mqtt_lastwill:del(ChildPid, WillId1, Context)
    end;
del_lastwill(_Pid, _WillId, _Context) ->
    ok.


start_lastwill_proc(Pid, WillId, Msg, Context) ->
    Sub = {
        {lastwill, Pid},
        {z_mqtt_lastwill, start_link, [Pid, WillId, Msg, Context]},
        transient, 5000, worker, [z_mqtt_lastwill]
    },
    lager:debug("MQTT lastwill ~p to ~p", [Pid, Msg#mqtt_msg.topic]),
    {ok, ModPid} = z_module_manager:whereis(?MODULE, Context),
    supervisor:start_child(ModPid, Sub).

