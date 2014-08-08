%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Queue for managing transports of messages with z_transport.
%% The queue manages #z_msg_v1 records and has some heuristics for queue management.

%% Copyright 2014 Marc Worrell
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

-module(z_transport_queue).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/0,
    is_empty/1,
    in/2,
    out/1,
    out_all/1,
    to_list/1,
    periodic/1,
    wait_ack/2,
    wait_ack/3,
    ack/2
    ]).

-include("zotonic.hrl").

-record(tq, {
        qos0,
        qos1,
        qos2,
        send_ack,
        wait_ack :: list()
    }).

new() ->
    #tq{
        % Queues for outgoing messages and acks
        qos0 = queue:new(),
        qos1 = queue:new(),
        qos2 = queue:new(),
        send_ack = queue:new(),

        % Sent messages waiting for an ack; send from this transport_queue
        wait_ack = []
    }.

is_empty(TQ) ->
    queue:is_empty(TQ#tq.qos2)
    andalso queue:is_empty(TQ#tq.qos1)
    andalso queue:is_empty(TQ#tq.qos0).

%% @doc Queue a message according to its qos level.
in(#z_msg_v1{qos=0} = Msg, TQ) ->
    TQ#tq{qos0=queue:in(Msg, TQ#tq.qos0)};
in(#z_msg_v1{qos=1} = Msg, TQ) ->
    TQ#tq{qos1=queue:in(Msg, TQ#tq.qos1)};
in(#z_msg_v1{qos=2} = Msg, TQ) ->
    TQ#tq{qos2=queue:in(Msg, TQ#tq.qos2)};
in(#z_msg_ack{} = Msg, TQ) ->
    TQ#tq{qos2=queue:in(Msg, TQ#tq.qos2)}.

%% @doc Fetch a message from the queue, precendence for acks and higher qos messages
out(TQ) ->
    case queue:out(TQ#tq.send_ack) of
        {empty, _Q} -> out2(TQ);
        {{value, Ack}, Q1} -> {Ack, TQ#tq{send_ack=Q1}}
    end.

out2(TQ) ->
    case queue:out(TQ#tq.qos2) of
        {empty, _Q} -> out1(TQ);
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos2=Q1}}
    end.

out1(TQ) ->
    case queue:out(TQ#tq.qos1) of
        {empty, _Q} -> out0(TQ);
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos1=Q1}}
    end.

out0(TQ) ->
    case queue:out(TQ#tq.qos0) of
        {empty, _Q} -> {empty, TQ};
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos0=Q1}}
    end.

out_all(TQ) ->
    All = to_list(TQ),
    {All, TQ#tq{
        qos0 = queue:new(),
        qos1 = queue:new(),
        qos2 = queue:new(),
        send_ack = queue:new()
    }}.

to_list(TQ) ->
    queue:to_list(TQ#tq.send_ack)
    ++ queue:to_list(TQ#tq.qos2)
    ++ queue:to_list(TQ#tq.qos1)
    ++ queue:to_list(TQ#tq.qos0).

ack(MsgId, TQ) ->
    Ack1 = lists:filter(
                fun (#z_msg_v1{msg_id=Id}) ->
                    Id =/= MsgId 
                end, 
                TQ#tq.wait_ack),
    TQ#tq{wait_ack = Ack1}.

wait_ack(Msg, TQ) ->
    TQ#tq{wait_ack = [Msg|TQ#tq.wait_ack]}.

wait_ack(#z_msg_v1{push_queue=Queue, qos=QoS} = Msg, Queue, TQ) when QoS > 0 ->
    TQ#tq{wait_ack = [Msg|TQ#tq.wait_ack]};
wait_ack(_Msg, _Queue, TQ) ->
    TQ.

%% @doc Periodic cleanup, drop expired messages from all queues.
%% Requeue messages from the ack queue that are waiting too long.
periodic(TQ) ->
    TQ.

