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
    wait_ack/3,
    ack/2
    ]).

-include("zotonic.hrl").

-record(tq, {
        len = 0 :: integer(),
        qos0,
        qos1,
        qos2,
        send_ack,
        wait_ack :: list()
    }).

%% Expire undelivered messages after 5 minutes
-define(RETRY_EXPIRE, 300000).

%% Requeue qos 1&2 messages once after waiting for an ack for 10 seconds
-define(RETRY_ACK, 10000).

%% Maximum number of message in queue
-define(MAX_LEN, 100).


new() ->
    #tq{
        % Number of items in all queues
        len = 0,

        % Queues for outgoing messages and acks
        qos0 = queue:new(),
        qos1 = queue:new(),
        qos2 = queue:new(),
        send_ack = queue:new(),

        % Sent messages waiting for an ack
        wait_ack = []
    }.

is_empty(TQ) ->
    queue:is_empty(TQ#tq.send_ack) 
    andalso queue:is_empty(TQ#tq.qos2)
    andalso queue:is_empty(TQ#tq.qos1)
    andalso queue:is_empty(TQ#tq.qos0).

%% @doc Queue a message according to its qos level.
in(Msg, #tq{len=Len} = TQ) when Len > ?MAX_LEN ->
    in(Msg, prune(TQ));
in(#z_msg_v1{qos=0} = Msg, TQ) ->
    TQ#tq{qos0=queue:in(Msg, TQ#tq.qos0), len=TQ#tq.len+1};
in(#z_msg_v1{qos=1} = Msg, TQ) ->
    TQ#tq{qos1=queue:in(Msg, TQ#tq.qos1), len=TQ#tq.len+1};
in(#z_msg_v1{qos=2} = Msg, TQ) ->
    TQ#tq{qos2=queue:in(Msg, TQ#tq.qos2), len=TQ#tq.len+1};
in(#z_msg_ack{} = Ack, TQ) ->
    TQ#tq{send_ack=queue:in({Ack,now_msec()}, TQ#tq.send_ack), len=TQ#tq.len+1}.

%% @doc Fetch a message from the queue, precendence for acks and higher qos messages
out(TQ) ->
    case queue:out(TQ#tq.send_ack) of
        {empty, _Q} -> out2(TQ);
        {{value, {Ack,_}}, Q1} -> {Ack, TQ#tq{send_ack=Q1, len=TQ#tq.len-1}}
    end.

out2(TQ) ->
    case queue:out(TQ#tq.qos2) of
        {empty, _Q} -> out1(TQ);
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos2=Q1, len=TQ#tq.len-1}}
    end.

out1(TQ) ->
    case queue:out(TQ#tq.qos1) of
        {empty, _Q} -> out0(TQ);
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos1=Q1, len=TQ#tq.len-1}}
    end.

out0(TQ) ->
    case queue:out(TQ#tq.qos0) of
        {empty, _Q} -> {empty, TQ};
        {{value, Msg}, Q1} -> {Msg, TQ#tq{qos0=Q1, len=TQ#tq.len-1}}
    end.

out_all(TQ) ->
    All = to_list(TQ),
    {All, TQ#tq{
        len  = 0,
        qos0 = queue:new(),
        qos1 = queue:new(),
        qos2 = queue:new(),
        send_ack = queue:new()
    }}.

to_list(TQ) ->
    [ Ack || {Ack,_Tm} <- queue:to_list(TQ#tq.send_ack) ]
    ++ queue:to_list(TQ#tq.qos2)
    ++ queue:to_list(TQ#tq.qos1)
    ++ queue:to_list(TQ#tq.qos0).

ack(#z_msg_ack{msg_id=MsgId}, TQ) ->
    Ack1 = lists:filter(
                fun (#z_msg_v1{msg_id=Id}) ->
                    Id =/= MsgId 
                end, 
                TQ#tq.wait_ack),
    TQ#tq{wait_ack = Ack1}.


wait_ack(#z_msg_ack{}, _Queue, TQ) ->
    TQ;
wait_ack(#z_msg_v1{dup=false, push_queue=Queue, qos=QoS} = Msg, Queue, TQ) when QoS > 0 ->
    TQ#tq{wait_ack = [Msg|TQ#tq.wait_ack]};
wait_ack(_Msg, _Queue, TQ) ->
    TQ.

%% @doc Periodic cleanup, drop expired messages from all queues.
%% Requeue messages from the ack queue that are waiting too long.
periodic(#tq{} = TQ) ->
    Now = now_msec(),
    Expired = Now - ?RETRY_EXPIRE,
    {Q0,N1} = expire(TQ#tq.qos0, Expired, TQ#tq.len),
    {Q1,N2} = expire(TQ#tq.qos1, Expired, N1),
    {Q2,N3} = expire(TQ#tq.qos2, Expired, N2),
    {SendAck,N4} = expire(TQ#tq.send_ack, Expired, N3),
    TQ1 = TQ#tq{
        len = N4,
        qos0 = Q0,
        qos1 = Q1,
        qos2 = Q2,
        send_ack = SendAck,
        wait_ack = []
    },
    % Requeue messages waiting too long for an ack
    Retry = Now - ?RETRY_ACK,
    lists:foldl(
            fun
                (#z_msg_v1{timestamp=Tm} = Msg, TQAcc) when Tm < Retry ->
                    in(Msg#z_msg_v1{dup=true}, TQAcc);
                (Msg, TQAcc) ->
                    TQAcc#tq{
                        wait_ack=[Msg|TQAcc#tq.wait_ack]
                    }
            end,
            TQ1,
            TQ#tq.wait_ack).


expire(MQs, Expired, N) when is_list(MQs) ->
    lists:foldl(
            fun
                (#z_msg_v1{timestamp=Tm} = Msg, {AccMQ,AccN}) when Tm < Expired ->
                    lager:debug("Dropping expired message ~p", [Msg]),
                    {AccMQ, AccN-1};
                (Msg, {AccMQ,AccN}) ->
                    {[Msg|AccMQ], AccN}
            end,
            {[],N},
            MQs);
expire(MQ, Expired, N) ->
    case queue:peek(MQ) of
        empty ->
            {MQ, N};
        {value, #z_msg_v1{timestamp=TM}} when TM < Expired ->
            {_, MQ1} = queue:out(MQ),
            expire(MQ1, Expired, N-1);
        {value, {#z_msg_ack{}, TM}} when TM < Expired ->
            {_, MQ1} = queue:out(MQ),
            expire(MQ1, Expired, N-1);
        {value, _} ->
            {MQ, N}
    end.


%% @doc Remove messages from the queues, called when more than ?MAX_LEN items are queued.
%% First qos=0 items are removed
prune(#tq{len=Len} = TQ) when Len >= ?MAX_LEN ->
    case queue:len(TQ#tq.qos0) of
        0 -> prune1(TQ);
        N -> prune1(TQ#tq{qos0=queue:new(), len=TQ#tq.len-N})
    end;
prune(TQ) ->
    TQ.

prune1(#tq{len=Len} = TQ) when Len >= ?MAX_LEN ->
    case queue:len(TQ#tq.qos1) of
        0 -> prune_send_ack(TQ);
        N -> prune_send_ack(TQ#tq{qos1=queue:new(), len=TQ#tq.len-N})
    end;
prune1(TQ) ->
    TQ.

prune_send_ack(#tq{len=Len} = TQ) when Len >= ?MAX_LEN ->
    case queue:len(TQ#tq.send_ack) of
        0 -> prune2(TQ);
        N -> prune2(TQ#tq{send_ack=queue:new(), len=TQ#tq.len-N})
    end;
prune_send_ack(TQ) ->
    TQ.

prune2(#tq{len=Len} = TQ) when Len >= ?MAX_LEN ->
    N = queue:len(TQ#tq.qos2),
    TQ#tq{qos2=queue:new(), len=TQ#tq.len-N};
prune2(TQ) ->
    TQ.


now_msec() ->
    {A,B,C} = os:timestamp(),
    (A*1000000+B)*1000 + C div 1000.
