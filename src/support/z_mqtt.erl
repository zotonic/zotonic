%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013 Marc Worrell

%% @doc Interface to MQTT pubsub functionality

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

-module(z_mqtt).

-export([
    transport_incoming/2,

    route/2,
    publish/2,
    publish/3,
    subscribe/2,
    subscribe/3,
    subscribe/4,
    unsubscribe/2,
    unsubscribe/3,
    expand_context_topic/2,
    make_context_topic/2,

    wrap_payload/2,
    payload_data/1,
    encode_packet_payload/1
    ]).

-include_lib("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").

-compile([{parse_transform, lager_transform}]).

-record(z_mqtt_payload, {
        site,
        user_id,
        payload
    }).


%% @doc Transport incoming messages to mod_mqtt
%% @todo Decouple this or move the mod_mqtt routines to core z_mqtt functions
transport_incoming(Cmd, Context) when is_tuple(Cmd) ->
    z_notifier:first(Cmd, Context).


%% @doc Entry point for messages received via the mqtt listener.
%% @todo Check the payload encoding, don't assume ubf (need magic number?)
%% @todo Payload might be a z_mqtt_payload record, in that case only verify the user_id etc
route(#mqtt_msg{} = Msg, Context) ->
    Msg1 = Msg#mqtt_msg{
        topic=expand_context_topic(Msg#mqtt_msg.topic, Context),
        payload=wrap_payload(Msg#mqtt_msg.payload, Context),
        encoder=fun(B) -> z_mqtt:encode_packet_payload(B) end
    },
    case z_mqtt_acl:is_allowed(publish, Msg1#mqtt_msg.topic, Context) of
        true ->
            emqtt_router:publish(Msg1);
        false ->
            lager:debug("MQTT publish access denied to ~p", [Msg1#mqtt_msg.topic]),
            {error, eacces}
    end.

%% @doc Entry point for messages received via events
publish(#mqtt_msg{} = Msg, Context) ->
    Msg1 = Msg#mqtt_msg{
        topic=expand_context_topic(Msg#mqtt_msg.topic, Context)
    },
    case z_mqtt_acl:is_allowed(publish, Msg1#mqtt_msg.topic, Context) of
        true ->
            emqtt_router:publish(Msg1);
        false ->
            lager:debug("MQTT publish access denied to ~p", [Msg1#mqtt_msg.topic]),
            {error, eacces}
    end.

publish(Topic, #z_mqtt_payload{} = Payload, Context) ->
    Msg = #mqtt_msg{
        topic=expand_context_topic(Topic, Context),
        retain=false,
        qos=?QOS_0,
        payload=Payload,
        encoder=fun(B) -> z_mqtt:encode_packet_payload(B) end
    },
    case z_mqtt_acl:is_allowed(publish, Msg#mqtt_msg.topic, Context) of
        true -> 
            emqtt_router:publish(Msg);
        false ->
            lager:debug("MQTT publish access denied to ~p", [Msg#mqtt_msg.topic]),
            {error, eacces}
    end;
publish(Topic, Data, Context) ->
    Payload = #z_mqtt_payload{
        site=z_context:site(Context), 
        user_id=z_acl:user(Context),
        payload=Data
    },
    publish(Topic, Payload, Context).

subscribe(Topic, Context) ->
    subscribe(Topic, ?QOS_0, self(), Context).

subscribe(Topic, Callback, Context) when is_pid(Callback); is_tuple(Callback) ->
    subscribe(Topic, ?QOS_0, Callback, Context).

subscribe(Topic, Qos, Pid, Context) when is_pid(Pid) ->
    Topic1 = expand_context_topic(Topic, Context),
    case z_mqtt_acl:is_allowed(subscribe, Topic1, Context) of
        true ->
            lager:debug("MQTT subscribe ~p to ~p for ~p", [Pid, Topic1, z_acl:user(Context)]),
            emqtt_router:subscribe({Topic1, Qos}, Pid);
        false ->
            lager:debug("MQTT subscribe access denied to ~p for ~p", [Topic, z_acl:user(Context)]),
            {error, eacces}
    end;
subscribe(Topic, Qos, MFA, Context) when is_tuple(MFA) ->
    Topic1 = expand_context_topic(Topic, Context),
    case z_mqtt_acl:is_allowed(subscribe, Topic1, Context) of
        true ->
            case z_notifier:first(#mqtt_subscribe{topic=Topic1, qos=Qos, mfa=MFA}, Context) of
                undefined -> undefined;
                ok -> ok
            end;
        false ->
            lager:debug("MQTT subscribe access denied to ~p for ~p", [Topic, z_acl:user(Context)]),
            {error, eacces}
    end.

unsubscribe(Topic, Context) ->
    unsubscribe(Topic, self(), Context).

unsubscribe(Topic, Pid, Context) when is_pid(Pid) ->
    lager:debug("MQTT unsubscribe ~p from ~p", [Pid, Topic]),
    emqtt_router:unsubscribe(expand_context_topic(Topic, Context), Pid);
unsubscribe(Topic, MFA, Context) when is_tuple(MFA) ->
    lager:debug("MQTT unsubscribe ~p from ~p", [MFA, Topic]),
    z_notifier:first(#mqtt_unsubscribe{topic=expand_context_topic(Topic, Context), mfa=MFA}, Context).


%doc Add a decode wrapper around the payload we received from via a postback event.
wrap_payload(Data, Context) ->
    #z_mqtt_payload{
        site=z_context:site(Context), 
        user_id=z_acl:user(Context),
        payload=Data
    }.


payload_data(#mqtt_msg{payload=Payload}) ->
    payload_data(Payload);
payload_data(Bin) when is_binary(Bin) ->
    {ok, Bin};
payload_data(#z_mqtt_payload{payload=Data}) ->
    {ok, Data}.

%% @doc UBF encode the data to be sent to a client over a TCP/IP connection.
-spec encode_packet_payload(#z_mqtt_payload{} | undefined | binary()) -> binary().
encode_packet_payload(undefined) ->
    <<>>;
encode_packet_payload(Payload) when is_binary(Payload) ->
    Length = z_convert:to_binary(size(Payload)),
    <<Length/binary, $~, Payload/binary, $~>>;
encode_packet_payload(Any) ->
    {ok, Bin} = z_ubf:encode(Any),
    Bin.


%% @doc Map the ~site, ~pagesession, ~session, ~user topics
-spec expand_context_topic(binary()|string(), #context{}) -> binary().
expand_context_topic(Topic, _Context) when is_list(Topic) ->
    unicode:characters_to_binary(Topic);
expand_context_topic(<<"~site", Topic/binary>>, Context) ->
    localsite(Topic, Context);    
expand_context_topic(<<"~user", Topic/binary>>, Context) ->
    localuser(Topic, Context);    
expand_context_topic(<<"~session", Topic/binary>>, Context) ->
    localsession(Topic, Context);    
expand_context_topic(<<"~pagesession", Topic/binary>>, Context) ->
    localpagesession(Topic, Context);    
expand_context_topic(<<$~, T/binary>> = Topic, Context) ->
    lager:error(z_context:lager_md(Context), "Illegal MQTT topic ~p, mapped to ~p", [Topic, T]),
    T;
expand_context_topic(Topic, _Context) ->
    Topic.

localsite(<<>>, Context) ->
    <<"site/",(z_convert:to_binary(z_context:site(Context)))/binary>>;
localsite(<<$/,_/binary>> = Topic, Context) ->
    <<"site/",(z_convert:to_binary(z_context:site(Context)))/binary, Topic/binary>>;
localsite(Topic, Context) when is_binary(Topic) ->
    <<"site/",(z_convert:to_binary(z_context:site(Context)))/binary, $/, Topic/binary>>.

localuser(Topic, #context{user_id=undefined} = Context) ->
    case Topic of
        <<>> -> localsite(<<"anonymous">>, Context);
        <<$/,_/binary>> -> localsite(<<"anonymous", Topic/binary>>, Context);
        _ -> localsite(<<"anonymous/", Topic/binary>>, Context)
    end;
localuser(Topic, #context{user_id=UserId} = Context) ->
    UserBin = z_convert:to_binary(UserId),
    case Topic of
        <<>> -> localsite(<<"user/", UserBin/binary>>, Context);
        <<$/,_/binary>> -> localsite(<<"user/", UserBin/binary, Topic/binary>>, Context);
        _ -> localsite(<<"user/", UserBin/binary, $/, Topic/binary>>, Context)
    end.

localsession(<<>>, #context{session_id=SessionId} = Context) when is_binary(SessionId) ->
    localsite(<<"session/", SessionId/binary>>, Context);
localsession(<<$/, _/binary>> = Topic, #context{session_id=SessionId} = Context) when is_binary(SessionId) ->
    localsite(<<"session/", SessionId/binary, Topic/binary>>, Context);
localsession(Topic, #context{session_id=SessionId} = Context) when is_binary(SessionId) ->
    localsite(<<"session/", SessionId/binary, $/, Topic/binary>>, Context).

localpagesession(<<>>, #context{page_id=PageId} = Context) when is_binary(PageId) ->
    localsite(<<"pagesession/", PageId/binary>>, Context);
localpagesession(<<$/, _/binary>> = Topic, #context{page_id=PageId} = Context) when is_binary(PageId) ->
    localsite(<<"pagesession/", PageId/binary, Topic/binary>>, Context);
localpagesession(Topic, #context{page_id=PageId} = Context) when is_binary(PageId) ->
    localsite(<<"pagesession/", PageId/binary, $/, Topic/binary>>, Context).



make_context_topic(<<"site/", Topic/binary>> = ST, Context) ->
    SiteBin = z_convert:to_binary(z_context:site(Context)),
    case binary:split(Topic, <<"/">>) of
        [SiteBin] -> <<"~site">>;
        [SiteBin, LocalTopic] -> make_localsite(LocalTopic, Context);
        _ -> ST
    end; 
make_context_topic(Topic, _Site) ->
    <<"//", Topic/binary>>.

make_localsite(<<>>, _Context) ->
    <<"~site">>;
make_localsite(<<"anonymous">>, #context{user_id=undefined} = _Context) ->
    <<"~user">>;
make_localsite(<<"anonymous/", Topic/binary>>, #context{user_id=undefined}) ->
    <<"~user/", Topic/binary>>;
make_localsite(<<"user/", Topic/binary>> = Topic0, #context{user_id=UserId}) when is_integer(UserId) ->
    UserIdBin = z_convert:to_binary(UserId),
    case Topic of
        UserIdBin ->                   <<"~user">>;
        <<UserIdBin, $/, T/binary>> -> <<"~user/", T/binary>>;
        _ ->                           <<"~site/", Topic0/binary>>
    end;
make_localsite(<<"session/", Topic/binary>> = Topic0, #context{session_id=SessionId}) when is_binary(SessionId) ->
    case Topic of
        SessionId ->                   <<"~session">>;
        <<SessionId, $/, T/binary>> -> <<"~session/", T/binary>>;
        _ ->                           <<"~site/", Topic0/binary>>
    end;
make_localsite(<<"session/", _/binary>> = Topic0, #context{session_pid=SessionPid} = Context) when is_pid(SessionPid) ->
    case z_session_manager:get_session_id(Context) of
        undefined -> <<"~site/", Topic0/binary>>;
        SessionId -> make_localsite(Topic0, Context#context{session_id=SessionId})
    end;
make_localsite(<<"pagesession/", Topic/binary>> = Topic0, #context{page_id=PageId}) when is_binary(PageId) ->
    case Topic of
        PageId ->                   <<"~pagesession">>;
        <<PageId, $/, T/binary>> -> <<"~pagesession/", T/binary>>;
        _ ->                        <<"~site/", Topic0/binary>>
    end;
make_localsite(Topic, _Context) ->
    <<"~site/", Topic/binary>>.


