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
    route/2,
    publish/2,
    publish/3,
    subscribe/2,
    subscribe/3,
    subscribe/4,
    unsubscribe/2,
    unsubscribe/3,
    maybe_context_topic/2,
    remove_context_topic/2,

    make_postback_payload/2,
    payload_data/1,
    encode_packet_payload/1
    ]).

-include_lib("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").

-compile([{parse_transform, lager_transform}]).

-record(z_mqtt_payload, {
        version = 1,
        site,
        user_id,
        encoding = binary,
        payload = <<>>
    }).



%% @doc Entry point for messages received via the mqtt listener.
%% @todo Check the payload encoding, don't assume ubf (need magic number?)
%% @todo Payload might be a z_mqtt_payload record, in that case only verify the user_id etc
route(#mqtt_msg{} = Msg, Context) ->
    Msg1 = Msg#mqtt_msg{
        topic=maybe_context_topic(Msg#mqtt_msg.topic, Context),
        payload=wrap_payload(ubf, Msg#mqtt_msg.payload, Context),
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
        topic=maybe_context_topic(Msg#mqtt_msg.topic, Context)
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
        topic=maybe_context_topic(Topic, Context),
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
    Topic1 = maybe_context_topic(Topic, Context),
    case z_mqtt_acl:is_allowed(subscribe, Topic1, Context) of
        true ->
            lager:debug("MQTT subscribe ~p to ~p", [Pid, Topic1]),
            emqtt_router:subscribe({Topic1, Qos}, Pid);
        false ->
            lager:debug("MQTT subscribe access denied to ~p", [Topic]),
            {error, eacces}
    end;
subscribe(Topic, Qos, MFA, Context) when is_tuple(MFA) ->
    Topic1 = maybe_context_topic(Topic, Context),
    case z_mqtt_acl:is_allowed(subscribe, Topic1, Context) of
        true ->
            case z_notifier:first(#mqtt_subscribe{topic=Topic1, qos=Qos, mfa=MFA}, Context) of
                undefined -> undefined;
                ok -> ok
            end;
        false ->
            lager:debug("MQTT subscribe access denied to ~p", [Topic]),
            {error, eacces}
    end.

unsubscribe(Topic, Context) ->
    unsubscribe(Topic, self(), Context).

unsubscribe(Topic, Pid, Context) when is_pid(Pid) ->
    emqtt_router:unsubscribe(maybe_context_topic(Topic, Context), Pid);
unsubscribe(Topic, MFA, Context) when is_tuple(MFA) ->
    z_notifier:first(#mqtt_unsubscribe{topic=maybe_context_topic(Topic, Context), mfa=MFA}, Context).


%doc Add a decode wrapper around the payload we received from via a postback event.
make_postback_payload(undefined, Context) ->
    wrap_payload(binary, <<>>, Context);
make_postback_payload([], Context) ->
    wrap_payload(binary, <<>>, Context);
make_postback_payload(Data, Context) ->
    wrap_payload(ubf, Data, Context).

wrap_payload(Encoding, Data, Context) ->
    #z_mqtt_payload{
        site=z_context:site(Context), 
        user_id=z_acl:user(Context),
        encoding=Encoding,
        payload=z_convert:to_binary(Data)
    }.


payload_data(#mqtt_msg{payload=Payload}) ->
    payload_data(Payload);
payload_data(Bin) when is_binary(Bin) ->
    {ok, Bin};
payload_data(#z_mqtt_payload{encoding=binary, payload=Data}) ->
    {ok, Data};
payload_data(#z_mqtt_payload{encoding=term, payload=Data}) ->
    {ok, Data};
payload_data(#z_mqtt_payload{encoding=ubf, payload=Data}) when is_binary(Data) ->
    case z_ubf:decode(Data) of
        {ok, Term, _} -> {ok, Term};
        {more, _} -> {error, eof};      
        {error, _} = Error -> Error
    end.

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


%% @doc Add the site's name to a topic iff the topic doesn't start with '//'
-spec maybe_context_topic(binary()|string(), #context{}) -> binary().
maybe_context_topic("//" ++ Topic, _Context) ->
    unicode:characters_to_binary(Topic);
maybe_context_topic(<<"//", Topic/binary>>, _Context) ->
    Topic;
maybe_context_topic("site/" ++ _ = Topic, _Context) ->
    unicode:characters_to_binary(Topic);
maybe_context_topic(<<"site/", _/binary>> = Topic, _Context) ->
    Topic;
maybe_context_topic(Topic, Context) ->
    iolist_to_binary([
            <<"site/">>,
            z_convert:to_binary(z_context:site(Context)),
            $/,
            to_binary(drop_slash(Topic))
        ]).

remove_context_topic(Topic, #context{} = Context) ->
    remove_context_topic(Topic, z_convert:to_binary(z_context:site(Context)));
remove_context_topic(Topic, Site) when is_atom(Site) ->
    remove_context_topic(Topic, z_convert:to_binary(Site));
remove_context_topic(<<"site/", Topic/binary>> = ST, Site) ->
    case binary:split(Topic, <<"/">>) of
        [Site, LocalTopic] -> <<$/, LocalTopic/binary>>;
        _ -> <<"//", ST/binary>>
    end; 
remove_context_topic(Topic, _Site) ->
    <<"//", Topic/binary>>.


to_binary(L) when is_list(L) ->
    unicode:characters_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

drop_slash(<<"/", T/binary>>) -> T;
drop_slash([$/ | T ]) -> T;
drop_slash(T) -> T.
