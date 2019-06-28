%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2018 Marc Worrell

%% @doc Interface to MQTT pubsub functionality

%% Copyright 2013-2018 Marc Worrell
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
    publish/3,
    publish/4,
    publish/2,

    subscribe/2,
    subscribe/3,
    subscribe/4,
    subscribe/5,

    unsubscribe/2,
    unsubscribe/3,

    call/2,
    call/3,

    temp_response_topic/1,
    await_response/2,
    await_response/3,

    map_topic/2,
    map_topic_filter/2,

    origin_topic/1,
    flatten_topic/1
]).

-type topic() :: mqtt_session:topic().
-type topic_any() :: mqtt_sessions:topic()
                   | m_rsc:resource_id()
                   | {object, list()}
                   | {subject, list()}.
-type publish_options() :: #{ retain => boolean(), qos => 0 | 1 | 2 }.
-type callback() :: pid() | mfa().

-export_type([ topic/0, topic_any/0 ]).

-include_lib("zotonic.hrl").


-define(MQTT_CALL_TIMEOUT, 10000).


-spec publish( topic(), term(), z:context()) -> ok | {error, term()}.
publish(Topic, Payload, Context) ->
    publish(Topic, Payload, #{ qos => 0, retain => false }, Context).

-spec publish( topic(), term(), publish_options(), z:context()) -> ok | {error, term()}.
publish(Topic, Payload, Options, Context) ->
    Msg = #{
        type => publish,
        topic => Topic,
        payload => Payload,
        qos => maps:get(qos, Options, 0),
        retain => maps:get(retain, Options, false)
    },
    publish(Msg, Context).


-spec publish( mqtt_packet_map:mqtt_packet(), z:context()) -> ok | {error, term()}.
publish(#{ type := publish, topic := Topic } = Msg, Context) ->
    case map_topic(Topic, Context) of
        {ok, Topic1} ->
            mqtt_sessions:publish(z_context:site(Context), Msg#{ topic => Topic1 }, Context);
        {error, _} = Error ->
            Error
    end.

-spec call( mqtt_packet_map:mqtt_packet(), z:context() ) -> {ok, term()} | {error, term()}.
call( #{ type := publish } = Msg, Context ) ->
    {ok, RespTopic} = temp_response_topic(Context),
    Props = maps:get(properties, Msg, #{}),
    Msg1 = Msg#{
        properties => Props#{ response_topic => RespTopic }
    },
    case publish(Msg1, Context) of
        ok ->
            case await_response(RespTopic, ?MQTT_CALL_TIMEOUT, Context) of
                {ok, #{ message := ReplyMsg } } ->
                    {ok, maps:get(payload, ReplyMsg, undefined)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec call( topic(), term(), z:context() ) -> {ok, term()} | {error, term()}.
call( Topic, Payload, Context ) ->
    Msg = #{
        type => publish,
        topic => Topic,
        payload => Payload,
        qos => 0,
        retain => false
    },
    call(Msg, Context).


-spec subscribe( topic(), z:context() ) -> ok | {error, term()}.
subscribe(TopicFilter, Context) ->
    subscribe(TopicFilter, self(), self(), #{ qos => 0 }, Context).

-spec subscribe( topic(), pid(), z:context() ) -> ok | {error, term()}.
subscribe(TopicFilter, Pid, Context) when is_pid(Pid) ->
    subscribe(TopicFilter, Pid, Pid, #{ qos => 0 }, Context).


-spec subscribe( topic(), callback(), pid(), z:context() ) -> ok | {error, term()}.
subscribe(TopicFilter, Callback, OwnerPid, Context) when is_pid(OwnerPid) ->
    subscribe(TopicFilter, Callback, OwnerPid, #{ qos => 0 }, Context).

-spec subscribe( topic(), callback(), pid(), mqtt_sessions:subscriber_options(), z:context() ) -> ok | {error, term()}.
subscribe(TopicFilter, Callback, OwnerPid, Options, Context) when is_pid(OwnerPid) ->
    case map_topic_filter(TopicFilter, Context) of
        {ok, TopicFilter1} ->
            mqtt_sessions:subscribe(z_context:site(Context), TopicFilter1, Callback, OwnerPid, Options, Context);
        {error, _} = Error ->
            Error
    end.


-spec unsubscribe( topic(), z:context() ) -> ok | {error, term()}.
unsubscribe(TopicFilter, Context) ->
    unsubscribe(TopicFilter, self(), Context).

-spec unsubscribe( topic(), pid(), z:context() ) -> ok | {error, term()}.
unsubscribe(TopicFilter, OwnerPid, Context) when is_pid(OwnerPid) ->
    case map_topic_filter(TopicFilter, Context) of
        {ok, TopicFilter1} ->
            mqtt_sessions:unsubscribe(z_context:site(Context), TopicFilter1, OwnerPid);
        {error, _} = Error ->
            Error
    end.


-spec temp_response_topic( z:context() ) -> {ok, mqtt_sessions:topic()} | {error, term()}.
temp_response_topic(Context) ->
    mqtt_sessions:temp_response_topic(z_context:site(Context), z_context:prune_for_async(Context)).

-spec await_response( mqtt_sessions:topic(), z:context() ) -> {ok, mqtt_packet_map:mqtt_packet()} | {error, timeout}.
await_response( Topic, Context ) ->
    mqtt_sessions:await_response(z_context:site(Context), Topic).

-spec await_response( mqtt_sessions:topic(), pos_integer(), z:context() ) -> {ok, mqtt_packet_map:mqtt_packet()} | {error, timeout}.
await_response( Topic, Timeout, Context ) ->
    mqtt_sessions:await_response(z_context:site(Context), Topic, Timeout).



-spec map_topic( mqtt_sessions:topic(), z:context() ) -> {ok, mqtt_sessions:topic()} | {error, no_client}.
map_topic(Topic, Context) when is_binary(Topic) ->
    map_topic(binary:split(Topic, <<"/">>, [global]), Context);
map_topic([ <<"~client">> | _ ], #context{ client_topic = undefined }) ->
    {error, no_client};
map_topic([ <<"~client">> | T ], #context{ client_topic = Route }) ->
    {ok, Route ++ T};
map_topic(Topic, Context) when is_tuple(Topic) ->
    {ok, map_topic_filter(Topic, Context)};
map_topic(Topic, _Context) ->
    {ok, Topic}.


%% @doc Map subscription topic to a topic filter.
-spec map_topic_filter( topic_any(), z:context()) -> topic().
map_topic_filter(Topic, Context) when is_list(Topic) ->
    map_topic(Topic, Context);
map_topic_filter(Topic, Context) when is_binary(Topic) ->
    map_topic(binary:split(Topic, <<"/">>, [global]), Context);
map_topic_filter(RscId, _Context) when is_integer(RscId) ->
    [ <<"model">>, <<"rsc">>, <<"event">>, z_convert:to_binary(RscId), <<"+">> ];
map_topic_filter({object, Props}, Context) when is_list(Props) ->
    map_topic_edge(<<"o">>, Props, Context);
map_topic_filter({subject, Props}, Context) when is_list(Props) ->
    map_topic_edge(<<"s">>, Props, Context).

map_topic_edge(ObjSub, Props, Context) ->
    Id = proplists:get_value(id, Props),
    MaybePredicate = proplists:get_value(predicate, Props),
    PredName = to_predicate_name(MaybePredicate, Context),
    [
        <<"model">>, <<"edge">>, <<"event">>,
        z_convert:to_binary(Id), ObjSub, PredName
    ].

to_predicate_name(undefined, _Context) -> <<"+">>;
to_predicate_name(<<"*">>, _Context) -> <<"+">>;
to_predicate_name("*", _Context) -> <<"+">>;
to_predicate_name('*', _Context) -> <<"+">>;
to_predicate_name(<<>>, _Context) -> <<"+">>;
to_predicate_name(Id, Context) when is_integer(Id) ->
    {ok, Name} = m_predicate:id_to_name(Id, Context),
    z_convert:to_binary(Name);
to_predicate_name(Pred, _Context) ->
    z_convert:to_binary(Pred).


origin_topic(<<"bridge/origin/", _/binary>> = Topic) ->
    Topic;
origin_topic(Topic) when is_binary(Topic) ->
    <<"bridge/origin/", Topic/binary>>;
origin_topic([ <<"bridge">>, <<"origin">> | _ ] = Topic) ->
    Topic;
origin_topic(Topic) when is_list(Topic) ->
    flatten_topic([ <<"bridge">>, <<"origin">> | Topic ]).

flatten_topic(T) when is_binary(T) ->
    T;
flatten_topic(T) when is_list(T) ->
    T1 = lists:map(fun z_convert:to_binary/1, T),
    iolist_to_binary( z_utils:combine_defined($/, T1) ).
