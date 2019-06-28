%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2013-2018 Marc Worrell

%% @doc Link MQTT messaging with Zotonic module callbacks

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

-module(mod_mqtt).

-mod_title("MQTT").
-mod_description("MQTT messaging, connecting server and browser.").
-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_prio(1000).

-export([
    'mqtt:test/#'/2,
    'mqtt:model/+/get/#'/2,
    'mqtt:model/+/post/#'/2,
    'mqtt:model/+/delete/#'/2,

    observe_module_activate/2,
    observe_action_event_type/2,
    module_callback/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec 'mqtt:test/#'(mqtt_packet_map:mqtt_packet(), z:context()) -> ok.
'mqtt:test/#'(Msg, _Context) ->
    lager:debug("mod_mqtt test topic: ~p", [Msg]),
    ok.

%% @doc Handle 'get' request for a model
-spec 'mqtt:model/+/get/#'(mqtt_packet_map:mqtt_packet(), z:context()) -> ok | {error, term()}.
'mqtt:model/+/get/#'(#{
            type := publish,
            topic := [ <<"model">>, Model, <<"get">> | Path ]
        } = Msg, Context) ->
    handle_model_request(Model, get, Path, Msg, Context).

%% @doc Handle 'post' request for a model
-spec 'mqtt:model/+/post/#'(mqtt_packet_map:mqtt_packet(), z:context()) -> ok | {error, term()}.
'mqtt:model/+/post/#'(#{
            type := publish,
            topic := [ <<"model">>, Model, <<"post">> | Path ]
        } = Msg, Context) ->
    handle_model_request(Model, post, Path, Msg, Context).

%% @doc Handle 'delete' request for a model
-spec 'mqtt:model/+/delete/#'(mqtt_packet_map:mqtt_packet(), z:context()) -> ok | {error, term()}.
'mqtt:model/+/delete/#'(#{
            type := publish,
            topic := [ <<"model">>, Model, <<"delete">> | Path ]
        } = Msg, Context) ->
    handle_model_request(Model, delete, Path, Msg, Context).

%% @doc Call the module and publish the result back to the 'response_topic'
handle_model_request(Model, Verb, Path, Msg, Context) ->
    Res = z_model:callback(Model, Verb, Path, Msg, Context),
    publish_response(Msg, Res, Context).

publish_response(#{ properties := #{ response_topic := Topic } } = Msg, {ok, Res}, Context) ->
    QoS = maps:get(qos, Msg, 0),
    z_mqtt:publish(Topic, #{ status => <<"ok">>, result => Res }, #{ qos => QoS }, Context);
publish_response(#{ properties := #{ response_topic := Topic } } = Msg, {error, Res}, Context) ->
    QoS = maps:get(qos, Msg, 0),
    z_mqtt:publish(Topic, #{ status => <<"error">>, message => Res }, #{ qos => QoS }, Context);
publish_response(#{}, _Res, _Context) ->
    ok.

observe_module_activate(#module_activate{ module = Module, pid = ModulePid }, Context) ->
    Exports = erlang:get_module_info(Module, exports),
    Fs = [ {z_convert:to_binary(F), F} || {F, 2} <- Exports ],
    lists:foreach(fun(F) ->
                    maybe_subscribe(F, Module, ModulePid, Context)
                  end,
                  Fs).

maybe_subscribe({<<"mqtt:", TopicFilter/binary>>, Function}, Module, ModulePid, Context) ->
    SubscriberContext = z_acl:sudo( z_context:new( z_context:site(Context) ) ),
    Callback = {?MODULE, module_callback, [ Module, Function ]},
    z_mqtt:subscribe(TopicFilter, Callback, ModulePid, #{ qos => 0 }, SubscriberContext);
maybe_subscribe(_, _M, _Pid, _Context) ->
    ok.

%% @doc Call the module subscription function using the context of the publisher
-spec module_callback(module(), atom(), map()) -> any().
module_callback(Module, Function, #{ message := Msg, publisher_context := Context }) ->
    Module:Function(Msg, Context).

%% @doc Handle the <tt>{live ...}</tt> event type.
observe_action_event_type(#action_event_type{event={mqtt, _Args}} = Ev, Context) ->
    scomp_mqtt_live:event_type_mqtt(Ev, Context);
observe_action_event_type(_, _Context) ->
    undefined.

