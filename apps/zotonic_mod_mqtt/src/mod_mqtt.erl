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
    observe_module_activate/2
    % observe_z_mqtt_cmd/2

    % observe_rsc_update_done/2,
    % observe_media_replace_file/2,
    % observe_edge_delete/2,
    % observe_edge_insert/2,
    % observe_edge_update/2,

    % observe_action_event_type/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    'mqtt:test/mod_mqtt'/2
]).

'mqtt:test/mod_mqtt'(_SubscriberContext, Message) ->
    lager:debug("mqtt:test/mod_mqtt received: ~p", [ Message ]),
    ok.


observe_module_activate(#module_activate{ module = Module, pid = ModulePid }, Context) ->
    Exports = erlang:get_module_info(Module, exports),
    Fs = [ {z_convert:to_binary(F), F} || {F, 3} <- Exports ],
    lists:foreach(fun(F) ->
                    maybe_subscribe(F, Module, ModulePid, Context)
                  end,
                  Fs).

maybe_subscribe({<<"mqtt:", Topic/binary>>, F}, M, ModulePid, Context) ->
    SubscriberContext = z_acl:sudo( z_context:new( z_context:site(Context) ) ),
    TopicFilter = z_mqtt:map_topic_filter(Topic, Context),
    Callback = {M, F, [ SubscriberContext ]},
    z_mqtt:subscribe(TopicFilter, Callback, ModulePid, #{ qos => 0 }, SubscriberContext);
maybe_subscribe(_, _M, _Pid, _Context) ->
    ok.


% observe_rsc_update_done(#rsc_update_done{id=Id} = UpdateDone, Context) ->
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(Id))/binary>>,
%         UpdateDone#rsc_update_done{pre_props=[], post_props=[]},
%         Context).

% observe_media_replace_file(#media_replace_file{id=Id} = MediaReplace, Context) ->
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(Id))/binary, "/medium">>,
%         MediaReplace#media_replace_file{medium=[]},
%         Context).

% observe_edge_delete(#edge_delete{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeDelete, Context) ->
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeDelete,
%         Context),
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeDelete,
%         Context).

% observe_edge_insert(#edge_insert{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeInsert, Context) ->
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeInsert,
%         Context),
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeInsert,
%         Context).

% observe_edge_update(#edge_update{subject_id=SubjectId, predicate=PredName, object_id=ObjectId} = EdgeUpdate, Context) ->
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(SubjectId))/binary, "/o/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeUpdate,
%         Context),
%     z_mqtt:publish(
%         <<"~site/rsc/",(z_convert:to_binary(ObjectId))/binary, "/s/", (z_convert:to_binary(PredName))/binary>>,
%         EdgeUpdate,
%         Context).

%% @doc Handle the <tt>{live ...}</tt> event type.
% observe_action_event_type(#action_event_type{event={mqtt, _Args}} = Ev, Context) ->
%     scomp_mqtt_live:event_type_mqtt(Ev, Context);
% observe_action_event_type(_, _Context) ->
%     undefined.



%% ==========================================================================
%% Internal functions.
%% ==========================================================================


% handle_cmd(#z_mqtt_cmd{cmd= <<"publish">>, topic=Topic, payload=Data}, Context) ->
%     Msg = msg_from_event(Topic, Data, Context),
%     z_mqtt:publish(Msg, Context).


