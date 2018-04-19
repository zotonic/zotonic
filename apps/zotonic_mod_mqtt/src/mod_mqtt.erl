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
    'mqtt:model/+/get'/2,
    'mqtt:model/+/post'/2,

    observe_module_activate/2

    % observe_rsc_update_done/2,
    % observe_media_replace_file/2,
    % observe_edge_delete/2,
    % observe_edge_insert/2,
    % observe_edge_update/2,

    % observe_action_event_type/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% @doc Handle 'get' request for a model
-spec 'mqtt:model/+/get'(z:context(), map()) -> ok.
'mqtt:model/+/get'(_Ctx, #{
        message := #{
            type := publish,
            topic := [ <<"model">>, Model, <<"get">> | Path ],
            payload := Payload
        } = Msg,
        publisher_context := Context
    }) ->
    Res = case model_module(Model, Context) of
        {ok, Mod} ->
            maybe_resolve(Mod:m_get(Path, Payload, Context), Context);
        {error, _} = Error ->
            lager:info("Publish to unknown model ~p", [Model]),
            Error
    end,
    publish_response(Msg, Res, Context).

%% @doc Handle 'post' request for a model
-spec 'mqtt:model/+/post'(z:context(), map()) -> ok.
'mqtt:model/+/post'(_Ctx, #{
        message := #{
            type := publish,
            topic := [ <<"model">>, Model, <<"post">> | Path ],
            payload := Payload
        } = Msg,
        publisher_context := Context
    }) ->
    Res = case model_module(Model, Context) of
        {ok, Mod} ->
            Mod:m_post(Path, Payload, Context);
        {error, _} = Error ->
            lager:info("Publish to unknown model ~p", [Model]),
            Error
    end,
    publish_response(Msg, Res, Context).

maybe_resolve({ok, {Res, []}}, _Context) ->
    {ok, Res};
maybe_resolve({ok, {Res, Ks}}, Context) ->
    Res1 = z_template_compiler_runtime:find_nested_value(Res, Ks, #{}, Context),
    {ok, Res1};
maybe_resolve({error, _} = Error, _Context) ->
    Error.

publish_response(#{ properties := #{ response_topic := Topic } }, {ok, Res}, Context) ->
    z_mqtt:publish(Topic, #{ status => "ok", result => Res }, Context);
publish_response(#{ properties := #{ response_topic := Topic } }, {error, Res}, Context) ->
    z_mqtt:publish(Topic, #{ status => "error", message => Res }, Context);
publish_response(#{}, _Res, _Context) ->
    ok.

model_module(Name, Context) ->
    case z_module_indexer:lookup(model, Name, Context) of
        {ok, #module_index{ erlang_module = M }} ->
            {ok, M};
        {error, _} = Error ->
            Error
    end.


observe_module_activate(#module_activate{ module = Module, pid = ModulePid }, Context) ->
    Exports = erlang:get_module_info(Module, exports),
    Fs = [ {z_convert:to_binary(F), F} || {F, 2} <- Exports ],
    lists:foreach(fun(F) ->
                    maybe_subscribe(F, Module, ModulePid, Context)
                  end,
                  Fs).

maybe_subscribe({<<"mqtt:", TopicFilter/binary>>, F}, M, ModulePid, Context) ->
    SubscriberContext = z_acl:sudo( z_context:new( z_context:site(Context) ) ),
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

