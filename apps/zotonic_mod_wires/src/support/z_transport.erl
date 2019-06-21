%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2018  Marc Worrell
%% @doc Deprecated transport handling.

%% Copyright 2014-2018 Marc Worrell
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

-module(z_transport).

-export([
    transport/3,
    reply/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type payload() :: map()
                 | #postback_event{}
                 | #postback_notify{}.

%% @doc Send JS to the client, client will evaluate the JS.
-spec reply(binary(), z:context()) -> ok | {error, term()}.
reply(JavaScript, #context{ client_id = ClientId } = Context) when is_binary(ClientId), is_binary(JavaScript) ->
    Topic = [ <<"bridge">>, ClientId, <<"zotonic-transport">>, <<"eval">> ],
    z_mqtt:publish(Topic, JavaScript, #{ qos => 1 }, Context);
reply(JavaScript, Context) when is_binary(JavaScript) ->
    case z_context:get_q(<<"zotonic_topic_reply">>, Context) of
        undefined ->
            {error, no_route};
        Topic ->
            z_mqtt:publish(Topic, JavaScript, #{ qos => 1 }, Context)
    end.

%% @doc Handle incoming transport messages, call event handlers of delegates.
-spec transport( binary() | undefined, payload(), z:context()) -> ok | {error, term()}.
transport(<<"postback">>, #postback_event{ postback = Postback } = Event, Context) ->
    % submit or postback events
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context),
    TriggerId1 = case TriggerId of
                    undefined -> Event#postback_event.trigger;
                    _         -> TriggerId
                 end,
    TargetId1 =  case TargetId of
                    undefined -> Event#postback_event.target;
                    _         -> TargetId
                 end,
    case maybe_set_q(mqtt, Event#postback_event.data, Context) of
        {ok, Context1} ->
            TriggerValue = Event#postback_event.triggervalue,
            Context2 = z_context:set_q(<<"triggervalue">>, TriggerValue, Context1),
            ContextRsc = z_context:set_controller_module(Module, Context2),
            ContextRes = incoming_postback_event(is_submit_event(EventType), Module, Tag, TriggerId1, TargetId1, ContextRsc),
            incoming_context_result(ContextRes);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(<<"notify">>, #postback_notify{ data = Data } = Notify, Context) ->
    % Notify for observer
    case maybe_set_q(mqtt, Data, Context) of
        {ok, Context1} ->
            Context2 = case z_notifier:first(Notify, Context1) of
                            undefined -> Context1;
                            #context{} = ContextNotify -> ContextNotify
                       end,
            incoming_context_result(Context2);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(Delegate, #postback_notify{ data = Data } = Notify, Context) ->
    % Notify for delegate
    case maybe_set_q(mqtt, Data, Context) of
        {ok, Context1} ->
            {ok, Module} = z_utils:ensure_existing_module(Delegate),
            incoming_context_result(Module:event(Notify, Context1));
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(<<"postback">>, Map, Context) when is_map(Map) ->
    Postback = jsxrecord:decode( maps:get(<<"z_postback">>, Map) ),
    transport(<<"postback">>, Postback, Context);
transport(<<"notify">>, Map, Context) when is_map(Map) ->
    Postback = jsxrecord:decode( maps:get(<<"z_postback">>, Map) ),
    transport(<<"postback">>, Postback, Context);
transport(Delegate, Payload, Context) ->
    % Event
    case z_utils:ensure_existing_module(Delegate) of
        {ok, Module} ->
            Msg = #z_msg_v1{
                data = map_to_list(Payload)
            },
            incoming_context_result(Module:event(Msg, Context));
        {error, _} ->
            lager:info("Unkwown delegate ~p for payload ~p", [Delegate, Payload]),
            incoming_context_result(Context)
    end.

map_to_list(Payload) when is_map(Payload) ->
    maps:to_list(Payload);
map_to_list(Payload) ->
    Payload.

%% @doc Send the accumulated actions (scripts) from the context back to the client.
incoming_context_result(Context) ->
    case iolist_to_binary( z_render:get_script(Context) ) of
        <<>> ->
            ok;
        Script ->
            % Send the script back to the client for evaluation
            reply(Script, Context)
    end.


incoming_postback_event(true, Module, Tag, Trigger, Target, Context) ->
    Context1 = case z_validation:validate_query_args(Context) of
        {ok, ContextEval} ->
            Module:event(#submit{ message=Tag, form=Trigger, target=Target }, ContextEval);
        {error, ContextEval} ->
            %% TODO: Posted form did not validate, return any errors.
            ContextEval
    end,
    z_render:wire({unmask, [{target, Trigger}]}, Context1);
incoming_postback_event(false, Module, Tag, Trigger, Target, Context) ->
    Module:event(#postback{ message=Tag, trigger=Trigger, target=Target }, Context).


is_submit_event(<<"submit">>) -> true;
is_submit_event("submit") -> true;
is_submit_event(submit) -> true;
is_submit_event(_Type) -> false.

maybe_set_q(form, Qs, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Type, #{ <<"q">> := Qs }, Context) when is_list(Qs) ->
    Qs1 = lists:foldr(
        fun
            (#{ <<"name">> := K, <<"value">> := V }, Acc) ->
                [ {K, V} | Acc ];
            (#{ <<"name">> := K }, Acc) ->
                [ {K, undefined} | Acc ]
        end,
        [],
        Qs),
    set_q(Qs1, Context);
maybe_set_q(_Type, {q, Qs}, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Type, _Data, Context) ->
    {ok, Context}.

set_q(Qs, Context) ->
    Context1 = z_context:add_q(Qs, Context),
    z_validation:validate_query_args(Context1).

