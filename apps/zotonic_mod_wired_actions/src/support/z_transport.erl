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

%% @doc Send JS to the client, client will evaluate the JS.
-spec reply(binary(), z:context()) -> ok | {error, term()}.
reply(JavaScript, #context{ client_id = ClientId } = Context) when is_binary(ClientId), is_binary(JavaScript) ->
    Topic = [ <<"bridge">>, ClientId, <<"zotonic-transport">>, <<"eval">> ],
    z_mqtt:publish(Topic, JavaScript, #{ qos => 0 }, Context).

%% @doc Handle incoming transport messages, call event handlers of delegates.
-spec transport( binary() | undefined, map(), z:context()) -> ok | {error, term()}.
transport(<<"postback">>, #{ <<"type">> := <<"postback_event">>, <<"postback">> := Postback } = Payload, Context) ->
    % submit or postback events
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context),
    TriggerId1 = case TriggerId of
                    undefined -> maps:get(<<"trigger">>, Payload, undefined);
                    _         -> TriggerId
                 end,
    TargetId1 =  case TargetId of
                    undefined -> maps:get(<<"target">>, Payload, undefined);
                    _         -> TargetId
                 end,
    case maybe_set_q(mqtt, maps:get(<<"data">>, Payload, undefined), Context) of
        {ok, Context1} ->
            TriggerValue = maps:get(<<"triggervalue">>, Payload, undefined),
            Context2 = z_context:set_q(<<"triggervalue">>, TriggerValue, Context1),
            ContextRsc = z_context:set_controller_module(Module, Context2),
            ContextRes = incoming_postback_event(is_submit_event(EventType), Module, Tag, TriggerId1, TargetId1, ContextRsc),
            incoming_context_result(ContextRes);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(<<"notify">>, #{ <<"type">> := <<"postback_notify">>, <<"data">> := Data } = Payload, Context) ->
    % Notify for observer
    case maybe_set_q(mqtt, Data, Context) of
        {ok, Context1} ->
            Notify = #postback_notify{
                message = maps:get(<<"message">>, Payload, undefined),
                data = Data,
                trigger = maps:get(<<"trigger">>, Payload, undefined),
                target = maps:get(<<"target">>, Payload, undefined)
            },
            Context2 = case z_notifier:first(Notify, Context1) of
                            undefined -> Context1;
                            #context{} = ContextNotify -> ContextNotify
                       end,
            incoming_context_result(Context2);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(Delegate, #{ <<"type">> := <<"postback_notify">>, <<"data">> := Data } = Payload, Context) ->
    % Notify for delegate
    case maybe_set_q(mqtt, Data, Context) of
        {ok, Context1} ->
            {ok, Module} = z_utils:ensure_existing_module(Delegate),
            Notify = #postback_notify{
                message = maps:get(<<"message">>, Payload, undefined),
                data = Data,
                trigger = maps:get(<<"trigger">>, Payload, undefined),
                target = maps:get(<<"target">>, Payload, undefined)
            },
            incoming_context_result(Module:event(Notify, Context1));
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(Delegate, Payload, Context) ->
    % Event
    {ok, Module} = z_utils:ensure_existing_module(Delegate),
    Msg = #z_msg_v1{
        data = Payload
    },
    incoming_context_result(Module:event(Msg, Context)).


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
    case z_validation:validate_query_args(Context) of
        {ok, ContextEval} ->
            Module:event(#submit{ message=Tag, form=Trigger, target=Target }, ContextEval);
        {error, ContextEval} ->
            %% TODO: Posted form did not validate, return any errors.
            ContextEval
    end;
incoming_postback_event(false, Module, Tag, Trigger, Target, Context) ->
    Module:event(#postback{ message=Tag, trigger=Trigger, target=Target }, Context).


is_submit_event(<<"submit">>) -> true;
is_submit_event("submit") -> true;
is_submit_event(submit) -> true;
is_submit_event(_Type) -> false.

maybe_set_q(form, Qs, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Type, {q, Qs}, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Type, _Data, Context) ->
    {ok, Context}.

set_q(Qs, Context) ->
    Qs0 = case z_context:get('q', Context) of
             L when is_list(L) -> L;
             _ -> []
          end,
    Qs1 = Qs ++ Qs0,
    z_validation:validate_query_args(
        z_context:set('q', Qs1, Context)).


