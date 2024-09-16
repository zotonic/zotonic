%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023  Marc Worrell
%% @doc Transport handling for wired postbacks.
%% @end

%% Copyright 2014-2023 Marc Worrell
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
    reply/2,
    reply_actions/1,
    reply_actions/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type payload() :: map()
                 | #postback_event{}
                 | #postback_notify{}.

%% @doc Send JS to the client, client will evaluate the JS. There must be either a valid
%% client_id in the context or a 'zotonic_topic_reply' query argument. The zotonic_topic_reply
%% is preferred over the client_id. Usually the zotonic_topic_reply is
%% "~client/zotonic-transport/eval". The script is transported using qos 1.
-spec reply(Javascript, Context) -> ok | {error, Reason} when
    Javascript :: iodata(),
    Context :: z:context(),
    Reason :: no_client | term().
reply(JavaScript, #context{ client_id = ClientId } = Context) ->
    Payload = iolist_to_binary(JavaScript),
    case z_convert:to_binary(z_context:get_q(<<"zotonic_topic_reply">>, Context)) of
        <<>> when is_binary(ClientId) ->
            Topic = [ <<"bridge">>, ClientId, <<"zotonic-transport">>, <<"eval">> ],
            z_mqtt:publish(Topic, Payload, #{ qos => 1 }, Context);
        <<>> ->
            {error, no_client};
        Topic ->
            z_mqtt:publish(Topic, Payload, #{ qos => 1 }, Context)
    end.

%% @doc Render the actions in the Context and send the rendered script to the user-agent
%% or to the topic in the query argument zotonic_topic_reply.
-spec reply_actions(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: no_client | term().
reply_actions(Context) ->
    Script = z_render:get_script(Context),
    reply(Script, Context).

-spec reply_actions(Actions, Context) -> ok | {error, Reason} when
    Actions :: z_render:action() | [ z_render:action() ],
    Context :: z:context(),
    Reason :: no_client | term().
reply_actions(Actions, Context) ->
    Context1 = z_context:new(Context),
    Context2 = z_render:wire(Actions, Context1),
    Script = z_render:get_script(Context2),
    reply(Script, Context).


%% @doc Handle incoming transport messages, call event handlers of delegates.
-spec transport( binary() | undefined, payload(), z:context()) -> ok | {error, term()}.
transport(<<"postback">>, #postback_event{ postback = Postback } = Event, Context) ->
    % submit or postback events
    {EventType, TriggerId, TargetId, Tag, Module} = z_crypto:depickle(Postback, Context),
    TriggerId1 = case TriggerId of
                    undefined -> Event#postback_event.trigger;
                    _         -> TriggerId
                 end,
    TargetId1 =  case TargetId of
                    undefined -> Event#postback_event.target;
                    _         -> TargetId
                 end,
    Context1 = maybe_set_csp(Event#postback_event.data, Context),
    case maybe_set_q(Event#postback_event.data, Context1) of
        {ok, Context2} ->
            TriggerValue = Event#postback_event.triggervalue,
            Context3 = z_context:set_q(<<"triggervalue">>, TriggerValue, Context2),
            ContextRsc = z_context:set_controller_module(Module, Context3),
            ContextRes = incoming_postback_event(is_submit_event(EventType), Module, Tag, TriggerId1, TargetId1, ContextRsc),
            incoming_context_result(ContextRes);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(<<"notify">>, #postback_notify{ data = Data } = Notify, Context) ->
    % Notify for observer
    Context1 = maybe_set_csp(Data, Context),
    case maybe_set_q(Data, Context1) of
        {ok, Context2} ->
            Context3 = case z_notifier:first(Notify, Context2) of
                            undefined -> Context2;
                            #context{} = ContextNotify -> ContextNotify
                       end,
            incoming_context_result(Context3);
        {error, ContextValidation} ->
            incoming_context_result(ContextValidation)
    end;
transport(Delegate, #postback_notify{ data = Data } = Notify, Context) ->
    % Notify for delegate
    Context1 = maybe_set_csp(Data, Context),
    case maybe_set_q(Data, Context1) of
        {ok, Context2} ->
            case z_utils:ensure_existing_module(Delegate) of
                {ok, Module} ->
                    incoming_context_result(Module:event(Notify, Context2));
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonc_core,
                        text => <<"Unknown delegate for postback_notify">>,
                        result => error,
                        reason => Reason,
                        delegate => Delegate
                    }),
                    incoming_context_result(Context)
            end;
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
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => zotonic_core,
                text => <<"Unknown delegate for payload">>,
                result => error,
                reason => Reason,
                delegate => Delegate,
                payload => Payload
            }),
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

maybe_set_csp(#{ <<"csp_nonce">> := Nonce }, Context) when is_binary(Nonce), Nonce =/= <<>> ->
    z_context:set_csp_nonce(Nonce, Context);
maybe_set_csp(_, Context) ->
    Context.

maybe_set_q(#{ <<"q">> := Qs }, Context) when is_list(Qs) ->
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
maybe_set_q({q, Qs}, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Data, Context) ->
    {ok, Context}.

set_q(Qs, Context) ->
    Context1 = z_context:add_q(Qs, Context),
    z_validation:validate_query_args(Context1).

