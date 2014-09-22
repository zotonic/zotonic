%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Transport data to the user-agent(s) connected to an user, session or page.

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

-module(z_transport).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    page/3,
    page/4,
    session/3,
    session/4,
    user/3,
    user/4,
    
    msg/4,

    transport/2,
    transport_user/3,
    transport_session/3,
    transport_page/3,

    data_decode/1,
    data_encode/1,

    incoming/2
    ]).

-include_lib("zotonic.hrl").

-record(session_state, {
        page_id,
        user_id
    }).

%%% ---------------------------------------------------------------------------------------
%%% Send data API
%%% ---------------------------------------------------------------------------------------

%% @doc Convenience function to send data to the current page
page(Delegate, Data, Context) ->
    page(Delegate, Data, [], Context).

%% @doc Convenience function to send data to the current page
page(Delegate, Data, Options, Context) ->
    Msg = msg(page, Delegate, Data, Options),
    transport(Msg, Context).

%% @doc Convenience function to send data to all pages of the current session
session(Delegate, Data, Context) ->
    session(Delegate, Data, [], Context).

%% @doc Convenience function to send data to all pages of the current session
session(Delegate, Data, Options, Context) ->
    Msg = msg(session, Delegate, Data, Options),
    transport(Msg, Context).

%% @doc Convenience function to send data to all sessions of the current user
user(Delegate, Data, Context) ->
    user(Delegate, Data, [], Context).

%% @doc Convenience function to send data to all sessions of the current user
user(Delegate, Data, Options, Context) ->
    Msg = msg(user, Delegate, Data, Options),
    transport(Msg, Context).

%% @doc Fill a transport message, for sending with the transport functions.
msg(Queue, javascript, Data, Options) when not is_binary(Data) ->
    msg(Queue, javascript, z_convert:to_binary(Data), Options);
msg(Queue, Delegate, Data, Options) ->
    #z_msg_v1{
        qos=proplists:get_value(qos, Options, 0),
        msg_id=z_convert:to_binary(z_ids:id(32)),
        timestamp=now_msec(),
        delegate=Delegate,
        push_queue=Queue,
        content_type=ubf,
        data=Data
    }.

%% @doc Put a message or ack on transport.
transport(L, Context) when is_list(L) ->
    lists:foreach(fun(Msg) ->
                    transport(Msg, Context)
                  end,
                  L);
transport(Msg, Context) ->
    case Msg#z_msg_v1.push_queue of
        session -> z_session:transport(Msg, Context);
        page -> z_session_page:transport(Msg, Context);
        user -> transport_user(Msg, z_acl:user(Context), Context)
    end.

%% @doc Put a message or ack on transport to all sessions of the given user
transport_user(_Msg, undefined, _Context) ->
    ok;
transport_user(Msg, UserId, Context) ->
    SessionPids = z_session_manager:whereis_user(UserId, Context),
    lists:foreach(
            fun(Pid) ->
                z_session:transport(Msg#z_msg_v1{push_queue=session}, Pid)
            end,
            SessionPids).

%% @doc Put a message or ack on transport to all pages of the given session.
transport_session(Msg, SessionPid, _Context) when is_pid(SessionPid) ->
    z_session:transport(Msg, SessionPid);
transport_session(Msg, SessionId, Context) ->
    z_session:transport(Msg, z_session_manager:whereis(SessionId, Context)).

%% @doc Put a message or ack on transport to a specific page.
transport_page(Msg, PagePid, _Context) when is_pid(PagePid) ->
    z_session_page:transport(Msg, PagePid);
transport_page(Msg, PageId, Context) ->
    z_session_page:transport(Msg, PageId, Context).


%%% ---------------------------------------------------------------------------------------
%%% Receive data API
%%% ---------------------------------------------------------------------------------------

-spec data_decode(binary()) -> {ok, any(), binary()} | {error, term()}.
%% @doc Decoding is done in this module, as it also defines the various atoms.
data_decode(Bin) ->
    z_ubf:decode(Bin).

-spec data_encode(any()) -> {ok, binary()}.
data_encode(Data) ->
    z_ubf:encode(Data).


%% @doc Handle incoming messages, originates from various sources, like postback, websocket etc
-spec incoming(list()|#z_msg_v1{}|#z_msg_ack{}, #context{}) -> {ok, list(), #context{}}.
incoming(Msg, Context) ->
    {ok, Rs, Context1} = incoming_msgs(Msg, Context),
    {ok, maybe_session_status_msg(Rs, Context1), cleanup_context(Context1, Context)}.

cleanup_context(ContextRes, Context) ->
    ContextRes#context{
        props=Context#context.props,
        updates=[],
        actions=[],
        content_scripts=[],
        scripts=[],
        wire=[],
        validators=[],
        render=[]
    }.

incoming_msgs(L, Context) when is_list(L) ->
    lists:foldl(
        fun(Msg, {ok, Msgs, Ctx}) ->
            {ok, Msgs1, Ctx1} = incoming(Msg, Ctx),
            {ok, lists:flatten([Msgs,Msgs1]), Ctx1}
        end,
        {ok, [], Context},
        L);
incoming_msgs(#z_msg_v1{page_id=PageId, session_id=SessionId, data=Data, ua_class=UA, content_type=CT} = Msg, Context) ->
    Context1 = maybe_logon(maybe_set_sessions(SessionId, PageId, Context)),
    Context2 = maybe_set_uaclass(UA, Context1),
    try
        maybe_auth_change(incoming_1(Msg#z_msg_v1{data=decode_data(CT,Data)}, Context2), Context2)
    catch
        throw:Reason ->
            lager:error(z_context:lager_md(Context2),
                        "Throw '~p' for transport message ~p",
                        [Reason, Msg]),
            lager:error(z_context:lager_md(Context2),
                        "Stack: ~p", [erlang:get_stacktrace()]),
            {ok, maybe_ack({error, Reason}, Msg, Context2), Context2};
        error:Reason ->
            lager:error(z_context:lager_md(Context2),
                        "Error '~p' for transport message ~p",
                        [Reason, Msg]),
            lager:error(z_context:lager_md(Context2),
                        "Stack: ~p", [erlang:get_stacktrace()]),
            {ok, maybe_ack({error, error}, Msg, Context2), Context2}
    end;
incoming_msgs(#z_msg_ack{page_id=PageId, session_id=SessionId} = Ack, Context) ->
    Context1 = maybe_set_sessions(SessionId, PageId, Context),
    case Ack#z_msg_ack.push_queue of
        page -> z_session_page:receive_ack(Ack, Context);
        session -> z_session:receive_ack(Ack, Context)
    end,
    {ok, [], Context1}.


%%% ---------------------------------------------------------------------------------------
%%% Internal functions
%%% ---------------------------------------------------------------------------------------

incoming_1(#z_msg_v1{delegate='$ping'} = Msg, Context) ->
    {ok, maybe_ack(pong, Msg, Context), Context};
incoming_1(#z_msg_v1{delegate= <<"$ping">>} = Msg, Context) ->
    {ok, maybe_ack(pong, Msg, Context), Context};
incoming_1(#z_msg_v1{delegate=session, data= <<"check">>}, Context) ->
    {ok, session_status_message(Context), Context};
incoming_1(#z_msg_v1{delegate=session, data= <<"ensure">>}, Context) ->
    {ok, Reply, Context1} = session_status_ensure(Context),
    {ok, Reply, Context1};
incoming_1(Msg, #context{session_pid=SPid, page_pid=PPid} = Context) when is_pid(SPid), is_pid(PPid) ->
    incoming_with_session(Msg, Context);
incoming_1(_Msg, Context) ->
    {ok, session_status_message(Context), Context}.


incoming_with_session(#z_msg_v1{delegate=postback, data=#postback_event{} = Pb} = Msg, Context) ->
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Pb#postback_event.postback, Context),
    TriggerId1 = case TriggerId of
                    undefined -> Pb#postback_event.trigger;
                    _         -> TriggerId
                 end,
    TargetId1 =  case TargetId of
                    undefined -> Pb#postback_event.target;
                    _         -> TargetId
                 end,
    case maybe_set_q(ubf, Pb#postback_event.data, Context) of
        {ok, Context1} ->
            Context2 = z_context:set_q("triggervalue", to_list(Pb#postback_event.triggervalue), Context1),
            ContextRsc = z_context:set_controller_module(Module, Context2),
            ContextRes = incoming_postback_event(z_convert:to_binary(EventType), Module, Tag, TriggerId1, TargetId1, ContextRsc),
            incoming_context_result(ok, Msg, ContextRes);
        {error, ContextValidation} ->
            incoming_context_result(ok, Msg, ContextValidation)
    end;
incoming_with_session(#z_msg_v1{delegate=mqtt, data=Data} = Msg, Context) ->
    Result = z_mqtt:transport_incoming(Data, Context),
    incoming_context_result(Result, Msg, Context);
incoming_with_session(#z_msg_v1{delegate=notify, content_type=Type, data=#postback_notify{} = Notify} = Msg, Context) ->
    case maybe_set_q(Type, Notify#postback_notify.data, Context) of
        {ok, Context1} ->
            % MochiWeb compatible values...
            Notify1 = Notify#postback_notify{
                            message=to_list(Notify#postback_notify.message),
                            trigger=to_list(Notify#postback_notify.trigger),
                            target=to_list(Notify#postback_notify.target)
                      },
            Context2 = case z_notifier:first(Notify1, Context1) of
                            undefined -> Context1;
                            #context{} = ContextNotify -> ContextNotify
                       end,
            incoming_context_result(ok, Msg, Context2);
        {error, ContextValidation} ->
            incoming_context_result(ok, Msg, ContextValidation)
    end;
incoming_with_session(#z_msg_v1{delegate=Delegate, content_type=Type, data=#postback_notify{} = Notify} = Msg, Context) ->
    case maybe_set_q(Type, Notify#postback_notify.data, Context) of
        {ok, Context1} ->
            {ok, Module} = z_utils:ensure_existing_module(Delegate),
            % MochiWeb compatible values...
            Notify1 = Notify#postback_notify{
                            message=to_list(Notify#postback_notify.message),
                            trigger=to_list(Notify#postback_notify.trigger),
                            target=to_list(Notify#postback_notify.target)
                      },
            incoming_context_result(ok, Msg, Module:event(Notify1, Context1));
        {error, ContextValidation} ->
            incoming_context_result(ok, Msg, ContextValidation)
    end;
incoming_with_session(#z_msg_v1{delegate=Delegate} = Msg, Context) when is_atom(Delegate); is_binary(Delegate) ->
    {ok, Module} = z_utils:ensure_existing_module(Delegate),
    incoming_context_result(ok, Msg, Module:event(Msg, Context)).


incoming_postback_event(<<"submit">>, Module, Tag, Trigger, Target, Context) -> 
    case z_validation:validate_query_args(Context) of
        {ok, ContextEval} ->   
            Module:event(#submit{message=Tag, form=Trigger, target=Target}, ContextEval);
        {error, ContextEval} ->
            %% TODO: Posted form did not validate, return any errors.
            ContextEval
    end;
incoming_postback_event(_Other, Module, Tag, Trigger, Target, Context) -> 
    Module:event(#postback{message=Tag, trigger=Trigger, target=Target}, Context).

incoming_context_result(Result, Msg, Context) ->
    OptAck = maybe_ack(Result, Msg, Context),
    {Script, ContextClean} = z_script:split(Context),
    case iolist_to_binary(Script) of
        <<>> -> 
            {ok, OptAck, ContextClean};
        ScriptBin ->
            {ok, lists:flatten([OptAck, msg(page, javascript, ScriptBin, [{qos,0}])]), ContextClean}
    end.

maybe_set_sessions(SessionId, PageId, Context) ->
    maybe_set_page_session(
        PageId,
        maybe_set_session(
            SessionId,
            Context)).

maybe_logon(#context{session_pid=undefined} = Context) ->
    Context;
maybe_logon(Context) ->
    UserId = z_acl:user(Context),
    try
        case z_context:get_session(auth_user_id, Context) of
            UserId when is_integer(UserId) ->
                z_memo:set_userid(UserId),
                Context;
            none when is_integer(UserId) ->
                z_memo:set_userid(undefined),
                z_acl:logoff(Context);
            _ ->
                z_auth:logon_from_session(Context)
        end
    catch
        exit:{noproc, _} ->
            lager:info("PageId without page session (session_id ~p)", [Context#context.session_id]),
            z_acl:logoff(Context#context{session_pid=undefined, page_pid=undefined})
    end.


maybe_set_session(undefined, Context) ->
    Context;
maybe_set_session(<<>>, Context) ->
    Context;
maybe_set_session(SessionId, #context{session_id=SessionId} = Context) ->
    Context;
maybe_set_session(SessionId, Context) ->
    {ok, Context1} = z_session_manager:start_session(optional, SessionId, Context),
    Context1.

maybe_set_page_session(undefined, Context) ->
    Context;
maybe_set_page_session(<<>>, Context) ->
    Context;
maybe_set_page_session(_PageId, #context{session_pid=undefined, session_id=SessionId} = Context) ->
    lager:info("PageId without page session (session_id ~p)", [SessionId]),
    Context;
maybe_set_page_session(PageId, #context{page_id=PageId} = Context) ->
    Context;
maybe_set_page_session(PageId, Context) ->
    try
        case z_session:lookup_page_session(PageId, Context) of
            {ok, Pid} ->
                Context#context{page_id=PageId, page_pid=Pid};
            {error, notfound} ->
                % TODO: decide if we should stop here... an unknown page was referenced
                Context
        end
    catch
        exit:{noproc, _} ->
            lager:info("PageId without page session (session_id ~p)", [Context#context.session_id]),
            Context#context{session_pid=undefined}
    end.

maybe_set_uaclass(undefined, Context) ->
    Context;
maybe_set_uaclass(UA, #context{ua_class=UA} = Context) ->
    Context;
maybe_set_uaclass(UA, Context) when is_atom(UA) ->
    z_user_agent:set_class(UA, Context).

maybe_set_q(form, Qs, Context) -> 
    set_q(Qs, Context);
maybe_set_q(_Type, {q, Qs}, Context) ->
    set_q(Qs, Context);
maybe_set_q(_Type, _Data, Context) ->
    {ok, Context}.


%% If an user authenticated during this request then the user-agent needs to re-connect
maybe_auth_change({ok, Msgs, ContextOut} = Res, ContextIn) ->
    case is_auth_change(ContextOut, ContextIn) of
        true ->
            % Assume the new session-id comes along in the z_sid cookie
            StatusMsg = msg(page, session, 
                            #session_state{page_id=ContextOut#context.page_id, user_id=ContextOut#context.user_id},
                            []),
            {ok, lists:flatten([StatusMsg,Msgs]), ContextOut};
        false ->
            Res
    end;
maybe_auth_change(Res, _ContextIn) ->
    Res.

is_auth_change(#context{session_id=U1}, #context{session_id=U2}) when U1 =/= U2 ->
    true;
is_auth_change(#context{page_id=U1}, #context{page_id=U2}) when U1 =/= U2 ->
    true;
is_auth_change(#context{user_id=U1}, #context{user_id=U2}) when U1 =/= U2 ->
    true;
is_auth_change(#context{}, #context{}) ->
    false.

maybe_session_status_msg(Result, #context{session_pid=SessionPid, page_pid=PagePid}) when is_pid(SessionPid), is_pid(PagePid) ->
    Result;
maybe_session_status_msg(Result, #context{session_pid=SessionPid}) when not is_pid(SessionPid) ->
    [msg(undefined, session, <<"session_invalid">>, []) | mklist(Result)];
maybe_session_status_msg(Result, #context{page_pid=PagePid}) when not is_pid(PagePid) ->
    [msg(undefined, session, <<"page_invalid">>, []) | mklist(Result)].

session_status_message(Context) ->
    case {is_pid(Context#context.session_pid), is_pid(Context#context.page_pid)} of
        {false,_} -> msg(undefined, session, <<"session_invalid">>, []);
        {_,false} -> msg(undefined, session, <<"page_invalid">>, []);
        {true, true} -> msg(undefined, session, <<"ok">>, [])
    end.

session_status_ensure(Context) ->
    Context1 = z_context:ensure_all(Context),
    Reply = msg(page,
                session, 
                #session_state{page_id=Context1#context.page_id, user_id=z_acl:user(Context1)},
                []),
    {ok, Reply, Context1}.


%% For MochiWeb we need to convert to strings
set_q(Qs, Context) ->
    Qs1 = [ {to_list(K), to_list(V)} || {K,V} <- Qs ],
    z_validation:validate_query_args(
        z_context:set('q', Qs1, Context)).

%% TODO: This can be removed when we switch to binary key/values for qs
to_list(B) when is_binary(B) -> z_convert:to_list(B);
to_list(V) -> V.

decode_data(ubf, Data) ->
    Data;
decode_data(form, Data) when is_binary(Data) ->
    mochiweb_util:parse_qs(Data);
decode_data(json, Data) when is_binary(Data) ->
    mochijson2:decode(Data);
decode_data(_Other, Data) ->
    Data.

maybe_ack(Result, #z_msg_v1{qos=N, msg_id=MsgId}, Context) when N > 0; Result =/= undefined ->
    #z_msg_ack{
        qos=N, 
        msg_id=MsgId, 
        result=Result,
        page_id=Context#context.page_id
    };
maybe_ack(_Result, #z_msg_v1{}, _Context) ->
    [].

mklist(L) when is_list(L) -> L;
mklist(V) -> [V].

now_msec() ->
    {A,B,C} = os:timestamp(),
    (A*1000000+B)*1000 + C div 1000.
