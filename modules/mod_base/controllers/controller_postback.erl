%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2014 Marc Worrell
%% @doc Handles Ajax and form posts

%% Copyright 2009-2014 Marc Worrell
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

-module(controller_postback).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    allowed_methods/1,
    content_types_provided/1,
    process_post/1
]).

-include_lib("zotonic.hrl").

% This must be lower than COMET_FLUSH_DATA in z_transport_comet.erl
-define(SIDEJOB_TIMEOUT, 90).

service_available(Context) ->
    Context1 = z_context:continue_session(Context),
    z_context:lager_md(Context1),
    {true, Context1}.

allowed_methods(Context) ->
    {[<<"POST">>], Context}.

content_types_provided(Context) ->
    % The user-agent won't handle the result of non-ajax form posts.
    % Suppress text/x-ubf as that will give problems with Firefox (issue #1230)
    case z_context:get_req_header(<<"content-type">>, Context) of
        <<"multipart/form-data", _/binary>> ->
            {[{<<"text/plain">>, undefined}], Context};
        <<"application/x-www-form-urlencoded", _/binary>> ->
            {[{<<"text/plain">>, undefined}], Context};
        _ ->
            {[{<<"text/x-ubf">>, undefined},
              {<<"text/plain">>, undefined}], Context}
    end.

process_post(Context) ->
    case z_context:get_req_header(<<"content-type">>, Context) of
        <<"text/x-ubf", _/binary>> ->
            process_post_ubf(Context);
        <<"text/plain", _/binary>> ->
            process_post_ubf(Context);
        <<"application/x-www-form-urlencoded", _/binary>> ->
            process_post_form(Context);
        <<"multipart/form-data", _/binary>> ->
            process_post_form(Context);
        _ ->
            {{halt, 415}, Context}
    end.

%% @doc AJAX postback, the received data is UBF which can be directly decoded
%% and handled by the z_transport:incoming/2 routines.
process_post_ubf(Context) ->
    {Data, Context1} = cowmachine_req:req_body(Context),
    {ok, Term, _Rest} = z_transport:data_decode(Data),
    {ok, Rs, ContextRs} = case z_context:get_q(<<"transport">>, Context) of
        <<"ajax">> ->
            % Forced an ajax request (probably for cookies) so process this
            % in the requestor process.
            z_transport:incoming(Term, Context);
        _ ->
            Context2 = z_transport:prepare_incoming_context(Term, Context1),
            case z_session:job(Term, Context2) of
                {ok, Pid} when is_pid(Pid) ->
                    % We wait a bit for some quick results, as otherwise the
                    % comet connection is used to transport the result.
                    MRef = erlang:monitor(process, Pid),
                    receive
                        {'DOWN', MRef, process, _Pid, _Reason} ->
                            {ok, [], Context2}
                    after
                        ?SIDEJOB_TIMEOUT ->
                            erlang:demonitor(MRef),
                            {ok, [], Context2}
                    end;
                {ok, JobMsgs, JobContext} ->
                    {ok, JobMsgs, JobContext};
                {error, overload} ->
                    {ok, z_transport:maybe_ack(overload, Term, Context2), Context2}
            end
    end,
    Rs1 = case z_session_page:get_transport_msgs(ContextRs) of
              [] -> Rs;
              Msgs -> mklist(Rs) ++ mklist(Msgs)
          end,
    {ok, ReplyData} = z_transport:data_encode(Rs1),
    post_return(ReplyData, ContextRs).

mklist(L) when is_list(L) -> L;
mklist(V) -> [V].

%% @doc A HTML form, we have to re-constitute the postback before calling z_transport:incoming/2
%%      All return data is sent via the comet or subsequent "normal" postback connection.
process_post_form(Context) ->
    Context1 = z_context:ensure_qs(Context),
    case z_context:get_q(<<"z_msg">>, Context1) of
        undefined ->
            % A "normal" post of a form, no javascript involved
            Event = #submit{
                        message = z_context:get_q(<<"z_message">>, Context1),
                        target = z_context:get_q(<<"z_target_id">>, Context1)
                    },
            Context2 = notify_submit(z_context:get_q(<<"z_delegate">>, Context), Event, Context1),
            {Script, Context3} = z_script:split(Context2),
            case Script of
                <<>> ->  nop;
                _JS -> z_transport:page(javascript, Script, Context3)
            end,
            post_form_return(Context3);
        #z_msg_v1{} = Msg ->
            Context2 = z_transport:prepare_incoming_context(Msg, Context1),
            {ok, Reply, Context3} = z_transport:incoming(Msg, Context2),
            z_transport:transport(Reply, Context3),
            post_form_return(Context3)
    end.

post_form_return(Context) ->
    % Make sure that we return 200, otherwise the form onload is not triggered.
    post_return(<<"#$">>, Context).

notify_submit(None, Event, Context) when None =:= undefined; None =:= <<>>; None =:= [] ->
    case z_notifier:first(Event, Context) of
        #context{} = Context1 -> Context1;
        undefined -> Context
    end;
notify_submit(Delegate, Event, Context) ->
    {ok, Module} = z_utils:ensure_existing_module(Delegate),
    Module:event(Event, Context).

post_return(Data, Context) ->
    Context1 = cowmachine_req:set_resp_body(Data, Context),
    {true, Context1}.
