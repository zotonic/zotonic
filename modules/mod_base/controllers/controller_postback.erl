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
    init/1, 
    service_available/2,
    allowed_methods/2,
    process_post/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:continue_all(z_context:set(DispatchArgs, Context)),
    z_context:lager_md(Context1),
    ?WM_REPLY(true, Context1).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    case wrq:get_req_header_lc("content-type", ReqData) of
        "text/x-ubf" ++ _ ->
            process_post_ubf(ReqData, Context);
        "text/plain" ++ _ ->
            process_post_ubf(ReqData, Context);
        "application/x-www-form-urlencoded" ++ _ ->
            process_post_form(ReqData, Context);
        "multipart/form-data" ++ _ ->
            process_post_form(ReqData, Context);
        _ ->
            {{halt, 415}, ReqData, Context}
    end. 

%% @doc AJAX postback, the received data is UBF which can be directly decoded
%% and handled by the z_transport:incoming/2 routines. 
process_post_ubf(ReqData, Context) ->
    {Data,RD1} = wrq:req_body(ReqData),
    Context1 = ?WM_REQ(RD1, Context),
    {ok, Term, _Rest} = z_ubf:decode(Data),
    {ok, Reply, Context2} = z_transport:incoming(Term, Context1),
    {ok, ReplyData} = z_ubf:encode(Reply), 
    post_return(ReplyData, Context2).

%% @doc A HTML form, we have to re-constitute the postback before calling z_transport:incoming/2 
process_post_form(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    case z_context:get_q("postback", Context) of
        undefined ->
            % A "nornal" post of a form, no javascript involved
            Event = #submit{
                        message = z_context:get_q("z_message", Context),
                        target = z_context:get_q("z_target_id", Context)
                    },
            Context1 = notify_submit(z_context:get_q("z_delegate", Context), Event, Context),
            {Script, Context2} = z_script:split(Context1),
            case Script of
                <<>> ->  nop;
                _JS -> z_transport:page(javascript, Script, Context2)
            end,
            post_return(<<>>, Context2);
        Postback ->
            % A wired postback of a form
            Msg = #z_msg_v1{
                        qos=0,
                        content_type=form,
                        delegate=postback,
                        data=#postback_event{
                                postback=Postback,
                                trigger=z_context:get_q("z_trigger_id", Context),
                                target=z_context:get_q("z_target_id", Context),
                                triggervalue=z_context:get_q("triggervalue", Context)
                        }
                  },
            {ok, Reply, Context1} = z_transport:incoming(Msg, Context),
            z_transport:transport(Reply, Context1),
            post_return(<<>>, Context1)
    end.

notify_submit(None, Event, Context) when None =:= undefined; None =:= <<>>; None =:= [] ->
    case z_notifier:first(Event, Context) of
        #context{} = Context1 -> Context1;
        undefined -> Context
    end;
notify_submit(Delegate, Event, Context) ->
    {ok, Module} = z_utils:ensure_existing_module(Delegate),
    Module:event(Event, Context).

% Make sure that we retun 200, otherwise the form onload is not triggered.
post_return(<<>>, Context) ->
    {x, RD, Context1} = ?WM_REPLY(x, Context),
    RD1 = case is_empty(wrq:resp_body(RD)) of
            true -> wrq:append_to_resp_body(<<" ">>, RD);
            false -> RD
          end,
    {true, RD1, Context1};
post_return(Data, Context) ->
    {x, RD, Context1} = ?WM_REPLY(x, Context),
    RD1 = wrq:append_to_resp_body(Data, RD),
    {true, RD1, Context1}.

is_empty(<<>>) -> true;
is_empty([]) -> true;
is_empty(undefined) -> true;
is_empty(_) -> false.

