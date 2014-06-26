%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2011 Marc Worrell
%% @doc Handles all ajax postback calls

%% Copyright 2009-2011 Marc Worrell
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
    forbidden/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2,
    
    process_postback/1
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    z_context:lager_md(Context1),
    ?WM_REPLY(true, Context1).


malformed_request(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(Context1),
    case z_context:get_q("postback", Context2) of
        undefined ->
            ?WM_REPLY(true, Context2);
        _ ->
            ?WM_REPLY(false, Context2)
    end.

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    %% Ensure all, but don't start a new session
    Context2 = z_context:set(no_session, true, Context1),
    Context3 = z_context:ensure_all(Context2),
    z_context:lager_md(Context3),
    ?WM_REPLY(not z_context:has_session(Context3), Context3).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Script, EventContext} = process_postback(Context1),
    CometScript = case z_context:has_session_page(EventContext) of
                      true -> z_session_page:get_scripts(EventContext#context.page_pid);
                      false -> []
                  end,
    
    % Send back all the javascript.
    RD  = z_context:get_reqdata(EventContext),
    RD1 = case wrq:get_req_header_lc("content-type", ReqData) of
        "multipart/form-data" ++ _ ->
            RDct = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD),
            case z_context:document_domain(EventContext) of
                undefined ->
                    wrq:append_to_resp_body([
                            "<textarea>", Script, CometScript, "</textarea>"
                            ], RDct);
                DocumentDomain ->
                    wrq:append_to_resp_body([
                            <<"<script>document.domain=\"">>, DocumentDomain,<<"\";</script><textarea>">>,
                            Script, CometScript, "</textarea>"
                            ], RDct)
            end;
        _ ->
            wrq:append_to_resp_body([Script, CometScript], RD)
    end,

    ReplyContext = z_context:set_reqdata(RD1, EventContext),
    ?WM_REPLY(true, ReplyContext).



%% @doc Process the postback, shared with the controller_websocket.
process_postback(Context1) ->
    EventContext = case z_context:get_q("postback", Context1) of
        "notify" ->
            Message = z_context:get_q("z_msg", Context1),
            TriggerId1 = case z_context:get_q("z_trigger_id", Context1) of
                             undefined -> undefined;
                             [] -> undefined;
                             TrId -> TrId
                         end,
            TargetId = case z_context:get_q("z_target_id", Context1) of
                             undefined -> undefined;
                             [] -> undefined;
                             TtId -> TtId
                         end,
            PostbackNotify = #postback_notify{message=Message, trigger=TriggerId1, target=TargetId},
            case z_context:get_q("z_delegate", Context1) of
                None when None =:= []; None =:= <<>>; None =:= undefined ->
                    case z_notifier:first(PostbackNotify, Context1) of
                        undefined -> Context1;
                        #context{} = ContextNotify -> ContextNotify
                    end;
                Delegate ->
                    {ok, Module} = z_utils:ensure_existing_module(Delegate),
                    Module:event(PostbackNotify, Context1)
            end;
        Postback ->
            {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context1),
            TriggerId1 = case TriggerId of
                undefined -> z_context:get_q("z_trigger_id", Context1);
                _         -> TriggerId
            end,
            ContextRsc = z_context:set_controller_module(Module, Context1),
            case EventType of
                "submit" -> 
                    case z_validation:validate_query_args(ContextRsc) of
                        {ok, ContextEval} ->   
                            Module:event(#submit{message=Tag, form=TriggerId1, target=TargetId}, ContextEval);
                        {error, ContextEval} ->
                            %% Posted form did not validate, return any errors.
                            ContextEval
                    end;
                _ -> 
                    Module:event(#postback{message=Tag, trigger=TriggerId1, target=TargetId}, ContextRsc)
            end
    end,

    % Remove the busy mask from the element that triggered this event.
    Unmask = case TriggerId1 of
                 undefined -> [];
                 "" -> [];
                 _HtmlElementId ->
                     [" z_unmask('", z_utils:js_escape(TriggerId1), "');"]
             end,
    Script = iolist_to_binary([z_script:get_script(EventContext), Unmask]),
    {Script, EventContext}.
