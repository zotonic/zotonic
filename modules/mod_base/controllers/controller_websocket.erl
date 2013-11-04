%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @doc WebSocket connections

%% Copyright 2010 Marc Worrell
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

-module(controller_websocket).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1, 
    forbidden/2,
    upgrades_provided/2,
    charsets_provided/2,
    content_types_provided/2,
    provide_content/2,
    websocket_start/2,    
    websocket_send_data/2
]).

% websocket handler exports.
-export([
    websocket_init/1,
    websocket_message/3,
    websocket_info/2,
    websocket_terminate/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

%% @doc The request must have a valid session cookie.
forbidden(ReqData, DispatchArgs) ->
    Context = z_context:new(ReqData),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:continue_session(Context1),
    ?WM_REPLY(not z_context:has_session(Context2), Context2).

%% @doc Possible connection upgrades
upgrades_provided(ReqData, Context) ->
    {[
        {"WebSocket", websocket_start}
    ], ReqData, Context}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

provide_content(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_qs(Context1),
    Context3 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context2),
    Rendered = z_template:render("error_websocket.tpl", z_context:get_all(Context), Context3),
    {Output, OutputContext} = z_context:output(Rendered, Context3),
    ?WM_REPLY(Output, OutputContext).

%% @doc Initiate the websocket connection upgrade
websocket_start(ReqData, Context) ->
    ContextReq = ?WM_REQ(ReqData, Context),
    Context1 = z_context:ensure_all(ContextReq),
    Context2 = case z_context:get(ws_handler, Context1) of
        undefined ->
            z_context:set(ws_handler, ?MODULE, Context1);
        _Hdlr -> Context1
    end,
    case z_context:get_req_header("sec-websocket-version", Context2) of
        undefined ->
            case z_context:get_req_header("sec-websocket-key1", Context2) of
                undefined ->
                    z_websocket_hixie75:start(ReqData, Context2);
                WsKey1 ->
                    z_websocket_hybi00:start(WsKey1, ReqData, Context2)
            end;
        "7" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07
            z_websocket_hybi17:start(ReqData, Context2);
        "8" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-10
            z_websocket_hybi17:start(ReqData, Context2);
        "13" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
            z_websocket_hybi17:start(ReqData, Context2)
    end.

%% @doc Send Data over websocket Pid to the client.
websocket_send_data(Pid, Data) ->
    Pid ! {send_data, Data}.

%% Called during initialization of the websocket.
websocket_init(_Context) ->
    ok.

%% Handle a message from the browser, should contain an url encoded request. Sends result script back to browser.
websocket_message(<<"Z:PING:",PingId/binary>>, SenderPid, Context) ->
    PageId = z_convert:to_binary(Context#context.page_id),
    case PingId of
        PageId ->
            % Ping for this page-id, reply with a pong
            z_session_page:websocket_attach(SenderPid, Context),
            SenderPid ! {send_data, iolist_to_binary(["Z:PONG:",PageId])},
            ok;
        _Other ->
            % Ping for wrong page-id, stay silent.
            ok
    end;
websocket_message(Msg, _SenderPid, Context) ->
    Qs = mochiweb_util:parse_qs(Msg),
    Context1 = z_context:set('q', Qs, Context),

    {ResultScript, ResultContext} = try
        % Enable caching lookup values, essential for fast data handling
        z_depcache:in_process(true),
        controller_postback:process_postback(Context1)
    catch
        Error:X ->
            ?zWarning(io_lib:format("~p:~p~n~p", [Error, X, erlang:get_stacktrace()]), Context1),
            {case z_context:get_q("z_trigger_id", Context1) of 
                undefined -> [];
                ZTrigger -> [" z_unmask_error('",z_utils:js_escape(ZTrigger),"');"]
             end, 
             Context1}
    end,
    % Cleanup process dict, so our process heap is smaller between calls
    z_session_page:add_script(ResultScript, ResultContext),
    z_utils:erase_process_dict(),
    ok.

websocket_info(_Msg, _Context) -> 
    ok.

websocket_terminate(_Reason, _Context) -> 
    ok.

