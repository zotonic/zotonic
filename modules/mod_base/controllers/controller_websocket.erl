%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2014 Marc Worrell
%% @doc WebSocket connections

%% Copyright 2010-2014 Marc Worrell
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
    service_available/2,
    upgrades_provided/2,
    charsets_provided/2,
    content_types_provided/2,
    provide_content/2,
    websocket_start/2,    
    websocket_send_data/2,
    is_websocket_request/1
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

init(DispatchArgs) ->
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:continue_session(Context1),
    z_context:lager_md(Context2),
    ?WM_REPLY(true, Context2).

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
    Context2 = z_context:set_resp_header("X-Robots-Tag", "noindex", Context1),
    Rendered = z_template:render("error_websocket.tpl", z_context:get_all(Context2), Context2),
    {Output, OutputContext} = z_context:output(Rendered, Context2),
    ?WM_REPLY(Output, OutputContext).

%% @doc Initiate the websocket connection upgrade
websocket_start(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = case z_context:get(ws_handler, Context1) of
        undefined -> z_context:set(ws_handler, ?MODULE, Context1);
        _Hdlr -> Context1
    end,
    Context3 = z_context:set(ws_request, true, Context2),
    PrunedContext = prune_context(Context3),
    case wrq:get_req_header_lc("sec-websocket-version", ReqData) of
        undefined ->
            case wrq:get_req_header_lc("sec-websocket-key1", ReqData) of
                undefined ->
                    z_websocket_hixie75:start(ReqData, PrunedContext);
                WsKey1 ->
                    z_websocket_hybi00:start(WsKey1, ReqData, PrunedContext)
            end;
        "7" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-07
            z_websocket_hybi17:start(ReqData, PrunedContext);
        "8" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-10
            z_websocket_hybi17:start(ReqData, PrunedContext);
        "13" ->
            % http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-17
            z_websocket_hybi17:start(ReqData, PrunedContext)
    end.


%% @doc Prune a context and reqdata for the long-lived websocket processes
prune_context(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    ContextPruned = z_context:prune_for_scomp(ContextQs),
    ReqData = z_context:get_reqdata(ContextQs),
    ReqData1 = #wm_reqdata{
        peer=ReqData#wm_reqdata.peer,
        port=ReqData#wm_reqdata.port
    },
    z_context:set_reqdata(ReqData1, ContextPruned). 


%% @doc Returns true if this a websocket request
is_websocket_request(Context) ->
    case z_context:get(ws_request, Context, false) of
        true -> true;
        _ -> false
    end.

%% @doc Send Data over websocket Pid to the client.
websocket_send_data(Pid, Data) ->
    Pid ! {send_data, Data}.

%% Called during initialization of the websocket.
websocket_init(_Context) ->
    ok.

%% Handle a message from the browser, should contain an ubf encoded request. Sends result script back to browser.
websocket_message(<<>>, _SenderPid, Context) ->
    {ok, Context};
websocket_message(Data, SenderPid, Context) ->
    try
        {ok, Term, RestData} = z_transport:data_decode(Data),
        {ok, Reply, ContextWs} = z_transport:incoming(Term, Context),
        {ok, ReplyData} = z_transport:data_encode(Reply),
        z_session_page:websocket_attach(SenderPid, ContextWs),
        websocket_send_data(SenderPid, ReplyData),
        websocket_message(RestData, SenderPid, ContextWs)
    catch
        Error:X ->
            ?zWarning(io_lib:format("~p:~p~n~p", [Error, X, erlang:get_stacktrace()]), Context),
            ok
    end.

websocket_info(_Msg, _Context) -> 
    ok.

websocket_terminate(_Reason, _Context) -> 
    ok.

