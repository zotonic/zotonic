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

-module(resource_websocket).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1, 
    upgrades_provided/2,
	websocket_start/1,
	loop/3,
	send_loop/2
    ]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init([]) -> {ok, []}.

upgrades_provided(ReqData, State) ->
    {[
        {"WebSocket", websocket_start}
    ], ReqData, State}.


%% @doc Initiate the websocket connection upgrade
websocket_start(ReqData) ->
    Context = z_context:new(ReqData),
    Context1 = z_context:ensure_all(Context),
    Socket = webmachine_request:socket(ReqData),
    Hostname = m_site:get(hostname, Context1),
    WebSocketPath = z_dispatcher:url_for(websocket, Context1),
    Data = ["HTTP/1.1 101 Web Socket Protocol Handshake", 13, 10,
            "Upgrade: WebSocket", 13, 10,
            "Connection: Upgrade", 13, 10,
            "WebSocket-Origin: http://", Hostname, 13, 10,
            "WebSocket-Location: ws://", Hostname, WebSocketPath, 13, 10, 
            13, 10],
    ok = send(Socket, Data),
    spawn_link(fun() -> start_send_loop(Socket, Context1) end),
    loop(none, Socket, Context1).


%% @doc Start receiving messages from the websocket
loop(Buff, Socket, Context) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Received} ->
            handle_data(Buff, Received, Socket, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Upack any data frames, send them to the handling functions.
handle_data(none, <<0,T/binary>>, Socket, Context) ->
    handle_data(<<>>, T, Socket, Context);
handle_data(none, <<>>, Socket, Context) ->
    resource_websocket:loop(none, Socket, Context);
handle_data(Msg, <<255,T/binary>>, Socket, Context) ->
    handle_message(Msg, Context),
    handle_data(none, T, Socket, Context);
handle_data(Msg, <<H,T/binary>>, Socket, Context) ->
    handle_data(<<Msg/binary, H>>, T, Socket, Context);
handle_data(Msg, <<>>, Socket, Context) ->
    resource_websocket:loop(Msg, Socket, Context).


%% Handle a message from the browser, should contain an url encoded request. Sends result script back to browser.
handle_message(Msg, Context) ->
    Qs = mochiweb_util:parse_qs(Msg),
    Context1 = z_context:set('q', Qs, Context),
    Postback = z_context:get_q("postback", Context1),
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context1),

    TriggerId1 = case TriggerId of
        undefined -> proplists:get_q("z_trigger_id", Context1);
        _         -> TriggerId
    end,

    ContextRsc = z_context:set_resource_module(Module, Context1),
    EventContext = case EventType of
        "submit" -> 
            case z_validation:validate_query_args(ContextRsc) of
                {ok, ContextEval} ->   
                    Module:event({submit, Tag, TriggerId1, TargetId}, ContextEval);
                {error, ContextEval} ->
                    ContextEval
            end;
        _ -> 
            Module:event({postback, Tag, TriggerId1, TargetId}, ContextRsc)
    end,
    Script = iolist_to_binary(z_script:get_script(EventContext)),
    z_session_page:add_script(Script, EventContext).
    

%% @doc Start the loop passing data (scripts) from the page to the browser
start_send_loop(Socket, Context) ->
    z_session_page:websocket_attach(self(), Context),
    send_loop(Socket, Context).

send_loop(Socket, Context) ->
    receive
        {send_data, Data} ->
            case send(Socket, [0, Data, 255]) of
                ok -> resource_websocket:send_loop(Socket, Context);
                closed -> closed
            end;
        _ ->
            resource_websocket:send_loop(Socket, Context)
    end.


%% @doc Send data to the user agent
send(undefined, _Data) ->
    ok;
send(Socket, Data) ->
    case gen_tcp:send(Socket, iolist_to_binary(Data)) of
        ok -> ok;
        {error, closed} -> closed;
        _ -> exit(normal)
    end.
