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
    forbidden/2,
    upgrades_provided/2,
    charsets_provided/2,
    content_types_provided/2,
    provide_content/2,
    websocket_start/2,
    loop/4,
    send_loop/2,
    
    pack_length/1,
    unpack_length/1
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init([]) -> {ok, []}.


%% @doc The request must have a valid session cookie.
forbidden(ReqData, _State) ->
    Context = z_context:new(ReqData),
    Context1 = z_context:continue_session(Context),
    ?WM_REPLY(not z_context:has_session(Context1), Context1).

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
    Hostname = m_site:get(hostname, Context1),
    WebSocketPath = z_dispatcher:url_for(websocket, [{z_pageid, z_context:get_q("z_pageid", Context1)}],Context1),
    Socket = webmachine_request:socket(ReqData),
    Protocol = case wrq:is_ssl(ReqData) of true -> "https"; _ -> "http" end,
    
    case z_context:get_req_header("sec-websocket-key1", Context1) of
        undefined ->
            % First draft protocol version, this code should be removed in due time.
            %% Send the handshake
            Data = ["HTTP/1.1 101 Web Socket Protocol Handshake", 13, 10,
                    "Upgrade: WebSocket", 13, 10,
                    "Connection: Upgrade", 13, 10,
                    "WebSocket-Origin: ",Protocol,"://", Hostname, 13, 10,
                    "WebSocket-Location: ws://", Hostname, WebSocketPath, 13, 10,
                    13, 10
                    ],
            ok = send(Socket, Data),
            spawn_link(fun() -> start_send_loop(Socket, Context1) end),
            loop(none, nolength, Socket, Context1);
            
        WsKey1 ->
            % Protocol draft 76

            %% Sec-Websocket stuff
            Key1 = process_key(WsKey1),
            Key2 = process_key(z_context:get_req_header("sec-websocket-key2", Context1)),
            {ok, Body} = gen_tcp:recv(Socket, 8),
            SignKey = crypto:md5(<<Key1:32/integer, Key2:32/integer, Body/binary>>),

            %% Send the handshake
            Data = ["HTTP/1.1 101 Web Socket Protocol Handshake", 13, 10,
                    "Upgrade: WebSocket", 13, 10,
                    "Connection: Upgrade", 13, 10,
                    "Sec-WebSocket-Origin: ",Protocol,"://", Hostname, 13, 10,
                    "Sec-WebSocket-Location: ws://", Hostname, WebSocketPath, 13, 10,
                    "Sec-WebSocket-Protocol: zotonic", 13, 10,
                    13, 10,
                    <<SignKey/binary>>
                    ],
            ok = send(Socket, Data),
            spawn_link(fun() -> start_send_loop(Socket, Context1) end),
            loop(none, nolength, Socket, Context1)
    end.

%% @doc Start receiving messages from the websocket
loop(Buff, Length, Socket, Context) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Received} ->
            handle_data(Buff, Length, Received, Socket, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Upack any data frames, send them to the handling functions.
handle_data(none, nolength, <<0,T/binary>>, Socket, Context) ->
    <<Type, _/binary>> = T,
    case Type =< 127 of
        true -> 
            handle_data(<<>>, nolength, T, Socket, Context);
        false ->
            {Length, LenBytes} = unpack_length(T),
            <<_:LenBytes/bytes, Rest:Length/bytes>> = T,
            handle_data(<<>>, Length, Rest, Socket, Context)
    end;

%% Extract frame ending with 255
handle_data(none, nolength, <<>>, Socket, Context) ->
    resource_websocket:loop(none, nolength, Socket, Context);
handle_data(<<>>, nolength, <<255,_T/binary>>, _Socket, _Context) ->
    % A packet of <<0,255>> signifies that the ua wants to close the connection
    ua_close_request;
handle_data(Msg, nolength, <<255,T/binary>>, Socket, Context) ->
    handle_message(Msg, Context),
    handle_data(none, nolength, T, Socket, Context);
handle_data(Msg, nolength, <<H,T/binary>>, Socket, Context) ->
    handle_data(<<Msg/binary, H>>, nolength, T, Socket, Context);

%% Extract frame with length bytes
handle_data(Msg, 0, T, Socket, Context) ->
    handle_message(Msg, Context),
    handle_data(none, nolength, T, Socket, Context);
handle_data(Msg, Length, <<H,T/binary>>, Socket, Context) when is_integer(Length) and Length > 0 ->
    handle_data(<<Msg/binary, H>>, Length-1, T, Socket, Context);

%% Data ended before the end of the frame, loop to fetch more
handle_data(Msg, Length, <<>>, Socket, Context) ->
    resource_websocket:loop(Msg, Length, Socket, Context).


%% @doc Unpack the length bytes
%% @author Davide Marquês (From yaws_websockets.erl)
unpack_length(Binary) ->
    unpack_length(Binary, 0, 0).
unpack_length(Binary, LenBytes, Length) ->
    <<_:LenBytes/bytes, B, _/bitstring>> = Binary,
    B_v = B band 16#7F,
    NewLength = (Length * 128) + B_v,
    case B band 16#80 of
    16#80 ->
        unpack_length(Binary, LenBytes + 1, NewLength);
    0 ->
        {NewLength, LenBytes + 1}
    end.

%% @doc Pack the length in 7 bits bytes
pack_length(N) ->
    pack_length(N, []).

pack_length(N, Acc) ->
    N1 = N div 128,
    B = N rem 128,
    case Acc of
        [] ->
            pack_length(N1, [B|Acc]);
        _ ->
            case N1 of
                0 -> [B+128|Acc];
                _ -> pack_length(N1, [B+128|Acc])
            end
    end.


%% Handle a message from the browser, should contain an url encoded request. Sends result script back to browser.
handle_message(Msg, Context) ->
    Qs = mochiweb_util:parse_qs(Msg),
    Context1 = z_context:set('q', Qs, Context),
    Postback = z_context:get_q("postback", Context1),
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context1),

    TriggerId1 = case TriggerId of
        undefined -> z_context:get_q("z_trigger_id", Context1);
        _         -> TriggerId
    end,

    {ResultScript, ResultContext} = try
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
        % Remove the busy mask from the element that triggered this event.
        {case TriggerId1 of 
            undefined -> Script;
            _ -> [Script, " z_unmask('",z_utils:js_escape(TriggerId1),"');" ]
         end, 
         EventContext}
    catch
        Error:X ->
            ?zWarning(io_lib:format("~p:~p~n~p", [Error, X, erlang:get_stacktrace()]), Context1),
            {case TriggerId1 of 
                undefined -> [];
                _ -> [" z_unmask_error('",z_utils:js_escape(TriggerId1),"');"]
             end, 
             Context1}
    end,
    z_session_page:add_script(ResultScript, ResultContext),
    erlang:erase().
    

%% @doc Start the loop passing data (scripts) from the page to the browser
start_send_loop(Socket, Context) ->
    % We want to receive any exit signal (including 'normal') from the socket's process.
    process_flag(trap_exit, true),
    z_session_page:websocket_attach(self(), Context),
    send_loop(Socket, Context).

send_loop(Socket, Context) ->
    receive
        {send_data, Data} ->
            case send(Socket, [0, Data, 255]) of
                ok -> resource_websocket:send_loop(Socket, Context);
                closed -> closed
            end;
        {'EXIT', _FromPid, _Reason} ->
            % Exit of the socket's process, stop sending data.
            exit;
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


%% Process a key from the websockey handshake, return an integere
%% @spec process_key(string()) -> integer
process_key(L) ->
    {Number,Spaces} = split_key(L, [], 0),
    Number div Spaces.

    split_key([], DAcc, Spaces) ->
        {list_to_integer(lists:reverse(DAcc)), Spaces};
    split_key([C|Rest], DAcc, Spaces) when C >= $0, C =< $9 ->
        split_key(Rest, [C|DAcc], Spaces);
    split_key([32|Rest], DAcc, Spaces) ->
        split_key(Rest, DAcc, Spaces+1);
    split_key([_|Rest], DAcc, Spaces) ->
        split_key(Rest, DAcc, Spaces).



