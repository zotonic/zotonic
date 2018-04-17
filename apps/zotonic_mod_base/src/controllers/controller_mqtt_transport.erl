%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2018 Marc Worrell
%% @doc MQTT WebSocket connections

%% Copyright 2018 Marc Worrell
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

-module(controller_mqtt_transport).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    upgrades_provided/1,
    charsets_provided/1,
    content_types_provided/1,
    provide_content/1,
    websocket_start/1,
    websocket_send_data/2,
    is_websocket_request/1
]).

% websocket handler exports.
-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").


%% ---------------------------------------------------------------------------------------------
%% Cowmachine controller callbacks
%% ---------------------------------------------------------------------------------------------

upgrades_provided(Context) ->
    {[{<<"WebSocket">>, websocket_start}], Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, provide_content}], Context}.

% TODO: handle SSE
provide_content(Context) ->
    Context2 = z_context:set_resp_header(<<"x-robots-tag">>, <<"noindex">>, Context),
    Rendered = z_template:render("error_websocket.tpl", z_context:get_all(Context2), Context2),
    z_context:output(Rendered, Context2).


%% ---------------------------------------------------------------------------------------------
%% Upgrade callback, called when starting the websocket connection
%% ---------------------------------------------------------------------------------------------

%% @doc Initiate the websocket connection upgrade
websocket_start(Context) ->
    Context2 = z_context:set(ws_request, true, Context),
    Handler = z_context:get(ws_handler, Context, ?MODULE),
    Protocol = case cowmachine_req:get_req_header(<<"sec-websocket-protocol">>, Context2) of
        undefined -> <<"mqtt">>;
        ReqProtocol ->
            Ps = binary:split(ReqProtocol, <<",">>, [global]),
            Ps1 = [ z_string:trim(P) || P <- Ps ],
            case lists:member(<<"mqtt.cotonic.org">>, Ps1) of
                true -> <<"mqtt.cotonic.org">>;
                false -> <<"mqtt">>
            end
    end,
    Context3 = cowmachine_req:set_resp_header(<<"sec-websocket-protocol">>, Protocol, Context2),
    cowmachine_websocket_upgrade:upgrade(Handler, Context3).

%% ---------------------------------------------------------------------------------------------
%% External entry points
%% ---------------------------------------------------------------------------------------------

%% @doc Returns true if this process is a websocket request handler
is_websocket_request(Context) ->
    case z_context:get(ws_request, Context, false) of
        true -> true;
        _ -> false
    end.

%% @doc Send Data over websocket Pid to the client.
%%      Called by the MQTT session, forwarding queued messages
websocket_send_data(_WsControllerPid, <<>>) ->
    ok;
websocket_send_data(WsControllerPid, Data) ->
    WsControllerPid ! {reply, Data}.


%% ---------------------------------------------------------------------------------------------
%% Websocket handler callbacks
%% ---------------------------------------------------------------------------------------------

websocket_init(Context) ->
    PrunedContext = z_context:prune_for_scomp(Context),
    PrunedContext1 = z_context:set(wsdata, <<>>, PrunedContext),
    {ok, PrunedContext1}.

%% @doc Handle a MQTT message from the browser
websocket_handle({binary, <<255, 254, 42, _, _>> = Ping}, Context) ->
    % This is an optional ping/pong when starting the handshake
    case z_context:get(session_ref, Context) of
        undefined ->
            {reply, {binary, Ping}, Context};
        _SessionRef ->
            handle_incoming_data(Ping, Context)
    end;
websocket_handle({binary, Data}, Context) ->
    handle_incoming_data(Data, Context);
websocket_handle(Data, Context) ->
    lager:warning("MQTT websocket: non binary data received: ~p", [Data]),
    {ok, Context}.

websocket_info({reply, {fetch_queue, Pid}}, Context) ->
    {ok, Payload} = gen_server:call(Pid, fetch_queue),
    {reply, {binary, Payload}, Context};
websocket_info({reply, Payload}, Context) when is_binary(Payload) ->
    {reply, {binary, Payload}, Context};
websocket_info({reply, Msg}, Context) when is_map(Msg) ->
    {ok, Payload} = mqtt_packet_map:encode(Msg),
    {reply, {binary, Payload}, Context};
websocket_info({reply, disconnect}, Context) ->
    {stop, Context};
websocket_info(close, Context) ->
    {stop, Context};
websocket_info({'DOWN', _MRef, process, Pid, _Reason}, Context) ->
    case z_context:get(session_ref, Context) of
        Pid -> {stop, Context};
        _Other -> {ok, Context}
    end;
websocket_info(Msg, Context) ->
    lager:info("~p: Unknown message ~p", [?MODULE, Msg]),
    {ok, Context}.


%% ---------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------

handle_incoming_data(Data, Context) ->
    WsData = z_context:get(wsdata, Context),
    case decode_incoming_data(<<WsData/binary, Data/binary>>, Context) of
        {ok, RestData, Context1} ->
            Context2 = z_context:set(wsdata, RestData, Context1),
            {ok, Context2};
        {error, Reason} ->
            lager:warning("MQTT websocket: closing due to: ~p", [Reason]),
            {stop, Context}
    end.

decode_incoming_data(<<>>, Context) ->
    {ok, <<>>, Context};
decode_incoming_data(Data, Context) ->
    case mqtt_packet_map:decode(Data) of
        {ok, {Msg, RestData}} ->
            case handle_message(Msg, Context) of
                {ok, Context1} ->
                    decode_incoming_data(RestData, Context1);
                {error, _} = Error ->
                    Error
            end;
        {error, incomplete_packet} ->
            {ok, Data, Context};
        {error, _} = Error ->
            Error
    end.

%% Send the message to the attached MQTT session
handle_message(Msg, Context) ->
    OptSessionRef = z_context:get(session_ref, Context),
    MsgOptions = [
        {transport, self()}
    ],
    case mqtt_sessions:incoming_message(mqtt_session_pool(Context), OptSessionRef, Msg, MsgOptions) of
        {ok, undefined} ->
            {ok, Context};
        {ok, OptSessionRef} ->
            {ok, Context};
        {ok, NewSessionRef} ->
            erlang:monitor(process, NewSessionRef),
            {ok, z_context:set(session_ref, NewSessionRef, Context)};
        {error, _} = Error ->
            Error
    end.

mqtt_session_pool(Context) ->
    z_context:site(Context).

