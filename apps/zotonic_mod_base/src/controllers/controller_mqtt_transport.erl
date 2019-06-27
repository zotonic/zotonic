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
    allowed_methods/1,
    content_types_accepted/1,
    upgrades_provided/1,
    process/4,
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

content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"json">>, []},
        {<<"application">>, <<"javascript">>, []},
        {<<"text">>, <<"javascript">>, []},
        {<<"text">>, <<"x-ubf">>, []},
        {<<"application">>, <<"x-bert">>, []},
        {<<"application">>, <<"x-www-form-urlencoded">>, []},
        {<<"multipart">>, <<"form-data">>, []}
    ], Context}.

allowed_methods(Context) ->
    {[<<"GET">>, <<"POST">>], Context}.

process(<<"POST">>, AcceptedCT, _ProvidedCT, Context) ->
    Topic = z_context:get_q(<<"*">>, Context),
    {Qs, Context1} = z_controller_helper:decode_request(AcceptedCT, Context),
    z_mqtt:publish(Topic, Qs, Context1),
    {true, Context1};
process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context) ->
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
websocket_handle({ping, Opaque}, Context) ->
    {reply, {pong, Opaque}, Context};
websocket_handle(Data, Context) ->
    lager:warning("MQTT websocket: non binary data received: ~p", [Data]),
    {ok, Context}.

% websocket_info({reply, {fetch_queue, Pid}}, Context) ->
%     {ok, Payload} = gen_server:call(Pid, fetch_queue),
%     {reply, {binary, Payload}, Context};
websocket_info({mqtt_transport, _SessionRef, Payload}, Context) when is_binary(Payload) ->
    {reply, {binary, Payload}, Context};
websocket_info({mqtt_transport, _SessionRef, disconnect}, Context) ->
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
    case z_context:get(session_ref, Context) of
        undefined ->
            handle_connect_data(Data, Context);
        SessionRef ->
            case mqtt_sessions:incoming_data(SessionRef, Data) of
                ok ->
                    {ok, Context};
                {error, _} ->
                    {stop, Context}
            end
    end.

handle_connect_data(Data, Context) ->
    WsData = z_context:get(wsdata, Context),
    NewData = << WsData/binary, Data/binary >>,
    MqttPool = z_context:site(Context),
    Options = #{
        connection_pid => self(),
        transport => self(),
        peer_ip => m_req:get(peer_ip, Context),
        context_prefs => #{
            user_id => z_acl:user(Context),
            language => z_context:language(Context),
            timezone => z_context:tz(Context),
            auth_options => z_context:get(auth_options, Context, #{})
        }
    },
    case mqtt_sessions:incoming_connect(MqttPool, NewData, Options) of
        {ok, {SessionRef, Rest}} ->
            erlang:monitor(process, SessionRef),
            Context1 = z_context:set(wsdata, undefined, Context),
            Context2 = z_context:set(session_ref, SessionRef, Context1),
            handle_incoming_data(Rest, Context2);
        {error, incomplete_packet} ->
            Context1 = z_context:set(wsdata, NewData, Context),
            {ok, Context1};
        {error, expect_connect} ->
            lager:info("MQTT: refusing connect with wrong packet type"),
            {stop, Context};
        {error, _} ->
            % Invalid packet or unkown host - just close the connection
            {stop, Context}
    end.
