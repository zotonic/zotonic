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

-include_lib("zotonic.hrl").

%% ---------------------------------------------------------------------------------------------
%% Cowmachine controller callbacks
%% ---------------------------------------------------------------------------------------------

upgrades_provided(Context) ->
    {[{<<"WebSocket">>, websocket_start}], Context}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, provide_content}], Context}.

provide_content(Context) ->
    Context2 = z_context:set_resp_header(<<"x-robots-tag">>, <<"noindex">>, Context),
    Rendered = z_template:render("error_websocket.tpl", z_context:get_all(Context2), Context2),
    z_context:output(Rendered, Context2).


%% ---------------------------------------------------------------------------------------------
%% Upgrade callback, called when starting the websocket connection
%% ---------------------------------------------------------------------------------------------

%% @doc Initiate the websocket connection upgrade
websocket_start(Context) ->
    Context1 = z_context:continue_session(Context),
    Context2 = z_context:set(ws_request, true, Context1),
    PrunedContext = z_context:prune_for_scomp(Context2),
    cowmachine_websocket_upgrade:upgrade(?MODULE, PrunedContext).

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
%%      Called by the page session, forwarding queued messages
websocket_send_data(_WsControllerPid, <<>>) ->
    ok;
websocket_send_data(WsControllerPid, Data) ->
    WsControllerPid ! {reply, {text, Data}}.


%% ---------------------------------------------------------------------------------------------
%% Websocket handler callbacks
%% ---------------------------------------------------------------------------------------------

websocket_init(Context) ->
    {ok, Context}.

%% @doc Handle a message from the browser, should contain an ubf encoded request.
websocket_handle({Type, Data}, Context) when Type =:= text; Type =:= binary ->
    try
        handle_websocket_data(Data, [], Context)
    catch
        Error:X ->
            StackTrace = erlang:get_stacktrace(),
            lager:warning("[~p] ~p:~p~n~p",
                          [z_context:site(Context), Error, X, StackTrace]),
            {ok, Context}
    end;
websocket_handle(_Data, Context) ->
    {ok, Context}.

websocket_info(_Msg, _Context) ->
    ok.


%% ---------------------------------------------------------------------------------------------
%% Internal
%% ---------------------------------------------------------------------------------------------

handle_websocket_data(<<>>, [], Context) ->
    {ok, Context};
handle_websocket_data(<<>>, Acc, Context) ->
    Acc1 = lists:reverse([ Reply || Reply <- Acc, Reply =/= <<>> ]),
    case Acc1 of
        [] -> {ok, Context};
        _ -> {reply, {text, Acc1}, Context}
    end;
handle_websocket_data(Data, Acc, Context) ->
    {ok, Term, RestData} = z_transport:data_decode(Data),
    {ok, Reply, ContextWs} = z_transport:incoming(Term, Context),
    {ok, ReplyData} = z_transport:data_encode(Reply),
    z_session_page:websocket_attach(self(), ContextWs),
    handle_websocket_data(RestData, [ReplyData|Acc], ContextWs).
