%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%%
%% @doc Zotonic WebSocket Request Adapter.
%%
%% Copyright 2014 Maas-Maarten Zeeman
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

-module(z_ws_request_adapter).


-export([
        init/2,
	get/2, 
	maybe_reply/2,
	ensure_response/2,
	upgrade_reply/3,
	parse_header/2,
	header/2,
        port/1,
	set_meta/3]).

-export([
	websocket_handler_init/3,
	websocket_handler_callback/5,

	websocket_handler_handle_event/5
	]).

%% Helper function.
-export([messages/1]).
-export_type([req/0]).

-include_lib("zotonic.hrl").

-define(IS_TOKEN_SEP(C), (C =:= $, orelse C =:= $\s orelse C=:= $\t)).

%% Request adapter record.
-record(req_adapter, {
	req, %% wm_reqdata,

	%% If set to true we check if we can compress.
	resp_compress = false :: boolean(),

	%% Headers to add to the response.
	resp_headers = [],

	%%
	sent_upgrade_reply = false :: boolean(),

	%% Possible meta values.
	websocket_version=undefined :: undefined | integer(),
	websocket_compress=false :: boolean()
	}).

-type req() :: #req_adapter{}.


%%
%%
%%

% @doc Initialize the request helper
%
-spec init(ReqData :: any(), RespCompress :: boolean()) -> req().
init(ReqData, RespCompress) ->
    #req_adapter{req=ReqData, resp_compress=RespCompress}.


% @doc Mimics cowboy_req:get/2
%
-spec get(atom() | list(), req()) -> any() | list().
get(socket, ReqAdapter) ->
    Req = ReqAdapter#req_adapter.req, 
    Req#wm_reqdata.socket;
get(resp_compress, ReqAdapter) ->
    ReqAdapter#req_adapter.resp_compress;
get(websocket_version, ReqAdapter) ->
    ReqAdapter#req_adapter.websocket_version;
get(websocket_compress, ReqAdapter) ->
    ReqAdapter#req_adapter.websocket_compress;
get(L, Req) when is_list(L) ->
    get(L, Req, []).
get([], _Req, Acc) ->
    lists:reverse(Acc);
get([H|T], Req, Acc) ->
    get(T, Req, [get(H, Req)|Acc]).


% @doc Mimics cowboy_req:maybe_reply/2
%
-spec maybe_reply(400, req()) -> ok.
maybe_reply(400, ReqAdapter) ->
    case ReqAdapter#req_adapter.sent_upgrade_reply of
        true ->
            ok;
        false ->	
            reply(400, ReqAdapter)
    end.


% @doc Mimics cowboy_req:ensure_response/2
%
-spec ensure_response(req(), 400) -> ok.
ensure_response(ReqAdapter, 400) ->
    reply(400, ReqAdapter).


%% 
%% Send an upgrade reply to the 
%-spec upgrade_reply(101, elli:headers(), req()) -> {ok, req()}.
upgrade_reply(101, Headers, #req_adapter{req=Req}=RA) ->
    UpgradeHeaders = [{<<"Connection">>, <<"Upgrade">>} | Headers],
    ok = send_response(Req, 101, RA#req_adapter.resp_headers ++ UpgradeHeaders, <<>>),
    {ok, RA#req_adapter{sent_upgrade_reply=true}}.



%% Note: The headers keys are already parsed by Erlang decode_packet. This
%% means that all keys are capitalized.

% @doc Mimics cowboy_req:parse_header/3 {ok, ParsedHeaders, Req}
%
parse_header(<<"upgrade">>, #req_adapter{req=Req}=RA) ->
    %% case insensitive tokens.
    Values = get_header_values(<<"Upgrade">>, Req),
    {ok, tokens(Values), RA};
parse_header(<<"connection">>, #req_adapter{req=Req}=RA) ->
    Values = get_header_values(<<"Connection">>, Req),
    {ok, tokens(Values), RA};
parse_header(<<"sec-websocket-extensions">>, #req_adapter{req=Req}=RA) ->
    Values = get_header_values(<<"Sec-WebSocket-Extensions">>, Req),
    %% We only recognize x-webkit-deflate-frame, which has no args,
    %% skip the rest.
    Exts = tokens(Values),
    Extensions = [{E, []} || E <- Exts, E =:= <<"x-webkit-deflate-frame">>],
    {ok, Extensions, RA}.

% @doc Mimics cowboy_req:header/2
%
header(<<"sec-websocket-version">>, #req_adapter{req=Req}=RA) ->
    {get_header_value(<<"Sec-WebSocket-Version">>, Req), RA};
header(<<"sec-websocket-key">>, #req_adapter{req=Req}=RA) ->
    {get_header_value(<<"Sec-Websocket-Key">>, Req), RA}.

% @doc Returned as message for active once.
%
port({ssl, SSLSocket}) ->
    SSLSocket;
port(Socket) ->
    Socket.

% @doc Mimics cowboy_req:set_meta/3
% 
set_meta(websocket_version, Version, ReqAdapter) ->
    ReqAdapter#req_adapter{websocket_version = Version};
set_meta(websocket_compress, Bool, ReqAdapter) ->
    ReqAdapter#req_adapter{websocket_compress = Bool}.


% @doc Call the websocket_init callback of the websocket handler.
%
% calls websocket_init(Req, HandlerOpts) ->
%     {ok, Headers, HandlerState} - We can upgrade, headers are added to the upgrade response.
%     {ok, Headers, hibernate, HandlerState} - We can upgrade, but this process will hibernate, headers 
%         are added to the upgrade response
%     {ok, Headers, Timeout, HandlerState} - We can upgrade, we will timout, headers are added to the 
%         upgrade respose.
%     {ok, Headers, hibernate, Timeout, HandlerState} - We can upgrade, set a timeout and hibernate. 
%         Headers are added to the response.
%     {shutdown, Headers} - We can't upgrade, a bad request response will be sent to the client.
%
-spec websocket_handler_init(req(), Handler :: module(), HandlerState :: any()) ->
    {shutdown, req()} |
    {ok, req(), any()} |
    {ok, req(), any(), hibernate} |
    {ok, req(), any(), Timeout :: non_neg_integer()} |
    {ok, req(), any(), Timeout :: non_neg_integer(), hibernate}.
websocket_handler_init(#req_adapter{req=Req}=RA, Handler, HandlerOpts) ->
    case Handler:websocket_init(Req, HandlerOpts) of
        {shutdown, Headers} ->
            {shutdown, RA#req_adapter{resp_headers=Headers}};
        {ok, Headers, HandlerState} ->
            {ok, RA#req_adapter{resp_headers=Headers}, HandlerState};
        {ok, Headers, hibernate, HandlerState} ->
            {ok, RA#req_adapter{resp_headers=Headers}, HandlerState, hibernate};
        {ok, Headers, Timeout, HandlerState} ->
            {ok, RA#req_adapter{resp_headers=Headers}, HandlerState, Timeout};
        {ok, Headers, hibernate, Timeout, HandlerState} ->
            {ok, RA#req_adapter{resp_headers=Headers}, HandlerState, Timeout, hibernate}
    end.

% @doc Calls websocket_info en websocket_handle callbacks.
-spec websocket_handler_callback(req(), Handler :: module(), websocket_info | websocket_handle, Message :: any(), HandlerState :: any()) ->
    {ok, req(), any()} |
    {ok, req(), any(), hibernate} |
    {reply, binary() | iolist(), req(), any()} | 
    {reply, binary() | iolist(), hibernate, req(), any()} | 
    {shutdown, req(), any()}. 
websocket_handler_callback(#req_adapter{req=Req}=RA, Handler, Callback, Message, HandlerState) ->
    case Handler:Callback(Req, Message, HandlerState) of 
        {ok, HandlerState1} ->
            {ok, RA, HandlerState1};
        {ok, hibernate, HandlerState1} ->
            {ok, RA, HandlerState1, hibernate};
        {reply, Payload, HandlerState1} ->
            {reply, Payload, RA, HandlerState1};
        {reply, Payload, hibernate, HandlerState1} ->
            {reply, Payload, RA, HandlerState1, hibernate};
        {shutdown, HandlerState1} ->
            {shutdown, RA, HandlerState1}
    end.

% @doc Report an event...
-spec websocket_handler_handle_event(req(), Handler :: module(), atom(), list(), any()) -> ok.
websocket_handler_handle_event(#req_adapter{req=Req}, Handler, Name, EventArgs, HandlerOpts) ->
    try
        Handler:websocket_handle_event(Name, [Req|EventArgs], HandlerOpts)
    catch
        EvClass:EvError ->
            error_logger:error_msg("~p:handle_event/3 crashed ~p:~p~n~p",
                                   [Handler, EvClass, EvError,
                                    erlang:get_stacktrace()])
    end.

% @doc Atoms used to identify messages in {active, once | true} mode.
-spec messages(RA :: req()) -> 
    {tcp, tcp_closed, tcp_error} | {ssl, ssl_closed, ssl_error}.
messages(#req_adapter{req=Req}) ->
    case Req#wm_reqdata.socket of
        undefined -> 
            undefined;
        Socket ->
            socket_messages(Socket)
    end.

socket_messages({ssl, _}) ->
    {ssl, ssl_closed, ssl_error};
socket_messages(_Socket) ->
    {tcp, tcp_closed, tcp_error}.

%%
%% Helpers
%%

% @doc Send a bad_request reply.
%
-spec reply(400, #req_adapter{}) -> ok.
reply(400, #req_adapter{req=ReqData}) ->
    Body = <<"Bad request">>,
    Size = size(Body),
    ok = send_response(ReqData, 400, [{"Connection", "close"},
                                      {"Content-Length", Size}], Body).

send_response(ReqData, Code, Headers, UserBody) ->
    Body = case {ReqData#wm_reqdata.method, Code} of
        {'HEAD', _} -> <<>>;
        {_, 304}    -> <<>>;
        {_, 204}    -> <<>>;
        _           -> UserBody
    end,

    Response = [<<"HTTP/1.1 ">>, status(Code), <<"\r\n">>,
                encode_headers(Headers), <<"\r\n">>,
                Body],

    mochiweb_socket:send(ReqData#wm_reqdata.socket, Response),
    ok.

status(Code) when is_integer(Code) ->
    iolist_to_binary([integer_to_list(Code), 32, httpd_util:reason_phrase(Code)]).

encode_headers([]) ->
    [];
encode_headers([[] | H]) ->
    encode_headers(H);
encode_headers([{K, V} | H]) ->
    [encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].


encode_value(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_value(V) when is_binary(V) -> V;
encode_value(V) when is_list(V) -> list_to_binary(V).
            

% @doc Get all header values for Key
get_header_values(Key, #wm_reqdata{}=ReqData) ->
    Hdrs = wrq:req_headers(ReqData),
    LcKey = string:to_lower(binary:bin_to_list(Key)),
    [binary:list_to_bin(V) || V <- proplists:get_all_values(LcKey, Hdrs)].

% @doc Get the first value.
get_header_value(Key, #wm_reqdata{}=ReqData) ->
    case wrq:get_req_header(binary:bin_to_list(Key), ReqData) of
        undefined -> undefined;
        L -> binary:list_to_bin(L)
    end. 

%%
%% Helpers
%% 

%% @doc Parse tokens 
tokens(L) when is_list(L) ->
    lists:flatten([tokens(V) || V <- L]);

tokens(Header) when is_binary(Header) ->
    parse_before(Header, []).

parse_before(<<>>, Acc) ->
    lists:reverse(Acc);
parse_before(<< C, Rest/bits >>, Acc) when ?IS_TOKEN_SEP(C) ->
    parse_before(Rest, Acc);
parse_before(Buffer, Acc) ->
    parse(Buffer, Acc, <<>>).

parse(<<>>, Acc, <<>>) ->
    lists:reverse(Acc);
parse(<<>>, Acc, Token) ->
    lists:reverse([Token|Acc]);
parse(<<C, Rest/bits>>, Acc, Token) when ?IS_TOKEN_SEP(C) ->
    parse_before(Rest, [Token|Acc]);
parse(<<C, Rest/bits>>, Acc, Token) ->
    parse(Rest, Acc, <<Token/binary, (lchr(C))>>).


% @doc convert ascii character to lowercase.
lchr($A) -> $a;
lchr($B) -> $b;
lchr($C) -> $c;
lchr($D) -> $d;
lchr($E) -> $e;
lchr($F) -> $f;
lchr($G) -> $g;
lchr($H) -> $h;
lchr($I) -> $i;
lchr($J) -> $j;
lchr($K) -> $k;
lchr($L) -> $l;
lchr($M) -> $m;
lchr($N) -> $n;
lchr($O) -> $o;
lchr($P) -> $p;
lchr($Q) -> $q;
lchr($R) -> $r;
lchr($S) -> $s;
lchr($T) -> $t;
lchr($U) -> $u;
lchr($V) -> $v;
lchr($W) -> $w;
lchr($X) -> $x;
lchr($Y) -> $y;
lchr($Z) -> $z;
lchr(Chr) -> Chr.

