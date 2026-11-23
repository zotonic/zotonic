%% @copyright 2026 Marc Worrell
%% @doc Apply the MIME policy before cowboy_compress_h sees response headers.
%% See ../README.md for the required stream handler ordering and protocol.
%% @end

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

-module(zotonic_compress_policy_h).
-behavior(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

%% Reserved internal response header; never forwarded to the client.
-define(MARKER, <<"x-zotonic-compress-skip">>).

-spec init(StreamID, Req, Opts) -> {cowboy_stream:commands(), term()} when
    StreamID :: cowboy_stream:streamid(), Req :: cowboy_req:req(), Opts :: cowboy:opts().
init(StreamID, Req, Opts) ->
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {commands(Commands), Next}.

-spec data(StreamID, IsFin, Data, State) -> {cowboy_stream:commands(), term()} when
    StreamID :: cowboy_stream:streamid(), IsFin :: cowboy_stream:fin(),
    Data :: binary(), State :: term().
data(StreamID, IsFin, Data, State) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, State),
    {commands(Commands), Next}.

-spec info(StreamID, Info, State) -> {cowboy_stream:commands(), term()} when
    StreamID :: cowboy_stream:streamid(), Info :: term(), State :: term().
info(StreamID, Info, State) ->
    {Commands, Next} = cowboy_stream:info(StreamID, Info, State),
    {commands(Commands), Next}.

-spec terminate(StreamID, Reason, State) -> ok when
    StreamID :: cowboy_stream:streamid(), Reason :: cowboy_stream:reason(), State :: term().
terminate(StreamID, Reason, State) ->
    cowboy_stream:terminate(StreamID, Reason, State).

%% Cowboy does not compress early errors; leave this path unchanged.
-spec early_error(StreamID, Reason, Req, Resp, Opts) -> cowboy_stream:resp_command() when
    StreamID :: cowboy_stream:streamid(), Reason :: cowboy_stream:reason(),
    Req :: cowboy_stream:partial_req(), Resp :: cowboy_stream:resp_command(), Opts :: cowboy:opts().
early_error(StreamID, Reason, Req, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, Req, Resp, Opts).

commands(Commands) ->
    lists:map(fun command/1, Commands).

command({response, Status, Headers, Body}) ->
    {response, Status, headers(Headers), Body};
command({headers, Status, Headers}) ->
    {headers, Status, headers(Headers)};
command(Command) ->
    Command.

headers(Headers0) ->
    % A controller cannot supply the private marker, including for a real encoding.
    Headers = maps:remove(?MARKER, Headers0),
    case maps:is_key(<<"content-encoding">>, Headers)
        orelse z_media_identify:is_mime_compressible(maps:get(<<"content-type">>, Headers, undefined))
    of
        true -> Headers;
        false -> Headers#{<<"content-encoding">> => <<"identity">>, ?MARKER => <<"1">>}
    end.
