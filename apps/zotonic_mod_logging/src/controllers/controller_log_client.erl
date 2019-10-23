%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell <marc@worrell.nl>
%% @doc Log client side events

%% Copyright 2019 Marc Worrell
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

-module(controller_log_client).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    allowed_methods/1,
    content_types_accepted/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

% Default max body length (50KB) for HTTP log requests.
-define(MAX_BODY_LENGTH, 50*1024).

service_available(Context) ->
    Context1 = z_context:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Context),
    {true, Context1}.

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

content_types_accepted(Context) ->
    {[
        {<<"application">>, <<"json">>, []}
    ], Context}.

-spec process( binary(), cowmachine_req:media_type() | undefined, cowmachine_req:media_type(), z:context() )
        -> {iodata(), z:context()} | {{halt, HttpCode :: pos_integer()}, z:context()}.
process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    {Body, Context1} = req_body(Context),
    log(Body, Context1),
    {<<>>, Context1}.

log(<<>>, _Context) ->
    ok;
log(Body, Context) ->
    Body1 = filter_text(Body),
    case json_decode(Body1) of
        {ok, LogEvent} when is_map(LogEvent) ->
            case mod_logging:is_ui_ratelimit_check(Context) of
                true ->
                    m_log_ui:insert_event(LogEvent, Context),
                    lager:info("UI event: ~s", [Body1]);
                false ->
                    ok
            end;
        {error, _} ->
            ok
    end.

filter_text(Body) ->
    filter_text(Body, <<>>).

filter_text(<<>>, Acc) -> Acc;
filter_text(<<10, Rest/binary>>, Acc) -> filter_text(Rest, <<Acc/binary, 10>>);
filter_text(<<9, Rest/binary>>, Acc) -> filter_text(Rest, <<Acc/binary, 9>>);
filter_text(<<C/utf8, Rest/binary>>, Acc) when C < 32 -> filter_text(Rest, Acc);
filter_text(<<C/utf8, Rest/binary>>, Acc) -> filter_text(Rest, <<Acc/binary, C/utf8>>).

json_decode(Body) ->
    try
        {ok, z_json:decode(Body)}
    catch
        _:_ -> {error, json}
    end.

-spec req_body( z:context() ) -> {binary(), z:context()}.
req_body(Context) ->
    case cowmachine_req:req_body(?MAX_BODY_LENGTH, Context) of
        {undefined, Context1} -> {<<>>, Context1};
        {Body, Context1} -> {Body, Context1}
    end.

