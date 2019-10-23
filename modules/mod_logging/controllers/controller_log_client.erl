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
    init/1,
    service_available/2,
    allowed_methods/2,
    process_post/2
]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context1 = z_context:new_request(ReqData, DispatchArgs, ?MODULE),
    RD1 = wrq:set_resp_header("Access-Control-Allow-Origin", "*", ReqData),
    {true, RD1, Context1}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

process_post(ReqData, Context) ->
    {Body, RD1} = wrq:req_body(ReqData),
    Context1 = ?WM_REQ(RD1, Context),
    Context2 = z_context:continue_session(Context1),
    log(Body, Context2),
    ?WM_REPLY(true, Context2).

log(<<>>, _Context) ->
    ok;
log(Body, Context) ->
    Body1 = filter_text(Body),
    case json_decode(Body1) of
        {ok, {struct, Props}} ->
            case mod_logging:is_ui_ratelimit_check(Context) of
                true ->
                    m_log_ui:insert_event(Props, Context),
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
        {ok, mochijson2:decode(Body)}
    catch
        _:_ -> {error, json}
    end.

