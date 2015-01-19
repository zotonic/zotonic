%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Receive real-time push messages from Instagram
%% See: http://instagram.com/developer/realtime/

%% Copyright 2015 Marc Worrell
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

-module(controller_instagram_push).
-author("Marc Worrell <marc@worrell.nl>").

-export([
        init/1,
        service_available/2,
        allowed_methods/2,
        content_types_provided/2,
        hubcheck/2,
        process_post/2
    ]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_qs(Context1),
    ?WM_REPLY(true, Context2).

allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain", hubcheck}], ReqData, Context}.

hubcheck(ReqData, Context0) ->
    Context = ?WM_REQ(ReqData, Context0),
    case z_context:get_q("hub.mode", Context) of
        "subscribe" ->
            VerifyToken = instagram_api:verify_token(Context),
            HubVerifyToken = z_convert:to_binary(z_context:get_q("hub.verify_token", Context)),
            case VerifyToken of
                HubVerifyToken ->
                    lager:info("[instagram] Hub verify of subscription"),
                    Challenge = z_context:get_q("hub.challenge", Context, ""),
                    ?WM_REPLY(Challenge, Context);
                _ ->
                    lager:error("[instagram] Hub verify of subscription failed token ~p expected ~p",
                                [HubVerifyToken, VerifyToken]),
                    ?WM_REPLY(<<>>, Context)
            end;
        _ ->
            ?WM_REPLY(<<>>, Context)
    end.

process_post(ReqData, Context0) ->
    {Body, ReqData1} = wrq:req_body(ReqData),
    Context = ?WM_REQ(ReqData1, Context0),
    {_Key, Secret, _Scope} = mod_instagram:get_config(Context),
    XHubSignature = z_convert:to_binary(
                        z_context:get_req_header("x-hub-signature", Context)),
    OurSignature = z_string:to_lower(
                        lists:flatten(
                            z_utils:hex_encode(
                                hmac_sha(Secret, Body)))),
    if 
        XHubSignature =:= OurSignature ->
            lager:debug("[instagram] Hub push received."),
            handle_push(Body, Context);
        true ->
            lager:error("[instagram] Hub post with mismatched X-Hub-Signature ~p expected ~p",
                        [XHubSignature, OurSignature])
    end,
    {x, RD, Context1} = ?WM_REPLY(x, Context),
    RD1 = wrq:append_to_resp_body(<<"ok">>, RD),
    {true, RD1, Context1}.

hmac_sha(Key, Data) ->
  case erlang:function_exported(crypto, hmac, 3) of
    true ->
      crypto:hmac(sha, Key, Data);
    false ->
      crypto:sha_mac(Key, Data)
  end.


handle_push(Data, Context) ->
    Updates = mochijson:binary_decode(Data),
    lists:foreach(fun({struct, Update}) -> update(Update, Context) end, Updates).

update(Update, Context) ->
    case proplists:get_value(<<"object">>, Update) of
        <<"tag">> ->
            Tag = proplists:get_value(<<"object_id">>, Update),
            Time = proplists:get_value(<<"time">>, Update),
            mod_instagram:poll(Tag, Time, Context);
        Other ->
            lager:info("[instagram] Dropping push update for object ~p", [Other])
    end.

