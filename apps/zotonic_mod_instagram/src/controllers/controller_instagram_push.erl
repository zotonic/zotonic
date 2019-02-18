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
        allowed_methods/1,
        content_types_provided/1,
        process/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[<<"POST">>, <<"GET">>], Context}.

content_types_provided(Context) ->
    {[ {<<"text">>, <<"plain">>, []} ], Context}.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context0) ->
    {Body, Context} = cowmachine_req:req_body(Context0),
    {_Key, Secret, _Scope} = mod_instagram:get_config(Context),
    XHubSignature = z_convert:to_binary(
                        z_context:get_req_header(<<"x-hub-signature">>, Context)),
    OurSignature = z_string:to_lower(
                        lists:flatten(
                            z_utils:hex_encode(
                                crypto:hmac(sha, Secret, Body)))),
    if
        XHubSignature =:= OurSignature ->
            lager:debug("[instagram] Hub push received."),
            handle_push(Body, Context);
        true ->
            lager:error("[instagram] Hub post with mismatched X-Hub-Signature ~p expected ~p",
                        [XHubSignature, OurSignature])
    end,
    Context1 = cowmachine_req:set_resp_body(<<"ok">>, Context),
    {true, Context1};
process(<<"GET">>, _AcceptedCT, _ProvidedCT, Context) ->
    case z_context:get_q(<<"hub.mode">>, Context) of
        <<"subscribe">> ->
            VerifyToken = instagram_api:verify_token(Context),
            HubVerifyToken = z_context:get_q(<<"hub.verify_token">>, Context),
            case VerifyToken of
                HubVerifyToken ->
                    lager:info("[instagram] Hub verify of subscription"),
                    Challenge = z_context:get_q(<<"hub.challenge">>, Context, <<>>),
                    {Challenge, Context};
                _ ->
                    lager:error("[instagram] Hub verify of subscription failed token ~p expected ~p",
                                [HubVerifyToken, VerifyToken]),
                    {<<>>, Context}
            end;
        _ ->
            {<<>>, Context}
    end.


handle_push(Data, Context) ->
    Updates = z_json:decode(Data),
    lists:foreach(fun(Update) -> update(Update, Context) end, Updates).

update(Update, Context) ->
    case maps:get(<<"object">>, Update, undefined) of
        #{
            <<"object_id">> := Tag,
            <<"time">> := Time
        } ->
            mod_instagram:poll(Tag, Time, Context);
        Other ->
            lager:info("[instagram] Dropping push update for object ~p", [Other])
    end.

