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
        hubcheck/1,
        process_post/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[<<"POST">>, <<"GET">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/plain">>, hubcheck}], Context}.

hubcheck(Context) ->
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

process_post(Context0) ->
    {Body, Context} = cowmachine_req:req_body(Context0),
    {_Key, Secret, _Scope} = mod_instagram:get_config(Context),
    XHubSignature = z_convert:to_binary(
                        z_context:get_req_header(<<"x-hub-signature">>, Context)),
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
    Context1 = cowmachine_req:set_resp_body(<<"ok">>, Context),
    {true, Context1}.

hmac_sha(Key, Data) ->
    crypto:hmac(sha, Key, Data).


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

