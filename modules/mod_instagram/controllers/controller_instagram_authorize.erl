%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Redirect to the authorize uri of Instagram
%% See: http://instagram.com/developer/authentication/

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

-module(controller_instagram_authorize).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, service_available/2, charsets_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).
-export([get_args/1]).
-export([redirect_location/1, redirect_uri/1]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Location = redirect_location(Context1),
    save_args(Context1),
    ?WM_REPLY({true, Location}, Context1).

redirect_location(Context) ->
    {AppId, _AppSecret, Scope} = mod_instagram:get_config(Context),
    Url = "https://api.instagram.com/oauth/authorize/?client_id="
        ++ z_utils:url_encode(AppId)
        ++ "&redirect_uri=" ++ z_utils:url_encode(redirect_uri(Context))
        ++ "&response_type=code",
    case Scope of
        [] -> Url;
        <<>> -> Url;
        undefined -> Url;
        _ -> Url  ++ "&scope=" ++ z_utils:url_encode(Scope)
    end.

redirect_uri(Context) ->
    z_convert:to_list(
                z_context:abs_url(
                        "/instagram/redirect",
                        Context)).

save_args(Context) ->
    z_context:set_session(?MODULE, z_context:get_q_all_noz(Context), Context).

get_args(Context) ->
    Args = z_context:get_session(?MODULE, Context),
    z_context:set_session(?MODULE, undefined, Context),
    case Args of
        L when is_list(L) -> L;
        undefined -> []
    end.
