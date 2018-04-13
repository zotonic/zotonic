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

-export([
    service_available/1,
    charsets_provided/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).
-export([get_args/1]).
-export([redirect_location/1, redirect_uri/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context2 = z_context:ensure_qs(Context),
    {true, Context2}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    Location = redirect_location(Context),
    save_args(Context),
    {{true, Location}, Context}.

redirect_location(Context) ->
    {AppId, _AppSecret, Scope} = mod_instagram:get_config(Context),
    Url = iolist_to_binary([
        <<"https://api.instagram.com/oauth/authorize/?client_id=">>,
        z_url:url_encode(AppId),
        "&redirect_uri=", z_url:url_encode(redirect_uri(Context)),
        "&response_type=code"
    ]),
    case Scope of
        [] -> Url;
        <<>> -> Url;
        undefined -> Url;
        _ -> <<Url/binary, "&scope=", (z_convert:to_binary(z_url:url_encode(Scope)))/binary>>
    end.

redirect_uri(Context) ->
    z_context:abs_url(<<"/instagram/redirect">>, Context).

save_args(Context) ->
    z_context:set_session(?MODULE, z_context:get_q_all_noz(Context), Context).

get_args(Context) ->
    Args = z_context:get_session(?MODULE, Context),
    z_context:set_session(?MODULE, undefined, Context),
    case Args of
        L when is_list(L) -> L;
        undefined -> []
    end.
