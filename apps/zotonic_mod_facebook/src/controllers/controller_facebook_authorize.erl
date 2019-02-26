%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2013 Marc Worrell
%% @doc Redirect to the authorize uri of Facebook
%% See: http://developers.facebook.com/docs/authentication/

%% Copyright 2010-2013 Marc Worrell
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

-module(controller_facebook_authorize).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).


service_available(Context) ->
    Context2 = z_context:ensure_qs(Context),
    {true, Context2}.

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    StateId = z_ids:id(),
    StateData = {StateId, z_context:get_q_all_noz(Context)},
    Context1 = z_context:set_state_cookie(StateData, Context),
    Location = redirect_location(StateId, Context),
    {{true, Location}, Context1}.

redirect_location(StateId, Context) ->
    {AppId, _AppSecret, Scope} = mod_facebook:get_config(Context),
    RedirectUrl = z_context:abs_url(
                            z_dispatcher:url_for(facebook_redirect, Context),
                            Context),
    iolist_to_binary([
        <<"https://www.facebook.com/v2.9/dialog/oauth?client_id=">>,
        z_url:url_encode(AppId),
        "&redirect_uri=", z_url:url_encode(RedirectUrl),
        "&display=popup",
        "&response_type=code",
        "&scope=", z_url:url_encode(Scope),
        "&state=", StateId
    ]).
