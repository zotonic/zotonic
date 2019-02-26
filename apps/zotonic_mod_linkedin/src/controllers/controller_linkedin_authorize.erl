%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Redirect to the authorize uri of LinkedIn
%% See: https://developer.linkedin.com/documents/authentication

%% Copyright 2014 Marc Worrell
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

-module(controller_linkedin_authorize).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    StateId = z_ids:id(),
    SecretData = {StateId, z_context:get_q_all_noz(Context)},
    Context1 = z_context:set_state_cookie(SecretData, Context),
    Location = redirect_location(StateId, Context1),
    {{true, Location}, Context1}.

redirect_location(StateId, Context) ->
    {AppId, _AppSecret, Scope} = mod_linkedin:get_config(Context),
    RedirectUrl = z_context:abs_url(
                            z_dispatcher:url_for(linkedin_redirect, Context),
                            Context),
    iolist_to_binary([
        <<"https://www.linkedin.com/uas/oauth2/authorization?response_type=code">>,
        "&client_id=", z_url:url_encode(AppId),
        "&redirect_uri=", z_url:url_encode(RedirectUrl),
        "&state=", StateId,
        "&scope=", z_url:url_encode(Scope)
    ]).
