%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-09-22
%% @doc Redirect to the authorize uri of Twitter
%% See: https://dev.twitter.com/docs/auth/oauth

%% Copyright 2011 Arjan Scherpenisse
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

-module(controller_twitter_authorize).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

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
    {ok, RequestToken = {Token, _Secret}} = oauth_twitter_client:get_request_token(Context),
    Context1 = z_context:set_state_cookie({twitter, RequestToken, z_context:get_q_all_noz(Context)}, Context),
    RedirectUrl = z_context:abs_url(
                            z_dispatcher:url_for(twitter_redirect, Context),
                            Context1),
    Lang = z_context:get_q(<<"lang">>, z_context:language(Context1)),
    Location = iolist_to_binary([
        oauth_twitter_client:authorize_url(Token),
        "&oauth_callback=", z_url:url_encode(RedirectUrl),
        "&lang=", z_url:url_encode(Lang)
    ]),
    {{true, Location}, Context1}.
