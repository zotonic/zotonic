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
    service_available/1,
    charsets_provided/1,
    content_types_provided/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1
]).
-export([get_args/1]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context2 = z_context:ensure_qs(Context),
    z_context:lager_md(Context2),
    {true, Context2}.

charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.

content_types_provided(Context) ->
    {[{<<"text/html">>, provide_content}], Context}.

resource_exists(Context) ->
    {false, Context}.

previously_existed(Context) ->
    {true, Context}.

moved_temporarily(Context) ->
    %% @todo add the redirect page parameter of the logon page to the redirect url
    {ok, {Token, Secret}} = oauth_twitter_client:get_request_token(Context),
    z_context:set_session(twitter_request_token, {Token, Secret}, Context),
    RedirectUrl = z_context:abs_url(
                            z_dispatcher:url_for(twitter_redirect, Context),
                            Context),
    Lang = z_context:get_q(<<"lang">>, z_context:language(Context)),
    Location = iolist_to_binary([
        oauth_twitter_client:authorize_url(Token),
        "&oauth_callback=", z_url:url_encode(RedirectUrl),
        "&lang=", Lang
    ]),
    save_args(Context),
    {{true, Location}, Context}.

save_args(Context) ->
    z_context:set_session(?MODULE, z_context:get_q_all_noz(Context), Context).

get_args(Context) ->
    Args = z_context:get_session(?MODULE, Context),
    z_context:set_session(?MODULE, undefined, Context),
    case Args of
        L when is_list(L) -> L;
        undefined -> []
    end.
