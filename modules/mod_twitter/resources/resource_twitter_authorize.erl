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

-module(resource_twitter_authorize).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([init/1, service_available/2, charsets_provided/2, content_types_provided/2]).
-export([resource_exists/2, previously_existed/2, moved_temporarily/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", provide_content}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    %% @todo add the redirect page parameter of the logon page to the redirect url
    Context1 = ?WM_REQ(ReqData, Context),
    {ok, {Token, Secret}} = oauth_twitter_client:get_request_token(Context1),
    z_context:set_session(twitter_request_token, {Token, Secret}, Context),

    Page = get_page(Context1),
    RedirectUrl = lists:flatten(
                        z_context:abs_url(
                            lists:flatten(z_dispatcher:url_for(twitter_redirect, [{p,Page}], Context1)), 
                            Context1)),
    Location = oauth_twitter_client:authorize_url(Token)
        ++ "&oauth_callback=" ++ z_utils:url_encode(RedirectUrl),
    ?WM_REPLY({true, Location}, Context1).


%% @doc Get the page we should redirect to after a successful log on.
get_page(Context) ->
    case z_context:get_q("p", Context, []) of
        [] ->
            RD = z_context:get_reqdata(Context),
            case wrq:get_req_header("referer", RD) of
                undefined -> "/";
                Referrer -> Referrer
            end;
        Other ->
            Other
    end.

