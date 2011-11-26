%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-11
%% @doc Redirect to the authorize uri of Facebook
%% See: http://developers.facebook.com/docs/authentication/

%% Copyright 2010 Marc Worrell
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

-module(resource_facebook_authorize).
-author("Marc Worrell <marc@worrell.nl>").

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
    {AppId, _AppSecret, Scope} = mod_facebook:get_config(Context1),
    Page = get_page(Context1),
    RedirectUrl = lists:flatten(
                        z_context:abs_url(
                            z_dispatcher:url_for(facebook_redirect, [{p,Page}], Context1),
                            Context1)),
    Location = "https://www.facebook.com/dialog/oauth?client_id="
                ++ z_utils:url_encode(AppId)
                ++ "&redirect_uri=" ++ z_utils:url_encode(RedirectUrl)
                ++ "&scope=" ++ Scope,
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

