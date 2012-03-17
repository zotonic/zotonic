%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% 
%% @doc Facebook integration. Adds Facebook login and other functionalities.

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

-module(mod_facebook).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Facebook").
-mod_description("Adds Facebook login and other Facebook related features.").
-mod_prio(400).

-export([
    observe_auth_logoff/3,
    observe_search_query/2,
    observe_admin_menu/3
]).
-export([get_config/1]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


% Default facebook appid and secret from the Zotonic Dev application.
-define(FACEBOOK_APPID, "106094309435783").
-define(FACEBOOK_APPSECRET, "50fb8d9d1ea8013c4c1640632c3ab706").
-define(FACEBOOK_SCOPE, "email"). %% by default, only request access to e-mail address.


%% @doc Reset the received facebook access token (as set in the session)
observe_auth_logoff(auth_logoff, AccContext, _Context) ->
    AccContext1 = case z_context:get_session(facebook_logon, AccContext) of
        true ->
            z_script:add_script(
                        "FB.logout(function() { window.location = '/'; }); setTimeout(function() { window.location='/'; }, 8000)", 
                        AccContext);
        _ ->
            AccContext
    end,
    z_context:set_session(facebook_logon, false, AccContext1),
    z_context:set_session(facebook_access_token, undefined, AccContext1).


%% @doc Return the facebook appid, secret and scope
%% @spec get_config(Context) -> {AppId, Secret, Scope}
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_facebook, appid, ?FACEBOOK_APPID, Context)),
      z_convert:to_list(m_config:get_value(mod_facebook, appsecret, ?FACEBOOK_APPSECRET, Context)),
      z_convert:to_list(m_config:get_value(mod_facebook, scope, ?FACEBOOK_SCOPE, Context))
    }.


%% @doc 
observe_search_query({search_query, {fql, Args}, OffsetLimit}, Context) ->
    m_facebook:search({fql, Args}, OffsetLimit, Context);
observe_search_query(_, _Context) ->
    undefined.

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_facebook,
                parent=admin_modules,
                label=?__("Facebook", Context),
                url={admin_facebook},
                visiblecheck={acl, use, ?MODULE}}
     
     |Acc].
