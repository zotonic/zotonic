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
    event/2
]).
-export([
    get_config/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").


% You have to add your Facebook appid and secret to the config.
% By default, we only request access to the Facebook user's e-mail address.
-define(FACEBOOK_SCOPE, <<"email">>).


%% @doc Reset the received facebook access token (as set in the session)
observe_auth_logoff(#auth_logoff{}, AccContext, _Context) ->
    z_context:set_session(facebook_logon, false, AccContext),
    z_context:set_session(facebook_access_token, undefined, AccContext).


%% @doc Return the facebook appid, secret and scope
%% @spec get_config(Context) -> {AppId, Secret, Scope}
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_facebook, appid, Context)),
      z_convert:to_list(m_config:get_value(mod_facebook, appsecret, Context)),
      z_convert:to_list(m_config:get_value(mod_facebook, scope, ?FACEBOOK_SCOPE, Context))
    }.


%% @doc
observe_search_query({search_query, {fql, Args}, OffsetLimit}, Context) ->
    m_facebook:search({fql, Args}, OffsetLimit, Context);
observe_search_query(_, _Context) ->
    undefined.


event(#submit{message=admin_facebook}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_render:growl(?__("Saved the Facebook settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Facebook settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        case is_setting(Key) of
                            true -> m_config:set_value(mod_facebook, binary_to_atom(Key, 'utf8'), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)).

is_setting(<<"appid">>) -> true;
is_setting(<<"appsecret">>) -> true;
is_setting(<<"scope">>) -> true;
is_setting(<<"useauth">>) -> true;
is_setting(_) -> false.


% %% @doc Redirect to facebook, keep extra arguments in query arg
% event(#postback{message={logon_redirect, Args}}, Context) ->
%     Pickled = z_utils:pickle(Args, Context),
%     z_render:wire([
%             {alert, [
%                     {title, ?__("One moment please", Context)},
%                     {text, ?__("Redirecting to Facebook", Context)},
%                     only_text
%                 ]},
%             {redirect, [{dispatch, facebook_authorize}, {pk, Pickled}]}
%         ], Context).

