%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Marc Worrell
%%
%% @doc Microsoft integration. Adds Microsoft/Azure login and other functionalities.

%% Copyright 2021 Marc Worrell
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

-module(mod_microsoft).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Microsoft").
-mod_description("Adds Microsoft login and other Microsoft/Azure related features.").
-mod_prio(400).
-mod_depends([ admin, authentication, mod_oauth2 ]).

-export([
    event/2
]).
-export([
    get_config/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").


% You have to add your Microsoft appid and secret to the config.
% By default, we only request access to the Microsoft user's e-mail address.
% The 'openid' scope is always added when requesting the access token.
-define(MICROSOFT_SCOPE, <<"email">>).

% The tenant: common, organizations, consumers or Microsot tenant id's
-define(MICROSOFT_TENANT, <<"common">>).


%% @doc Return the Microsoft appid, secret and scope
-spec get_config(z:context()) -> {AppId::string(), Secret::string(), Scope::string(), Tenant::string()}.
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_microsoft, appid, Context)),
      z_convert:to_list(m_config:get_value(mod_microsoft, appsecret, Context)),
      z_convert:to_list(m_config:get_value(mod_microsoft, scope, ?MICROSOFT_SCOPE, Context)),
      z_convert:to_list(m_config:get_value(mod_microsoft, tenant, ?MICROSOFT_TENANT, Context))
    }.

event(#submit{message=admin_microsoft}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_render:growl(?__("Saved the Microsoft settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the Microsoft settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        case is_setting(Key) of
                            true -> m_config:set_value(mod_microsoft, binary_to_atom(Key, 'utf8'), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)).

is_setting(<<"appid">>) -> true;
is_setting(<<"appsecret">>) -> true;
is_setting(<<"scope">>) -> true;
is_setting(<<"useauth">>) -> true;
is_setting(<<"tenant">>) -> true;
is_setting(_) -> false.
