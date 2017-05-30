%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Use LinkedIn for logon
%%
%% Setup instructions:
%% * Enable the mod_linkedin module
%% * Configure in the admin the linkedin keys (Auth -> External Services)

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

-module(mod_linkedin).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("LinkedIn").
-mod_description("Use LinkedIn for logon.").
-mod_prio(500).
-mod_depends([admin, authentication]).
-mod_provides([linkedin]).

%% interface functions
-export([
        event/2,
        get_config/1
    ]).

-define(LINKEDIN_SCOPE, "r_basicprofile r_emailaddress").

-include_lib("zotonic_core/include/zotonic.hrl").


event(#submit{message=admin_linkedin}, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            save_settings(Context),
            z_render:growl(?__("Saved the LinkedIn settings.", Context), Context);
        false ->
            z_render:growl(?__("You don't have permission to change the LinkedIn settings.", Context), Context)
    end.

save_settings(Context) ->
    lists:foreach(fun ({Key, Value}) ->
                        case is_setting(Key) of
                            true -> m_config:set_value(mod_linkedin, binary_to_atom(Key, 'utf8'), Value, Context);
                            false -> ok
                        end
                  end,
                  z_context:get_q_all_noz(Context)).

is_setting(<<"appid">>) -> true;
is_setting(<<"appsecret">>) -> true;
is_setting(<<"useauth">>) -> true;
is_setting(_) -> false.


%% @doc Return the linkedin appid, secret and scope
%% @spec get_config(Context) -> {AppId, Secret, Scope}
get_config(Context) ->
    { z_convert:to_list(m_config:get_value(mod_linkedin, appid, Context)),
      z_convert:to_list(m_config:get_value(mod_linkedin, appsecret, Context)),
      ?LINKEDIN_SCOPE
    }.

