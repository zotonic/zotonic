%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Add 2FA TOTP authentication

%% Copyright 2010-2019 Marc Worrell
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

-module(mod_auth2fa).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Authentication 2FA").
-mod_description("Add 2FA authentication using TOTP").
-mod_prio(600).
-mod_depends([authentication]).

-export([
    event/2,
    observe_admin_menu/3,
    observe_auth_postcheck/2
]).

-include("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

%% @doc Change the 2FA setting for the user group
event(#postback{ message={auth2fa_ug, Args} }, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            {id, Id} = proplists:lookup(id, Args),
            TV = z_convert:to_binary( z_context:get_q(triggervalue, Context) ),
            {ok, _} = m_rsc:update(Id, [ {acl_force_2fa, TV} ], Context),
            z_render:growl(?__("Changed the 2FA setting.", Context), Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to change the 2FA settings.", Context), Context)
    end;
event(#postback{ message={auth2fa_remove, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    case z_acl:is_allowed(use, mod_admin_identity, Context)
        orelse Id =:= z_acl:user(Context)
    of
        true ->
            ok = m_auth2fa:totp_disable(Id, Context),
            Context;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to remove the 2FA.", Context), Context)
    end.

%% @doc Add admin menu for external services.
observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id = admin_auth2fa_config,
                parent = admin_auth,
                label = ?__("2FA configuration", Context),
                url = {admin_auth2fa_config},
                visiblecheck = {acl, use, mod_admin_config}}

     | Acc ].

%% @doc Check the 2FA code, called after password check passed.
observe_auth_postcheck(#auth_postcheck{ id = UserId, query_args = QueryArgs }, Context) ->
    case m_auth2fa:is_totp_enabled(UserId, Context) of
        true ->
            case z_string:trim( z_convert:to_binary( proplists:get_value("passcode", QueryArgs) ) ) of
                <<>> ->
                    {error, need_passcode};
                PassCode ->
                    case m_auth2fa:is_valid_totp(UserId, PassCode, Context) of
                        true -> undefined;
                        false -> {error, passcode}
                    end
            end;
        false ->
            undefined
    end.
