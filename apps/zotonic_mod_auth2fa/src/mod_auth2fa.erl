%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Marc Worrell
%% @doc Add 2FA TOTP authentication
%% @end

%% Copyright 2019-2024 Marc Worrell
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

-mod_title("Two-Factor authentication").
-mod_description("Add two-factor authentication using TOTP").
-mod_prio(400).
-mod_depends([authentication, server_storage]).

-export([
    event/2,
    observe_admin_menu/3,
    observe_auth_postcheck/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

%% @doc Change the 2FA setting for the user group
event(#postback{ message={auth2fa_ug, Args} }, Context) ->
    case z_acl:is_allowed(use, mod_admin_config, Context) of
        true ->
            {id, Id} = proplists:lookup(id, Args),
            TV = z_convert:to_binary( z_context:get_q(triggervalue, Context) ),
            {ok, _} = m_rsc:update(Id, [ {acl_2fa, TV} ], Context),
            z_render:growl(?__("Changed the 2FA setting.", Context), Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to change the 2FA settings.", Context), Context)
    end;
event(#postback{ message={auth2fa_remove_confirm, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    UserId = z_acl:user(Context),
    case m_auth2fa:is_allowed_reset(UserId, Context) of
        true when Id =:= 1, UserId =/= 1 ->
            z_render:growl(?__("Only the admin can change the two-factor authentication of the admin.", Context), Context);
        true ->
            z_render:dialog(
                ?__("Remove two-factor authentication", Context),
                "_dialog_auth2fa_remove.tpl",
                [ {id, Id} ],
                Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to remove the 2FA.", Context), Context)
    end;
event(#submit{ message={auth2fa_remove, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    UserId = z_acl:user(Context),
    case m_auth2fa:is_allowed_reset(UserId, Context) of
        true when Id =:= 1, UserId =/= 1 ->
            z_render:growl(?__("Only the admin can change the two-factor authentication of the admin.", Context), Context);
        true when Id =:= 1, UserId =:= 1 ->
            ok = m_auth2fa:totp_disable(Id, Context),
            z_render:dialog_close(Context);
        true ->
            case z_acl:is_allowed(use, mod_admin_identity, Context) of
                true ->
                    ok = m_auth2fa:totp_disable(Id, Context),
                    z_render:dialog_close(Context);
                false ->
                    Code = z_convert:to_binary(z_context:get_q(<<"passcode">>, Context)),
                    case m_auth2fa:is_valid_totp(UserId, Code, Context) of
                        true ->
                            ok = m_auth2fa:totp_disable(Id, Context),
                            z_render:dialog_close(Context);
                        false ->
                            OnError = proplists:get_all_values(onerror, Args),
                            z_render:wire(OnError, Context)
                    end
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to remove the 2FA.", Context), Context)
    end;
event(#postback{ message={request_2fa, _Args} }, Context) ->
    case z_auth:is_auth(Context) of
        true ->
            UserId = z_acl:user(Context),
            case m_auth2fa:is_totp_enabled(UserId, Context) of
                true ->
                    Context;
                false ->
                    m_auth2fa:set_totp_requested(Context),
                    {ok, {ImageUrl, PassCode}} = m_auth2fa:new_totp_image_url(Context),
                    Vars = [
                        {backdrop, static},
                        {passcode, PassCode},
                        {image_url, ImageUrl}
                    ],
                    z_render:dialog(
                        ?__("Add two-factor authentication", Context),
                        "_dialog_auth2fa_passcode.tpl",
                        Vars,
                        Context)
            end;
        false ->
            Context
    end;
event(#submit{ message={auth2fa_set, Args} }, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    {secret, Secret} = proplists:lookup(secret, Args),
    UserId = z_acl:user(Context),
    if
        Id =:= UserId ->
            Secret1 = z_auth2fa_base32:decode(Secret),
            Code = z_context:get_q(<<"passcode">>, Context),
            case m_auth2fa:is_valid_totp_test(Secret1, Code) of
                true ->
                    ok = m_auth2fa:totp_set(UserId, Secret1, Context),
                    Context1 = z_render:dialog_close(Context),
                    z_render:wire({alert, [
                        {title, ?__("Success", Context1)},
                        {text, ?__("Two-factor authentication has been enabled for your account.", Context1)}
                    ]}, Context1);
                false ->
                    z_render:wire(proplists:get_all_values(onerror, Args), Context)
            end;
        true ->
            z_render:growl(?__("Sorry, you are not allowed to set the 2FA.", Context), Context)
    end;
event(#postback{ message={dialog_2fa, _Args} }, Context) ->
    case z_acl:user(Context) of
        undefined ->
            Context;
        _UserId ->
            {ok, {ImageUrl, PassCode}} = m_auth2fa:new_totp_image_url(Context),
            Vars = [
                {backdrop, static},
                {passcode, PassCode},
                {image_url, ImageUrl}
            ],
            z_render:dialog(
                ?__("Add two-factor authentication", Context),
                "_dialog_auth2fa_passcode.tpl",
                Vars,
                Context)
    end.


%% @doc Add admin menu for external services.
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id = admin_auth2fa_config,
                parent = admin_auth,
                label = ?__("Two-factor authentication", Context),
                url = {admin_auth2fa_config},
                visiblecheck = {acl, use, mod_admin_config}}

     | Acc ].

%% @doc Check the 2FA code, called after password check passed.
observe_auth_postcheck(#auth_postcheck{ id = UserId, query_args = QueryArgs }, Context) ->
    case m_auth2fa:is_totp_enabled(UserId, Context) of
        true ->
            case z_string:trim( z_convert:to_binary( maps:get(<<"passcode">>, QueryArgs, <<>>) ) ) of
                <<>> ->
                    {error, need_passcode};
                PassCode ->
                    case m_auth2fa:is_valid_totp(UserId, PassCode, Context) of
                        true -> undefined;
                        false -> {error, passcode}
                    end
            end;
        false ->
            % Could also have a POST of the new passcode secret to be set.
            % In that case the passcode can be set for the user and 'undefined'
            % returned
            case z_convert:to_integer(m_config:get_value(mod_auth2fa, mode, Context)) of
                3 ->
                    case maps:get(<<"code-new">>, QueryArgs, undefined) of
                        CodeNew when is_binary(CodeNew), CodeNew =/= <<>> ->
                            Secret = z_auth2fa_base32:decode(CodeNew),
                            Code = maps:get(<<"test_passcode">>, QueryArgs, <<>>),
                            case m_auth2fa:is_valid_totp_test(Secret, Code) of
                                true ->
                                    % Save the new 2FA code
                                    m_auth2fa:totp_set(UserId, Secret, Context),
                                    undefined;
                                false ->
                                    {error, set_passcode_error}
                            end;
                        _ ->
                            {error, set_passcode}
                    end;
                _ ->
                    undefined
            end
    end.
