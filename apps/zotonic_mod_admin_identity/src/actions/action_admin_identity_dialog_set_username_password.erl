%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Open a dialog with some fields to add or change an username/password identity

%% Copyright 2009-2023 Marc Worrell
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

-module(action_admin_identity_dialog_set_username_password).
-author("Marc Worrell <marc@worrell.nl>").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    case Id of
        undefined ->
            ?LOG_WARNING(#{
                in => zotonic_mod_admin_identity,
                text => <<"Set username/password for unknown resource">>,
                result => error,
                reason => admin_user,
                id => Id
            }),
            {<<>>, Context};
        ?ACL_ADMIN_USER_ID ->
            ?LOG_ERROR(#{
                in => zotonic_mod_admin_identity,
                text => <<"Set username/password not allowed for the admin user">>,
                result => error,
                reason => admin_user
            }),
            {<<>>, Context};
        UserId ->
            OnDelete = proplists:get_all_values(on_delete, Args),
            Postback = {set_username_password, UserId, OnDelete},
        	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
        	{PostbackMsgJS, Context}
    end.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={set_username_password, Id, OnDelete}}, Context) ->
    Username = m_identity:get_username(Id, Context),
    Vars = [
        {delegate, atom_to_binary(?MODULE, 'utf8')},
        {id, Id},
        {username, Username},
        {on_delete, OnDelete}
    ],
    z_render:dialog(?__("Set username / password", Context), "_action_dialog_set_username_password.tpl", Vars, Context);

event(#postback{message={delete_username, Args}}, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    case {z_acl:is_read_only(Context), z_acl:is_allowed(use, mod_admin_identity, Context), z_acl:user(Context)} of
        {true, _, _} ->
            z_render:growl_error(?__("Only an administrator or the user him/herself can set a password.", Context), Context);
        {false, _, Id} ->
            z_render:growl_error(?__("Sorry, you can not remove your own username.", Context), Context);
        {false, true, _} when Id =:= 1 ->
            z_render:growl_error(?__("The admin user can not be removed.", Context), Context);
        {false, true, _} ->
            case m_identity:delete_username(Id, Context) of
                ok ->
                    Context1 = z_render:growl(?__("Deleted the user account.", Context), Context),
                    z_render:wire(proplists:get_all_values(on_delete, Args), Context1);
                {error, _} ->
                    z_render:growl_error(?__("You are not allowed to delete this user account.", Context), Context)
            end;
        {false, false, _} ->
            z_render:growl_error(?__("Only an administrator or the user him/herself can set a password.", Context), Context)
    end;

event(#submit{message=set_username_password}, Context) ->
    Id = z_convert:to_integer(z_context:get_q(<<"id">>, Context)),
    Username = z_context:get_q_validated(<<"new_username">>, Context),
    Password = z_context:get_q_validated(<<"new_password">>, Context),
    case not z_acl:is_read_only(Context)
        andalso (
            z_acl:is_allowed(use, mod_admin_identity, Context)
            orelse z_acl:user(Context) == Id)
    of
        true ->
            case Password of
                <<>> ->
                    % Only change the username
                    case m_identity:set_username(Id, Username, Context) of
                        ok ->
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, ?__("Changed the username.", Context)}]}
                                ], Context);
                        {error, eexist} ->
                            z_render:wire({growl, [{text, ?__("The username is already in use, please try another.", Context)},{type, "error"}]}, Context)
                    end;
                _Password ->
                    case m_identity:set_username_pw(Id, Username, Password, Context) of
                        {error, _} ->
                            %% Assume duplicate key violation, user needs to pick another username.
                            z_render:growl_error(?__("The username is already in use, please try another.", Context), Context);
                        ok ->
                            case z_convert:to_bool(z_context:get_q(<<"send_welcome">>, Context)) of
                                true ->
                                    Vars = [{id, Id},
                                            {username, Username},
                                            {password, Password}],
                                    z_email:send_render(m_rsc:p(Id, email_raw, Context), "email_admin_new_user.tpl", Vars, Context);
                                false ->
                                    nop
                            end,
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, ?__("The new username/ password has been set.", Context)}]}
                                ], Context)
                    end
            end;
        false ->
            z_render:growl_error(?__("Only an administrator or the user him/herself can set a password.", Context), Context)
    end.
