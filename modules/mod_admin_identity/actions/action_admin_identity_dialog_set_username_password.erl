%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-05-12
%% @doc Open a dialog with some fields to add or change an username/password identity

%% Copyright 2009 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

-define(PASSWORD_DOTS, "••••••").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnDelete = proplists:get_all_values(on_delete, Args),
    Postback = {set_username_password, Id, OnDelete},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={set_username_password, Id, OnDelete}}, Context) ->
    {Username, Password} = case m_identity:get_username(Id, Context) of
                                undefined -> {[], []};
                                Name -> {Name, ?PASSWORD_DOTS}
                            end,
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id},
        {username, Username},
        {password, Password},
        {on_delete, OnDelete}
    ],
    z_render:dialog("Set username/ password.", "_action_dialog_set_username_password.tpl", Vars, Context);

event(#submit{message=set_username_password}, Context) ->
    Id = z_convert:to_integer(z_context:get_q("id", Context)),
    Username = z_context:get_q_validated("new_username", Context),
    Password = z_context:get_q_validated("new_password", Context),
    
    case z_acl:is_allowed(use, mod_admin_identity, Context) orelse z_acl:user(Context) == Id of
        true ->
            case Password of
                ?PASSWORD_DOTS ->
                    % Only change the username
                    case m_identity:set_username(Id, Username, Context) of
                        ok ->
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, "Changed the username."}]}
                                ], Context);
                        {error, eexist} ->
                            z_render:wire({growl, [{text, "The username is already in use, please try another."},{type, "error"}]}, Context)
                    end;
                _Password ->
                    case m_identity:set_username_pw(Id, Username, Password, Context) of
                        {error, _} ->
                            %% Assume duplicate key violation, user needs to pick another username.
                            z_render:growl_error("The username is in use, please supply an unique username.", Context);
                        ok ->
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, "The new username/ password has been set."}]}
                                ], Context)
                    end
            end;
        false ->
            z_render:growl_error("Only an administrator or the user him/herself can set a password.", Context)
    end.
