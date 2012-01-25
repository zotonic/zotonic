%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-07-13
%% @doc Add a complete new person and make it into an user.

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

-module(action_admin_identity_dialog_user_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_user_add, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


event(#postback{message={dialog_user_add, OnSuccess}}, Context) ->
    case z_acl:is_allowed(insert, identity, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess}
            ],
            z_render:dialog("Add a new user.", "_action_dialog_user_add.tpl", Vars, Context);
        false ->
            z_render:growl_error("Only administrators can add users.", Context)
    end;

%% @doc Delete an username from an user.
%% @spec event(Event, Context1) -> Context2
event(#submit{message={user_add, Props}}, Context) ->
    case z_acl:is_allowed(insert, identity, Context) of
        true ->
            NameFirst = z_context:get_q_validated("name_first", Context),
            NamePrefix = z_context:get_q("surprefix", Context),
            NameSur = z_context:get_q_validated("name_surname", Context),
            Title = case NamePrefix of
                [] -> [ NameFirst, " ", NameSur ];
                _ -> [ NameFirst, " ", NamePrefix, " ", NameSur ]
            end,

            PersonProps = [
                {is_published, true},
                {category, person},
                {title, lists:flatten(Title)},
                {name_first, NameFirst},
                {name_surname_prefix, NamePrefix},
                {name_surname, NameSur},
                {email, z_context:get_q_validated("email", Context)},
                {creator_id, self}
            ],
            
            F = fun(Ctx) ->
                case m_rsc:insert(PersonProps, Ctx) of
                    {ok, PersonId} ->
                        Username = z_context:get_q_validated("new_username", Ctx),
                        Password = z_context:get_q_validated("new_password", Ctx),
                        case m_identity:set_username_pw(PersonId, Username, Password, Ctx) of
                            ok -> {ok, PersonId};
                            {error, PWReason} -> throw({error, PWReason})
                        end;
                    {error, InsReason} ->
                        throw({error, InsReason})
                end
            end,
            
            case z_db:transaction(F, Context) of
                {ok, _PersonId} ->
                    Context1 = z_render:growl(["Created the user ",z_html:escape(Title), "."], Context),
                    z_render:wire(proplists:get_all_values(on_success, Props), Context1);
                {rollback, {Error, _CallStack}} ->
                    case Error of
                        {error, eexist} ->
                            z_render:growl_error("Duplicate username, please choose another username.", Context);
                        {error, eacces} ->
                            z_render:growl_error("You are not allowed to create the person page.", Context);
                        _OtherError ->
                            ?DEBUG(Error),
                            z_render:growl_error("Could not create the user. Sorry.", Context)
                    end
            end;
        false ->
            z_render:growl_error("Only administrators can add users.", Context)
    end.
