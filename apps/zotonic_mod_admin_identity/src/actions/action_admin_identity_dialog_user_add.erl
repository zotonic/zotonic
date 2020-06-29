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

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_user_add, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


event(#postback{message={dialog_user_add, OnSuccess}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess}
            ],
            z_render:dialog(?__("Add a new user", Context),"_action_dialog_user_add.tpl", Vars, Context);
        false ->
            z_render:growl_error(?__("Only administrators can add users.", Context), Context)
    end;

%% @doc Create user resource and a password identity
%% @spec event(Event, Context1) -> Context2
event(#submit{message={user_add, Args}}, Context) ->
    case z_acl:is_allowed(use, mod_admin_identity, Context) of
        true ->
            Username = z_context:get_q_validated(<<"new_username">>, Context),
            Password = z_context:get_q_validated(<<"new_password">>, Context),
            Email = z_context:get_q_validated(<<"email">>, Context),
            Qs = z_context:get_q_all_noz(Context),
            {ok, Props} = z_props:from_qs(Qs),
            Props1 = maps:remove(<<"new_username">>, Props),
            Props2 = maps:remove(<<"new_password">>, Props1),
            Props3 = maps:remove(<<"send_welcome">>, Props2),
            Vs = [
                {id, Props3}
            ],
            {Title, _} = z_template:render_to_iolist("_name.tpl", Vs, Context),
            PersonProps = Props3#{
                <<"is_published">> => true,
                <<"title">> => unicode:characters_to_binary(Title),
                <<"creator_id">> => self
            },
            F = fun(Ctx) ->
                case m_rsc:insert(PersonProps, Ctx) of
                    {ok, PersonId} ->
                        case m_identity:set_username_pw(PersonId, Username, Password, Ctx) of
                            ok -> ok;
                            {error, PWReason} -> throw({error, PWReason})
                        end,
                        case z_convert:to_bool(z_context:get_q(<<"send_welcome">>, Context)) of
                            true ->
                                Vars = [
                                    {id, PersonId},
                                    {username, Username}
                                ],
                                z_email:send_render(Email, "email_admin_new_user.tpl", Vars, Context);
                            false ->
                                nop
                        end,
                        {ok, PersonId};
                    {error, InsReason} ->
                        throw({error, InsReason})
                end
            end,

            case z_db:transaction(F, Context) of
                {ok, _PersonId} ->
                    Context1 = z_render:growl(["Created the user ",z_html:escape(Title), "."], Context),
                    z_render:wire(proplists:get_all_values(on_success, Args), Context1);
                {rollback, {Error, _CallStack}} ->
                    case Error of
                        {error, eexist} ->
                            z_render:growl_error(?__("Duplicate username, please choose another username.", Context), Context);
                        {error, eacces} ->
                            z_render:growl_error(?__("You are not allowed to create the person page.", Context), Context);
                        _OtherError ->
                            z_render:growl_error(?__("Could not create the user. Sorry.", Context), Context)
                    end
            end;
        false ->
            z_render:growl_error(?__("Only administrators can add users.", Context), Context)
    end.
