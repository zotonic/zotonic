%% @copyright 2015-2024 Marc Worrell
%% @doc Support routines for editing/creating resources
%% @end

%% Copyright 2015-2024 Marc Worrell
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


-module(admin_acl_rules_rsc).

-export([
    event/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message={switch_rule_state, Args}}, Context) ->
    {code, SignedCode} = proplists:lookup(code, Args),
    {state, State} = proplists:lookup(state, Args),
    case {m_acl_rule:is_valid_code(SignedCode, Context), State} of
        {true, edit} ->
            z_mqtt:publish(
                [ <<"~client">>, <<"model">>, <<"auth">>, <<"post">>, <<"refresh">> ],
                #{
                    acl_user_groups_code => SignedCode,
                    acl_user_groups_state => <<"edit">>
                },
                Context);
        {_, _} ->
            z_mqtt:publish(
                [ <<"~client">>, <<"model">>, <<"auth">>, <<"post">>, <<"refresh">> ],
                #{
                    acl_user_groups_state => undefined
                },
                Context)
    end,
    timer:sleep(300),
    z_render:wire({reload, []}, Context);
event(#submit{message={change_catcg, Args}}, Context) ->
    Id = proplists:get_value(id, Args),
    ErrorDiv = proplists:get_value(error, Args),
    CatId = m_rsc:rid(z_context:get_q(<<"category_id">>, Context), Context),
    CGId = m_rsc:rid(z_context:get_q(<<"content_group_id">>, Context), Context),
    case check_catcg(CGId, CatId, ErrorDiv, Context) of
        {true, ContextError} ->
            update(Id, CatId, CGId, Context, ContextError);
        {false, ContextError} ->
            ContextError
    end;
event(#z_msg_v1{data=Data}, Context) ->
    handle_cmd(proplists:get_value(<<"cmd">>, Data), Data, Context).

handle_cmd(<<"reload_cgsel">>, Data, Context) ->
    CatId = m_rsc:rid(proplists:get_value(<<"cat_id">>, Data), Context),
    CGId1 = m_rsc:rid(proplists:get_value(<<"cg_id1">>, Data), Context),
    CGId2 = m_rsc:rid(proplists:get_value(<<"cg_id2">>, Data), Context),
    CGMenu = m_hierarchy:menu('content_group', Context),
    CGAllowed = allowed_content_groups(CatId, [CGId1, CGId2], CGMenu, Context),
    CGId = case {lists:member(CGId1, CGAllowed),lists:member(CGId2, CGAllowed)} of
                {true, _} -> CGId1;
                {_, true} -> CGId2;
                _ -> undefined
           end,
    Vars = [
        {is_cg_reload, true},
        {cg_id, CGId},
        {cg_allowed, CGAllowed},
        {cg_menu, CGMenu},
        {cgsel_id, proplists:get_value(<<"cgsel">>, Data)}
    ],
    z_render:update(proplists:get_value(<<"cgwrap">>, Data),
                    #render{
                        template="_admin_content_group_dropdown.tpl",
                        vars=Vars
                    },
                    Context).

allowed_content_groups(CatId, ExtraIds, CGMenu, Context) ->
    CGIds = acl_user_groups_rules:tree_ids(CGMenu)
            ++ acl_user_groups_checks:has_collab_groups(Context)
            ++ ExtraIds,
    lists:filter(fun
                    (undefined) -> false;
                    (CGId) -> can_insert_category(CGId, CatId, Context)
                 end,
                 CGIds).

update(Id, CatId, CGId, Context, ErrorContext) ->
    Props = [
            {category_id,CatId},
            {content_group_id,CGId}
        ],
    case  m_rsc:update(Id, Props, Context) of
        {ok, _} ->
            z_render:wire([{dialog_close, []}, {reload, []}], Context);
        {error, eacces} ->
            z_render:growl_error("You don't have permission to edit this page.", ErrorContext);
        {error, Message} when is_list(Message); is_binary(Message) ->
            z_render:growl_error(Message, ErrorContext)
    end.

check_catcg(CGId, CatId, ErrorDiv, Context) ->
    IsOk = can_insert_category(CGId, CatId, Context),
    Context1 = case IsOk of
                    true -> z_render:wire({hide, [{target,ErrorDiv}]}, Context);
                    false -> z_render:wire({show, [{target,ErrorDiv}]}, Context)
               end,
    {IsOk, Context1}.


% Check via notification if this insert is allowed, the notification allows extra observers to
% hook into this check.
can_insert_category(CGId, CatId, Context) ->
    z_acl:is_allowed(
        insert,
        #acl_rsc{
            id = undefined,
            category = CatId,
            props = #{
                <<"content_group_id">> => CGId
            }
        },
        Context).
