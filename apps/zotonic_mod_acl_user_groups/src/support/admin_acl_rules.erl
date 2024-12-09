%% @copyright 2015 Arjan Scherpenisse
%% @doc Admin callbacks for the user groups

%% Copyright 2015 Arjan Scherpenisse
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

-module(admin_acl_rules).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([event/2]).

event(#postback{message={admin_connect_select, Args}} = Msg, Context) ->
    case proplists:get_value(subject_id, Args) of
        undefined ->
            event_admin(Msg, Context);
        SubjectId when is_integer(SubjectId) ->
            % Select collab group dialog
            SelectId = m_rsc:rid(z_context:get_q(<<"select_id">>, Context), Context),
            case m_rsc:is_a(SelectId, acl_collaboration_group, Context) of
                true ->
                    case m_rsc:update(SubjectId, [{content_group_id, SelectId}], Context) of
                    {ok, _} ->
                        z_render:wire([
                                {dialog_close, []},
                                {reload, []}
                            ], Context);
                    {error, eacces} ->
                        z_render:growl(?__("You are not allowed to move to this collaboration group.", Context), Context);
                    {error, _} ->
                            z_render:growl(?__("Could not move to collaboration group.", Context), Context)
                    end;
                false ->
                    z_render:growl(?__("Please select a collaboration_group", Context), Context)
            end
    end;
event(Msg, Context) ->
    event_admin(Msg, Context).


event_admin(Msg, Context) ->
    case mod_acl_user_groups:is_acl_admin(Context) of
        true ->
            event1(Msg, Context);
        false ->
            z_render:growl_error(?__("You are not allowed to perform this action", Context), Context)
    end.

event1(#postback_notify{message= <<"feedback">>, target=TargetId}, Context) ->
    Vars = [
        {cat, acl_collaboration_group},
        {text, z_context:get_q(<<"triggervalue">>, Context)}
    ],
    z_render:wire([
        {remove_class, [{target, TargetId}, {class, "loading"}]},
        {update, [{target, TargetId}, {template, "_admin_acl_rule_collab_li_list.tpl"} | Vars]}
    ], Context);
event1(#postback{message={collab_select, Args}}, Context) ->
    z_render:update(
                "acl-cg-collab-select",
                #render{
                    template="_admin_acl_rule_collab_select.tpl",
                    vars=[
                        {content_group_id, proplists:get_value(id, Args)}
                    ]
                },
                Context);

event1(#submit{message={add_rule, [{kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all_noz(Context),
    Row1 = normalize_values(Row),
    {ok, _NewRuleId} = m_acl_rule:insert(Kind, Row1, Context),
    Context;

event1(#submit{message={update_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    Row = z_context:get_q_all_noz(Context),
    Row1 = normalize_values(Row),
    m_acl_rule:update(Kind, RuleId, Row1, Context),
    Context;

event1(#postback{message={remove_rule, [{id, RuleId}, {kind, Kind}]}}, Context) ->
    ok = m_acl_rule:delete(Kind, RuleId, Context),
    Context;

event1(#postback{message={revert, _Args}}, Context) ->
    ok = m_acl_rule:revert(rsc, Context),
    ok = m_acl_rule:revert(collab, Context),
    ok = m_acl_rule:revert(module, Context),
    z_render:growl(?__("Reverted rules", Context), Context);

event1(#postback{message={publish, _Args}}, Context) ->
    ok = m_acl_rule:publish(rsc, Context),
    ok = m_acl_rule:publish(collab, Context),
    ok = m_acl_rule:publish(module, Context),
    z_render:growl(?__("Publish successful", Context), Context);

event1(#submit{ message = {set_upload_permissions, _Args} }, Context) ->
    QIds = z_context:get_q_all("id", Context),
    lists:foreach(
        fun(QId) ->
            case m_rsc:rid(QId, Context) of
                undefined ->
                    skip;
                Id ->
                    Size = z_convert:to_integer(z_context:get_q("size-"++QId, Context)),
                    Mime = z_convert:to_binary(z_context:get_q("mime-"++QId, Context)),
                    Props = [
                        {acl_upload_size, Size},
                        {acl_mime_allowed, z_string:trim(Mime)}
                    ],
                    m_rsc:update(Id, Props, Context)
            end
        end,
        QIds),
    z_render:growl(?__("Upload settings saved", Context), Context);

event1(#submit{message={acl_rule_import, []}}, Context) ->
    #upload{tmpfile=TmpFile} = z_context:get_q_validated(<<"upload_file">>, Context),
    {ok, Binary} = file:read_file(TmpFile),
    ContextAsync = z_context:prune_for_async(Context),
    z_proc:spawn_md(fun() ->
                    Data = binary_to_term(Binary),
                    acl_user_groups_export:import(Data, ContextAsync)
                 end),
    z_render:dialog_close(z_render:growl(?__("Importing, the list of rules will refresh after importing.", Context), Context));

event1(#submit{message=acl_collab_config}, Context) ->
    CollabGroupLink = z_context:get_q(collab_group_link, Context),
    CollabGroupUpdate = z_context:get_q(collab_group_update, Context),
    m_config:set_value(mod_acl_user_groups, collab_group_link, CollabGroupLink, Context),
    m_config:set_value(mod_acl_user_groups, collab_group_update, CollabGroupUpdate, Context),
    z_render:growl(?__("Saved collaboration group settings", Context), Context).

normalize_values(Row) ->
    {Actions, Rest} =
        lists:foldl(
            fun({<<"action$", A/binary>>, <<"on">>}, {Actions, Rest}) ->
                  {[A|Actions], Rest};
               ({<<"action$", _/binary>>, _}, Acc) ->
                  Acc;
               ({<<"module">>, M}, {A,Rest}) ->
                  {A, [{module, M} | Rest]};
               ({<<"is_owner">>, V}, {A, Rest}) ->
                  {A, [{is_owner,z_convert:to_bool(V)}|Rest]};
               ({<<"is_block">>, V}, {A, Rest}) ->
                  {A, [{is_block,z_convert:to_bool(V)}|Rest]};
               ({K, V}, {A, Rest}) ->
                  {A, [{z_convert:to_atom(K),val(V)}|Rest]}
            end,
            {[], []},
            Row),
    [{actions, Actions} | Rest].

val([]) -> undefined;
val(I) -> z_convert:to_integer(I).
