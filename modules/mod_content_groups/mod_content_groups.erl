%% @copyright 2015 Marc Worrell
%% @doc Adds content groups to enable access-control rules on resources.

%% Copyright 2015 Marc Worrell
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

-module(mod_content_groups).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Content Groups").
-mod_description("Categorize content into a hierarchical structure of content groups.").
-mod_prio(400).
-mod_schema(1).
-mod_depends([menu, mod_mqtt]).
-mod_provides([]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
    event/2,
    observe_rsc_get/3,
    observe_rsc_delete/2,
    observe_pivot_related/3,
    observe_rsc_update_done/2,
    observe_admin_menu/3,
    manage_schema/2
    ]).

event(#submit{message={delete_move, Args}}, Context) ->
    ToGroupId = z_convert:to_integer(z_context:get_q_validated("content_group_id", Context)),
    {id, Id} = proplists:lookup(id, Args),
    Ids = [ Id | m_hierarchy:children('content_group', Id, Context) ],
    case deletable(Ids, Context) andalso z_acl:rsc_editable(ToGroupId, Context) of
        true ->
            Context1 = z_context:prune_for_async(Context),
            spawn(fun() ->
                    cg_move_and_delete(Ids, ToGroupId, Context1)
                  end),
            z_render:wire({dialog_close, []}, Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
    end;
event(#postback{message={delete_all, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    IfEmpty = proplists:get_value(if_empty, Args, false),
    Ids = [ Id | m_hierarchy:children('content_group', Id, Context) ],
    case not IfEmpty orelse not m_content_group:is_used(Id, Context) of
        true ->
            case deletable(Ids, Context)  of
                true ->
                    Context1 = z_context:prune_for_async(Context),
                    spawn(fun() ->
                            cg_delete(Ids, Context1)
                          end),
                    z_render:wire({dialog_close, []}, Context);
                false ->
                    z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
            end;
        false ->
            z_render:wire({alert, [{message, ?__("Delete is canceled, there are pages in this content group.", Context)}]}, Context)
    end.

cg_delete(Ids, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    RscIds = in_content_groups(Ids, Context),
    case delete_all(RscIds, 0, length(RscIds), Context) of
        ok ->
            lists:foreach(fun(Id) ->
                             m_rsc:delete(Id, Context)
                          end,
                          Ids),
            Context1 = z_render:wire({unmask, []}, Context),
            z_session_page:add_script(Context1);
        {error, _} ->
            Context1 = z_render:wire([
                    {unmask, []},
                    {alert, [{message, ?__("Not all resources could be deleted.", Context)}]}
                ],
                Context),
            z_session_page:add_script(Context1)

    end.

cg_move_and_delete(Ids, ToGroupId, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    RscIds = in_content_groups(Ids, Context),
    ok = move_all(RscIds, ToGroupId, 0, length(RscIds), Context),
    lists:foreach(fun(Id) ->
                     m_rsc:delete(Id, Context)
                  end,
                  Ids),
    Context1 = z_render:wire({unmask, []}, Context),
    z_session_page:add_script(Context1),
    ok.

in_content_groups(Ids, Context) ->
    In = lists:flatten(z_utils:combine($,, [ integer_to_list(NId) || NId <- Ids ])),
    z_db:q("select id from rsc where content_group_id in ("++In++")", [], Context, 60000).

delete_all([], _N, _Total, _Context) ->
    ok;
delete_all([{Id}|Ids], N, Total, Context) ->
    case catch m_rsc:delete(Id, Context) of
        ok ->
            maybe_progress(N, N+1, Total, Context),
            delete_all(Ids, N+1, Total, Context);
        Error ->
            {error, Error}
    end.

move_all([], _ToGroupId, _N, _Total, _Context) ->
    ok;
move_all([{Id}|Ids], ToGroupId, N, Total, Context) ->
    m_rsc_update:update(Id, [{content_group_id,ToGroupId}], z_acl:sudo(Context)),
    maybe_progress(N, N+1, Total, Context),
    move_all(Ids, ToGroupId, N+1, Total, Context).

maybe_progress(_N1, _N2, 0, _Context) ->
    ok;
maybe_progress(N1, N2, Total, Context) ->
    z_pivot_rsc:pivot_delay(Context),
    PerStep = Total / 100,
    S1 = round(N1 / PerStep),
    S2 = round(N2 / PerStep),
    case S1 of
        S2 -> ok;
        _ -> z_session_page:add_script(z_render:wire({mask_progress, [{percent,S2}]}, Context))
    end.

deletable(Ids, Context) ->
    lists:all(fun(Id) -> z_acl:rsc_deletable(Id, Context) end, Ids).

observe_rsc_get(#rsc_get{}, [], _Context) ->
    [];
observe_rsc_get(#rsc_get{}, Props, Context) ->
    case proplists:get_value(content_group_id, Props) of
        undefined ->
            [
                {content_group_id, 
                        case m_category:is_meta(proplists:get_value(category_id, Props), Context) of
                            true -> m_rsc:rid(system_content_group, Context);
                            false -> m_rsc:rid(default_content_group, Context)
                        end}
                | proplists:delete(content_group_id, Props)
            ];
        _ ->
            Props
    end.

%% @doc Do not allow a content group to be removed iff there are resources in that content group
observe_rsc_delete(#rsc_delete{id=Id, is_a=IsA}, Context) ->
    case lists:member(content_group, IsA) of
        true ->
            case m_content_group:is_used(Id, Context) of
                true -> throw({error, is_used});
                false -> ok
            end;
        false ->
            ok
    end.


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_content_groups,
                parent=admin_structure,
                label=?__("Content groups", Context),
                url={admin_menu_hierarchy, [{name, "content_group"}]},
                visiblecheck={acl, use, mod_admin_config}}
     |Acc].

observe_pivot_related(#pivot_related{id=Id}, Ids, Context) ->
    case m_rsc:p_no_acl(Id, content_group_id, Context) of
        undefined ->
            Ids;
        CId ->
            lists:foldl(
                fun(PId,Acc) ->
                    case lists:member(PId, Acc) of
                        true -> Acc;
                        false -> [PId|Acc]
                    end
                end,
                Ids,
                [CId | m_hierarchy:parents(content_group, CId, Context) ])
    end.

observe_rsc_update_done(#rsc_update_done{pre_is_a=PreIsA, post_is_a=PostIsA}, Context) ->
    case  lists:member('content_group', PreIsA) 
        orelse lists:member('content_group', PostIsA)
    of
        true -> m_hierarchy:ensure(content_group, Context);
        false -> ok
    end.

manage_schema(_Version, Context) ->
    z_datamodel:manage(
              ?MODULE,
              #datamodel{
                categories=[
                    {content_group, meta, [
                        {title, {trans, [{en, <<"Content Group">>}, {nl, <<"Paginagroep">>}]}}
                    ]}
                ],
                resources=[
                    {system_content_group, content_group, [
                        {title, {trans, [{en, <<"System Content">>}, {nl, <<"Systeempagina’s"/utf8>>}]}}
                    ]},
                    {default_content_group, content_group, [
                        {title, {trans, [{en, <<"Default Content Group">>}, {nl, <<"Standaard paginagroep">>}]}}
                    ]}
                ]
              },
              Context),
    m_hierarchy:ensure(content_group, Context),
    SysId = m_rsc:rid(system_content_group, Context),
    {MetaFrom, MetaTo} = m_category:get_range(meta, Context),
    z_db:q("
        update rsc
        set content_group_id = $1
        where pivot_category_nr >= $2
          and pivot_category_nr <= $3
          and content_group_id is null
        ",
        [SysId, MetaFrom, MetaTo],
        Context),
    ok.

