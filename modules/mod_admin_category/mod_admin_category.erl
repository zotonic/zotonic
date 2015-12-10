%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2015 Marc Worrell
%% @doc Module for editing and managing categories.

%% Copyright 2009-2015 Marc Worrell
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

-module(mod_admin_category).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin category support").
-mod_description("Support editing and changing the category hierarchy.").
-mod_prio(600).
-mod_depends([admin, menu]).
-mod_provides([]).

-export([
    event/2,
    observe_category_hierarchy_save/2,
    observe_admin_menu/3,
    observe_rsc_delete/2
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


event(#submit{message={delete_move, Args}}, Context) ->
    ToCatId = z_convert:to_integer(z_context:get_q_validated("category_id", Context)),
    {id, Id} = proplists:lookup(id, Args),
    Ids = [ Id | m_hierarchy:children('$category', Id, Context) ],
    case deletable(Ids, Context) andalso z_acl:rsc_editable(ToCatId, Context) of
        true ->
            Context1 = z_context:prune_for_async(Context),
            spawn(fun() ->
                    cat_move_and_delete(Ids, ToCatId, Context1)
                  end),
            z_render:wire({dialog_close, []}, Context);
        false ->
            z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
    end;
event(#postback{message={delete_all, Args}}, Context) ->
    {id, Id} = proplists:lookup(id, Args),
    IfEmpty = proplists:get_value(if_empty, Args, false),
    Ids = [ Id | m_hierarchy:children('$category', Id, Context) ],
    case not IfEmpty orelse not m_category:is_used(Id, Context) of
        true ->
            case deletable(Ids, Context)  of
                true ->
                    Context1 = z_context:prune_for_async(Context),
                    spawn(fun() ->
                            cat_delete(Ids, Context1)
                          end),
                    z_render:wire({dialog_close, []}, Context);
                false ->
                    z_render:growl(?__("Sorry, you are not allowed to delete this.", Context), Context)
            end;
        false ->
            z_render:wire({alert, [{message, ?__("Delete is canceled, there are pages in the category.", Context)}]}, Context)
    end.

cat_delete(Ids, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    RscIds = in_categories(Ids, Context),
    case delete_all(RscIds, 0, length(RscIds), Context) of
        ok ->
            lists:foreach(fun(Id) ->
                             m_rsc:delete(Id, Context)
                          end,
                          Ids),
            z_session_page:add_script(z_render:wire({unmask, []}, Context));
        {error, _} ->
            Context1 = z_render:wire([
                    {unmask, []},
                    {alert, [{message, ?__("Not all resources could be deleted.", Context)}]}
                ],
                Context),
            z_session_page:add_script(Context1)

    end.

cat_move_and_delete(Ids, ToGroupId, Context) ->
    z_session_page:add_script(z_render:wire({mask, [{message, ?__("Deleting...", Context)}]}, Context)),
    RscIds = in_categories(Ids, Context),
    ok = move_all(RscIds, ToGroupId, 0, length(RscIds), Context),
    lists:foreach(fun(Id) ->
                     m_rsc:delete(Id, Context)
                  end,
                  Ids),
    z_session_page:add_script(z_render:wire({unmask, []}, Context)),
    ok.

in_categories(Ids, Context) ->
    In = lists:flatten(z_utils:combine($,, [ integer_to_list(NId) || NId <- Ids ])),
    z_db:q("select id from rsc where category_id in ("++In++")", [], Context, 60000).

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

move_all([], _ToCatId, _N, _Total, _Context) ->
    ok;
move_all([{Id}|Ids], ToCatId, N, Total, Context) ->
    m_rsc_update:update(Id, [{category_id, ToCatId}], z_acl:sudo(Context)),
    maybe_progress(N, N+1, Total, Context),
    move_all(Ids, ToCatId, N+1, Total, Context).

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


observe_category_hierarchy_save(#category_hierarchy_save{tree=New}, Context) ->
    case z_acl:is_allowed(insert, category, Context) of
        true ->
            case m_hierarchy:menu('$category', Context) of
                New -> 
                    ok;
                Old ->
                    % Check if any ids are added or deleted
                    NewIds = lists:sort(ids(New, [])),
                    OldIds = lists:sort(ids(Old, [])),
                    Deleted = OldIds -- NewIds,
                    % Inserted = NewIds -- OldIds,

                    % Delete all ids not in the new category tree
                    lists:map(fun(Id) ->
                                  ok = m_category:delete(Id, undefined, Context)
                              end,
                              Deleted),

                    _ = m_hierarchy:save_nocheck('$category', New, Context),
                    m_category:flush(Context),
                    ok
            end;
        false ->
            undefined
    end.

%% @doc Do not allow a category to be removed iff there are resources within that category
observe_rsc_delete(#rsc_delete{id=Id, is_a=IsA}, Context) ->
    case lists:member(category, IsA) of
        true ->
            case m_category:is_used(Id, Context) of
                true -> throw({error, is_used});
                false -> ok
            end;
        false ->
            ok
    end.

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_categories,
                parent=admin_structure,
                label=?__("Categories", Context),
                url={admin_category_sorter},
                visiblecheck={acl, insert, category}}
     
     |Acc].


ids([], Acc) ->
    Acc;
ids([{Id,Sub}|Rest], Acc) ->
    Acc1 = ids(Sub, Acc),
    ids(Rest, [Id|Acc1]).

