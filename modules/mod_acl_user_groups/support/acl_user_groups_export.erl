%% @copyright 2015 Marc Worrell
%% @doc Import/export of all ACL rules, including the group and user hierarchies

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


-module(acl_user_groups_export).

-export([
    export/1,
    import/2
]).


%% @doc Import an export
import({acl_export, 1, CGs, UGs, CGMenu, UGMenu, RscRules, ModRules}, Context) ->
    lager:notice("[~p] ACL import by ~p: starting",
                 [z_context:site(Context), z_acl:user(Context)]),
    import_all(content_group, CGs, [], Context),
    import_all(acl_user_group, UGs, [], Context),
    CGMenu1 = menu_from_names(CGMenu, Context),
    m_hierarchy:save(content_group, CGMenu1, Context),
    m_hierarchy:ensure(content_group, Context),
    UGMenu1 = menu_from_names(UGMenu, Context),
    m_hierarchy:save(acl_user_group, UGMenu1, Context),
    m_hierarchy:ensure(acl_user_group, Context),
    m_acl_rule:import_rules(rsc, edit, RscRules, Context),
    m_acl_rule:import_rules(module, edit, ModRules, Context),
    lager:notice("[~p] ACL import by ~p: done",
                 [z_context:site(Context), z_acl:user(Context)]),
    ok.



%% @doc Make an export structure of all data related to the access control
%% @todo Peform a dependency sort of all groups, so that content_group are inserted
%%       in the correct order (ie the content groups of the content groups first)
export(Context) ->
    lager:notice("[~p] ACL export by ~p",
                 [z_context:site(Context), z_acl:user(Context)]),
    ensure_name(content_group, Context),
    ensure_name(acl_user_group, Context),
    {acl_export, 1,
        fetch_all(content_group, Context),
        fetch_all(acl_user_group, Context),
        menu_to_names(m_hierarchy:menu(content_group, Context), Context),
        menu_to_names(m_hierarchy:menu(acl_user_group, Context), Context),
        m_acl_rule:ids_to_names(m_acl_rule:all_rules(rsc, edit, Context), Context),
        m_acl_rule:ids_to_names(m_acl_rule:all_rules(module, edit, Context), Context)}.


% @doc Fetch all resources within the given category
fetch_all(Category, Context) ->
    m_category:fold(Category,
                    fun(Id, Acc, Ctx) ->
                        Ps = m_rsc:get(Id, Ctx),
                        Ps1 = cleanup_rsc(Ps),
                        Rsc = {rsc,
                               m_rsc:is_a(Id, Ctx), 
                               m_rsc:p_no_acl(proplists:get_value(content_group_id, Ps), name, Ctx),
                               Ps1},
                        [Rsc | Acc]
                    end,
                    [],
                    Context).

cleanup_rsc(Ps) ->
    lists:foldl(
        fun(P,Acc) ->
            proplists:delete(P, Acc)
        end,
        Ps,
        [
            id, version,
            category_id, content_group_id,
            creator_id, modifier_id,
            created, modified,
            page_path,
            pivot_geocode,
            installed_by
        ]).

%% @doc Ensure that all resources of this category are present in the database
import_all(_Cat, [], _IdsAcc, _Context) ->
    ok;
import_all(Cat, [Rsc|Rest], IdsAcc, Context) ->
    IdsAcc1 = import_1(Cat, Rsc, IdsAcc, Context),
    import_all(Cat, Rest, IdsAcc1, Context).

import_1(Cat, {rsc, IsA, CGName, Ps0}, IdsAcc, Context) ->
    Name = proplists:get_value(name, Ps0),
    Ps = cleanup_rsc(Ps0),
    Cat1 = select_cat(Cat, lists:reverse(IsA), Context),
    Props = [
        {category_id, Cat1}
        | Ps
    ],
    case m_rsc:rid(Name, Context) of
        undefined ->
            lager:info("[~p] ACL export, inserting ~p with name ~p",
                       [z_context:site(Context), Cat1, Name]),
            case Name of
                CGName ->
                    {ok, Id} = m_rsc:insert(Props, Context),
                    m_rsc:update(Id, [{content_group_id, Id}], Context),
                    [{Name,Id}|IdsAcc];
                _Other ->
                    {CGId, IdsAcc1} = ensure_content_group(CGName, IdsAcc, Context),
                    Props1 = [
                        {content_group_id, CGId} | Props
                    ],
                    {ok, Id} = m_rsc:insert(Props1, Context),
                    [{Name, Id}|IdsAcc1]
            end;
        _Id ->
            case proplists:get_value(Name, IdsAcc) of
                undefined ->
                    IdsAcc;
                Id ->
                    {CGId, IdsAcc1} = ensure_content_group(CGName, IdsAcc, Context),
                    Props1 = [
                        {content_group_id, CGId} | Props
                    ],
                    case z_acl:rsc_editable(Id, Context) of
                        true ->
                            lager:info("[~p] ACL export, updating ~p with name ~p",
                                       [z_context:site(Context), Cat1, Name]),
                            {ok, Id} = m_rsc:update(Id, Props1, Context);
                        false ->
                            ok
                    end,
                    [{Name, Id}|IdsAcc1]
            end
    end.

select_cat(Cat, [], Context) ->
    {ok, Id} = m_category:name_to_id(Cat, Context),
    Id;
select_cat(Cat, [C|Cs], Context) ->
    case m_category:name_to_id(C, Context) of
        {ok, Id} -> Id;
        {error, _} -> select_cat(Cat, Cs, Context)
    end.

ensure_content_group(CGName, IdsAcc, Context) ->
    case proplists:get_value(CGName, IdsAcc) of
        undefined ->
            case m_rsc:rid(CGName, Context) of
                undefined ->
                    Props = [
                        {category, content_group},
                        {title, CGName},
                        {name, CGName}
                    ],
                    lager:info("[~p] ACL export, inserting content_group with name ~p",
                               [z_context:site(Context), CGName]),
                    {ok, Id} = m_rsc:insert(Props, Context),
                    {Id, [{CGName,Id}|IdsAcc]};
                Id ->
                    {Id, IdsAcc}
            end;
        Id ->
            {Id, IdsAcc}
    end.


menu_to_names(Menu, Context) ->
    lists:reverse(menu_to_names(Menu, [], Context)).

menu_to_names([], Acc, _Context) ->
    lists:reverse(Acc);
menu_to_names([{Id,Sub}|Rest], Acc, Context) ->
    Acc1 = [{m_rsc:p_no_acl(Id, name, Context), menu_to_names(Sub, Context)} | Acc],
    menu_to_names(Rest, Acc1, Context).

menu_from_names(Menu, Context) ->
    lists:reverse(menu_from_names(Menu, [], Context)).

menu_from_names([], Acc, _Context) ->
    lists:reverse(Acc);
menu_from_names([{Name,Sub}|Rest], Acc, Context) ->
    Acc1 = [{m_rsc:rid(Name, Context), menu_from_names(Sub, Context)} | Acc],
    menu_from_names(Rest, Acc1, Context).



%% @doc Ensure that all things of a category have an unique name.
ensure_name(Cat, Context) ->
    m_category:foreach(Cat, 
                       fun(Id, Ctx) ->
                            m_rsc:ensure_name(Id, Ctx)
                       end,
                       Context).
