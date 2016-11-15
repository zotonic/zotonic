%% @copyright 2015-2016 Marc Worrell
%% @doc Expansion of all user groups and content groups, used to fill acl lookup tables.

%% Copyright 2015-2016 Marc Worrell
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

-module(acl_user_groups_rules).

-export([
    expand/2,
    expand_rsc/2,
    expand_collab/2,
    test/0
]).

-export([
    tree_expand/1,
    tree_ids/1
    ]).

-include_lib("zotonic.hrl").

-type rsc_id() :: integer().

-type module_name() :: atom().
-type module_action() :: use.

-type only_if_owner() :: true | false.
-type allow() :: true | false.
-type user_group_id() :: rsc_id().
-type content_group_id() :: rsc_id().
-type category_id() :: rsc_id().
-type rsc_action() :: view | insert | delete | update | link.

-type module_rule() :: {module_name(), module_action(), user_group_id(), allow()}.
-type rsc_rule() :: {content_group_id(), {category_id(), rsc_action(), only_if_owner(), allow()}, user_group_id()}.

-type rule() :: rsc_rule() | module_rule().
-type user_group_path() :: {user_group_id(), list(user_group_id())}.

-spec expand(edit|publish, #context{}) -> {list(content_group_id()),
                                           list(user_group_id()),
                                           list(user_group_path()),
                                           list(rule())}.
expand(State, Context) ->
    GroupTree = m_hierarchy:menu(content_group, Context),
    UserTree = m_hierarchy:menu(acl_user_group, Context),
    RscRules = m_acl_rule:all_rules(rsc, State, Context),
    RuleGroups = rule_content_groups(RscRules),
    {lists:usort(tree_ids(GroupTree) ++ RuleGroups),
     tree_ids(UserTree),
     expand_group_path(UserTree),
     expand_module(State, UserTree, Context)
        ++ expand_collab(State, Context)
        ++ expand_rsc(State, RscRules, GroupTree, UserTree, Context)}.

rule_content_groups(Rules) ->
    lists:flatten([
        case proplists:get_value(content_group_id, R) of
            undefined -> [];
            Id -> Id
        end
        || R <- Rules
    ]).

-spec expand_rsc(edit|publish, #context{}) -> list(rule()).
expand_rsc(State, Context) ->
    GroupTree = m_hierarchy:menu(content_group, Context),
    UserTree = m_hierarchy:menu(acl_user_group, Context),
    expand_rsc(State, m_acl_rule:all_rules(rsc, State, Context), GroupTree, UserTree, Context).

-spec expand_module(edit|publish, list(), #context{}) -> list(module_rule()).
expand_module(State, UserTree, Context) ->
    Modules = z_module_manager:active(Context),
    Modules1 = [ {<<>>, Modules} | [ {z_convert:to_binary(M),[M]} || M <- Modules ] ],
    RuleRows = resort_deny_rules(m_acl_rule:all_rules(module, State, Context)),
    Rules = expand_rule_rows(module, Modules1, RuleRows, Context),
    [ {M,A,GId,IsAllow} || {x,{M,A,_IsOwner,IsAllow},GId} <- expand_rules([{x,[]}], Rules, UserTree, Context) ].

-spec expand_collab(edit|publish, #context{}) -> list(rsc_rule()).
expand_collab(State,Context) ->
    CategoryTree = m_category:menu(Context),
    RuleRows = resort_deny_rules(m_acl_rule:all_rules(collab, State, Context)),
    Cs = [{undefined, tree_ids(CategoryTree)} | tree_expand(CategoryTree) ],
    Rules = expand_rule_rows(category_id, Cs, RuleRows, Context),
    [ {collab, Action, collab} || {undefined, Action, undefined} <- Rules ].

-spec expand_rsc(edit|publish, list(), list(), list(), #context{}) -> list(rsc_rule()).
expand_rsc(_State, RscRules, GroupTree, UserTree, Context) ->
    CategoryTree = m_category:menu(Context),
    RuleRows = resort_deny_rules(RscRules),
    Cs = [{undefined, tree_ids(CategoryTree)} | tree_expand(CategoryTree) ],
    Rules = expand_rule_rows(category_id, Cs, RuleRows, Context),
    expand_rules(GroupTree, Rules, UserTree, Context).

expand_rule_rows(category_id, Cs, RuleRows, Context) ->
    NonMetaCs = remove_meta_category(Cs, Context),
    lists:flatten([ expand_rule_row(category_id, RuleRow, Cs, NonMetaCs, Context) || RuleRow <- RuleRows ]);
expand_rule_rows(Prop, Cs, RuleRows, Context) ->
    lists:flatten([ expand_rule_row(Prop, RuleRow, Cs, Cs, Context) || RuleRow <- RuleRows ]).

remove_meta_category(Cs, Context) ->
    case m_category:name_to_id(meta, Context) of
        {ok, MetaId} ->
            MetaIds = [ MetaId | proplists:get_value(MetaId, Cs, [])],
            Cs1 = lists:filter(fun({Id,_SubIds}) ->
                                    not lists:member(Id, MetaIds)
                               end,
                               Cs),
            [ {Id, SubIds -- MetaIds} || {Id, SubIds} <- Cs1 ];
        {error, _} ->
            Cs
    end.

expand_rule_row(Prop, Row, Cs, NonMetaCs, Context) ->
    Actions = [ Act || {Act,true} <- proplists:get_value(actions, Row, []) ],
    IsAllow = not proplists:get_value(is_block, Row),
    ContentGroupId = proplists:get_value(content_group_id, Row),
    UserGroupId = proplists:get_value(acl_user_group_id, Row),
    IsOwner = proplists:get_value(is_owner, Row, false),
    PropId = proplists:get_value(Prop, Row),
    ContentGroupName = m_rsc:p_no_acl(ContentGroupId, name, Context),
    CIdsEdit = maybe_filter_meta(ContentGroupName, Prop, PropId, Cs, NonMetaCs, Context),
    CIdsView = proplists:get_value(PropId, Cs, [PropId]),
    lists:flatten(
        [
            [
              {ContentGroupId, {CId, Action, IsOwner, IsAllow}, UserGroupId}
              || CId <- select_cids(Action, CIdsEdit, CIdsView)
            ]
            || Action <- Actions
        ]).

select_cids(view, _Edit, View) -> View;
select_cids(_Action, Edit, _View) -> Edit.

maybe_filter_meta(<<"system_content_group">>, category_id, PropId, Cs, _NonMetaCs, _Context) ->
    proplists:get_value(PropId, Cs, [PropId]);
maybe_filter_meta(_ContentGroupName, category_id, PropId, Cs, NonMetaCs, Context) ->
    case m_rsc:is_a(PropId, meta, Context) of
        true -> proplists:get_value(PropId, Cs, [PropId]);
        false -> proplists:get_value(PropId, NonMetaCs, [PropId])
    end;
maybe_filter_meta(_ContentGroupName, _Prop, PropId, Cs, _NonMetaCs, _Context) ->
    proplists:get_value(PropId, Cs, [PropId]).

%% @doc Given two id lists, return all possible combinations.
expand_rules(TreeA, Rules, TreeB, _Context) ->
    As = [{undefined, tree_ids(TreeA)} | tree_expand(TreeA) ],
    Bs = tree_expand(TreeB),
    lists:flatten(
        lists:map(fun({A, Pred, B}) ->
                        case {lists:keyfind(A, 1, As), lists:keyfind(B, 1, Bs)} of
                            {{A, A1}, {B, B1}} ->
                                expand_rule(A1, Pred, B1);
                            {false, {B, B1}} ->
                                % acl_collaboration_groups are not part of the group hierarchy
                                expand_rule([A], Pred, B1);
                            Other ->
                                lager:warning("Tree expand of {~p, ~p, ~p} returned ~p",
                                              [A, Pred, B, Other]),
                                []
                        end
                  end,
                  Rules)).

expand_rule(As, Pred, Bs) ->
    [ {A, Pred, B} || A <- As, B <- Bs ].


%% @doc Given a tree, return a list with (id, [id|contained_ids])
tree_expand(Tree) ->
    lists:flatten([ element(2,tree_expand(T, [])) || T <- Tree ]).

tree_expand({Id, []}, Acc) ->
    {[Id], [{Id,[Id]}|Acc]};
tree_expand({Id, Subs}, Acc) ->
    {BIds, Acc1} = lists:foldl(
                            fun(SId, {BIdsAcc, LookupAcc}) ->
                                {BelowIds, LookupAcc1} = tree_expand(SId, LookupAcc),
                                {[BelowIds,BIdsAcc], LookupAcc1}
                            end,
                            {[], Acc},
                            Subs),
    BIds1 = lists:flatten(BIds),
    {[Id|BIds1], [{Id,[Id|BIds1]} | Acc1]}.

tree_ids(Tree) ->
    tree_ids(Tree, []).

tree_ids([], Acc) ->
    Acc;
tree_ids([{Id,Sub}|Rest], Acc) ->
    Acc1 = tree_ids(Sub, Acc),
    tree_ids(Rest, [Id|Acc1]).

% Fetch the path to all ids in the tree
expand_group_path(Tree) ->
    expand_group_path(Tree, [], []).

expand_group_path([], _Path, Acc) ->
    Acc;
expand_group_path([{Id, Subs}|Rest], Path, Acc) ->
    Path1 = [Id|Path],
    Acc1 = [{Id, Path1}|Acc],
    Acc2 = expand_group_path(Subs, Path1, Acc1),
    expand_group_path(Rest, Path, Acc2).


% Bubble all deny rules after the allow rules for a specific user group
resort_deny_rules(Rs) ->
    resort_deny_rules(Rs, undefined, [], []).

resort_deny_rules([], _GroupId, DenyAcc, Acc) ->
    lists:reverse(DenyAcc ++ Acc);
resort_deny_rules([R|Rs], GroupId, DenyAcc, Acc) ->
    case proplists:get_value(acl_user_group_id, R) of
        GroupId ->
            case proplists:get_value(is_block, R) of
                true ->
                    resort_deny_rules(Rs, GroupId, [R|DenyAcc], Acc);
                false ->
                    resort_deny_rules(Rs, GroupId, DenyAcc, [R|Acc])
            end;
        NewGroupId ->
            Acc1 = DenyAcc ++ Acc,
            case proplists:get_value(is_block, R) of
                true ->
                    resort_deny_rules(Rs, NewGroupId, [R], Acc1);
                false ->
                    resort_deny_rules(Rs, NewGroupId, [], [R|Acc1])
            end
    end.

test() ->
    [] = tree_expand([]),
    [{1,[1]}, {2,[2]}] = tree_expand([{1,[]}, {2,[]}]),
    [{1,[1]},{2,[2,3]},{3,[3]}] = acl_user_groups_rules:tree_expand([{1,[]}, {2,[{3,[]}]}]),
    ok.
