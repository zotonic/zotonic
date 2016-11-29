%% @copyright 2015-2016 Marc Worrell
%% @doc Routines for ACL notifications.

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

%% @todo: add restriction for acl_rsc_update_check
%% @todo: check if we can know the content_group_id on the category insert check

-module(acl_user_groups_checks).

-export([
        state/1,

        max_upload_size/1,
        max_upload_size_default/0,

        user_groups/1,
        user_groups_all/1,

        has_collab_groups/1,

        acl_is_allowed/2,
        acl_logon/2,
        acl_logoff/2,
        acl_context_authenticated/1,
        acl_add_sql_check/2
    ]).

-export([
        can_insert_category/2,
        can_insert_category/3,
        can_insert_category_collab/2,
        can_update_category/3,
        can_rsc_insert/3,
        can_move/3,
        default_content_group/2

    ,can_rsc/3
    ]).

-include_lib("zotonic.hrl").

-define(MAX_UPLOAD_SIZE_MB, 50).

-record(aclug, {
        user_groups = [],
        collab_groups = [],
        state = publish :: publish | edit
    }).

state(#context{acl=#aclug{state=State}}) ->
    State;
state(#context{}) ->
    publish.

max_upload_size(Context) ->
    Ids = user_groups_all(Context),
    Sizes = [ m_rsc:p_no_acl(Id, acl_upload_size, Context) || Id <- Ids ],
    Sizes1 = [ Sz || Sz <- Sizes, is_integer(Sz), Sz > 0 ],
    Sizes2 = [ z_convert:to_integer(Sz) || Sz <- Sizes1 ],
    case Sizes2 of
        [] -> max_upload_size_default();
        _ -> lists:max(Sizes2) * 1024 * 1024
    end.

max_upload_size_default() ->
    ?MAX_UPLOAD_SIZE_MB * 1024 * 1024.


user_groups(#context{acl=#aclug{user_groups=Ids}}) ->
    Ids;
user_groups(#context{user_id=UserId, acl=admin} = Context) ->
    MgrId = m_rsc:rid(acl_user_group_managers, Context),
    Groups = m_edge:objects(UserId, hasusergroup, Context),
    case {MgrId, lists:member(MgrId, Groups)} of
        {undefined, _} -> Groups;
        {_, true} -> Groups;
        {_, false} -> Groups ++ [MgrId]
    end;
user_groups(#context{user_id=UserId} = Context) ->
    has_user_groups(UserId, Context).

user_groups_all(Context) ->
    user_groups_expand(user_groups(Context), Context).

user_groups_expand(Ids, Context) ->
    Ids2 = [ mod_acl_user_groups:lookup(Id, Context) || Id <- Ids ],
    Ids3 = [ Xs || Xs <- Ids2, Xs =/= undefined ],
    lists:usort(lists:flatten(Ids3)).

%% @doc API to check if a category can be updated in a content-group
can_update_category(_, _, #context{acl=admin}) ->
    true;
can_update_category(_, _, #context{user_id=1}) ->
    true;
can_update_category(CGId, CatId, Context) ->
    CGId1 = m_rsc:rid(CGId, Context),
    CatId1 = m_rsc:rid(CatId, Context),
    UGs = user_groups(Context),
    can_rsc_ug(CGId1, CatId1, update, false, UGs, Context).

%% @doc API to check if a category can be inserted into any content-group
can_insert_category(_, #context{acl=admin}) ->
    true;
can_insert_category(_, #context{user_id=1}) ->
    true;
can_insert_category(Cat, Context) ->
    can_insert(Cat, Context).

%% @doc API to check if a category can be inserted into a content-group
can_insert_category(_, _, #context{acl=admin}) ->
    true;
can_insert_category(_, _, #context{user_id=1}) ->
    true;
can_insert_category(CGId, CatId, Context) ->
    CGId1 = m_rsc:rid(CGId, Context),
    CatId1 = m_rsc:rid(CatId, Context),
    case is_collab_group_member(CGId1, Context) of
        true ->
            case mod_acl_user_groups:await_lookup({collab, {CatId1, insert, false}, collab}, Context) of
                true -> true;
                false -> false;
                undefined -> false
            end
            orelse can_insert_category_rsc_ug(CGId1, CatId1, true, Context);
        false ->
            can_insert_category_rsc_ug(CGId1, CatId1, false, Context)
    end.

% Check for insert permission on the content-group, or on collab groups iff the content group is a
% collaboration group.
can_insert_category_rsc_ug(CGId1, CatId1, IsCollabGroup, Context) ->
    UGs = user_groups(Context),
    can_rsc_ug(CGId1, CatId1, insert, false, UGs, Context)
    orelse (
        (IsCollabGroup orelse m_rsc:is_a(CGId1, acl_collaboration_group, Context))
        andalso can_rsc_ug(m_rsc:rid(acl_collaboration_group, Context), CatId1, insert, false, UGs, Context)
    ).

%% @doc API to check if a category can be inserted into (any) collaboration-group
can_insert_category_collab(_, #context{acl=admin}) ->
    true;
can_insert_category_collab(_, #context{user_id=1}) ->
    true;
can_insert_category_collab(Cat, Context) ->
    CatId = m_rsc:rid(Cat, Context),
    case has_collab_groups(Context) of
        [] ->
            false;
        _Groups ->
            case mod_acl_user_groups:await_lookup({collab, {CatId, insert, false}, collab}, Context) of
                true -> true;
                false -> false;
                undefined -> false
            end
    end.


can_rsc_insert(_, _, #context{acl=admin}) ->
    true;
can_rsc_insert(_, _, #context{user_id=1}) ->
    true;
can_rsc_insert(CGId, RscId, Context) ->
    CatId = m_rsc:p_no_acl(RscId, category_id, Context),
    can_insert_category(CGId, CatId, Context).

%% @doc API to check if a category can be inserted into a content-group
can_move(_, _, #context{acl=admin}) ->
    true;
can_move(_, _, #context{user_id=1}) ->
    true;
can_move(CGId, RscId, Context) ->
    can_rsc(RscId, delete, Context)
    andalso can_rsc_insert(CGId, RscId, Context).



acl_is_allowed(_, #context{acl=admin}) ->
    true;
acl_is_allowed(_, #context{user_id=1}) ->
    true;
acl_is_allowed(#acl_is_allowed{object=undefined}, _Context) ->
    undefined;
acl_is_allowed(#acl_is_allowed{action=view, object=Id}, Context) when is_integer(Id) orelse is_atom(Id) ->
    can_rsc(Id, view, Context);
acl_is_allowed(#acl_is_allowed{action=insert, object=#acl_media{mime=Mime, size=Size}}, Context) ->
    can_media(Mime, Size, Context);
acl_is_allowed(#acl_is_allowed{action=insert, object=#acl_rsc{category=Cat}}, Context) ->
    can_insert(Cat, Context);
acl_is_allowed(#acl_is_allowed{action=insert, object=Cat}, Context) when is_atom(Cat) ->
    can_insert(Cat, Context);
acl_is_allowed(#acl_is_allowed{action=update, object=Id}, Context) ->
    can_rsc(Id, update, Context);
acl_is_allowed(#acl_is_allowed{object=#acl_edge{} = Edge}, Context) ->
    can_edge(Edge, Context);
acl_is_allowed(#acl_is_allowed{action=delete, object=Id}, Context) ->
    can_rsc(Id, delete, Context);
acl_is_allowed(#acl_is_allowed{action=Action, object=ModuleName}, Context) when is_atom(Action), is_atom(ModuleName) ->
    can_module(Action, ModuleName, Context);
acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.


acl_logon(#acl_logon{id=UserId}, Context) ->
    Context#context{
            acl=#aclug{
                user_groups = has_user_groups(UserId, Context),
                collab_groups = has_collab_groups(UserId, Context),
                state = session_state(Context)
            },
            user_id=UserId}.

acl_logoff(#acl_logoff{}, Context) ->
     Context#context{
            acl=#aclug{
                user_groups = has_user_groups(undefined, Context),
                collab_groups = [],
                state = session_state(Context)
            },
            user_id=undefined}.

%% @doc Set an anonymous context to the context of a 'typical' member.
acl_context_authenticated(#context{user_id=undefined} = Context) ->
    UserGroups = [ m_rsc:rid(acl_user_group_members, Context) ],
    Context#context{
        acl=#aclug{user_groups=UserGroups, state=session_state(Context)},
        user_id=authenticated
    };
acl_context_authenticated(#context{} = Context) ->
    Context.

default_content_group(CatId, Context) ->
    case m_category:is_a(CatId, meta, Context) of
        true ->
            m_rsc:rid(system_content_group, Context);
        false ->
            % Find the first content group the user can insert this.
            Default = m_rsc:rid(default_content_group, Context),
            UGs = user_groups(Context),
            case can_rsc_1(insert_rsc, insert, Default, CatId, UGs, Context) of
                true ->
                    Default;
                false ->
                    case find_first_content_group(CatId, UGs, Context) of
                        undefined ->
                            case find_first_collab_group(CatId, Context) of
                                undefined ->
                                    Default;
                                CGId -> CGId
                            end;
                        CGId -> CGId
                    end
            end
    end.

find_first_content_group(CatId, UGs, Context) ->
    Tree = m_hierarchy:menu('content_group', Context),
    find_cg(Tree, CatId, UGs, Context).

find_cg([], _CatId, _UGs, _Context) ->
    undefined;
find_cg([{CGId, Sub}|Rest], CatId, UGs, Context) ->
    case can_rsc_1(insert_rsc, insert, CGId, CatId, UGs, Context) of
        true ->
            CGId;
        false ->
            case find_cg(Sub, CatId, UGs, Context) of
                undefined ->
                    find_cg(Rest, CatId, UGs, Context);
                Found ->
                    Found
            end
    end.

%% @doc Get first collaboration group where the user may insert in; otherwise return undefined.
find_first_collab_group(CatId, Context) ->
    CanInsert = lists:filter(
                  fun(CGId) ->
                          can_insert_category(CGId, CatId, Context)
                  end, has_collab_groups(Context)),
    case CanInsert of
        [] -> undefined;
        [First|_] -> First
    end.


%% @doc Add a restriction on the visible content groups to SQL searches
acl_add_sql_check(#acl_add_sql_check{alias=Alias, args=Args, search_sql=SearchSql}, Context) ->
    case lists:member(no_content_group_check, SearchSql#search_sql.extra) of
        true ->
            {publish_check("", Alias, SearchSql, Context), Args};
        false ->
            case restrict_content_groups(Context) of
                all ->
                    {[], Args};
                all_published ->
                    {publish_check("", Alias, SearchSql, Context), Args};
                [] ->
                    % User is not allowed to see anything
                    Clause = " false ",
                    {Clause, Args};
                Ids when is_list(Ids) ->
                    % User can see some content groups
                    Clause = lists:flatten([
                                " (",Alias,".content_group_id is null or ",
                                     Alias,".content_group_id in (SELECT(unnest($",integer_to_list(length(Args)+1),"::int[]))))",
                                publish_check(" and ", Alias, SearchSql, Context)]),
                    {Clause, Args ++ [Ids]}
            end
    end.

publish_check(MaybeAnd, Alias, #search_sql{extra=Extra}, Context) ->
    case lists:member(no_publish_check, Extra) orelse z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            "";
        false ->
            [MaybeAnd,
             Alias,".is_published and ",
             Alias,".publication_start <= now() and ",
             Alias,".publication_end >= now()"]
    end.

restrict_content_groups(#context{user_id=1}) ->
    all;
restrict_content_groups(#context{acl=admin}) ->
    all;
restrict_content_groups(Context) ->
    % If user can see all collaboration groups, then give up on restricting (too many groups)
    % TODO: might want to do a negative restriction for hidden content-groups
    UGs = user_groups(Context),
    CGs = lists:flatten([ mod_acl_user_groups:await_lookup({view,UId}, Context) || UId <- UGs ]),
    CollabId = m_rsc:rid(acl_collaboration_group, Context),
    case lists:member(CollabId, CGs) of
        true -> all_published;
        false -> lists:usort(CGs) ++ has_collab_groups(Context)
    end.

session_state(Context) ->
    case z_context:get_session(acl_user_groups_state, Context) of
        undefined -> publish;
        publish -> publish;
        edit -> edit
    end.


% Fetch all usergroups the user is member of.
% Anonymous user are member of `acl_user_group_anonymous`.
% Users are member of `acl_user_group_members`, unless they have hasusergroup connections to other groups.
-spec has_user_groups(integer()|undefined, #context{}) -> list(integer()).
has_user_groups(undefined, Context) ->
    [ m_rsc:rid(acl_user_group_anonymous, Context) ];
has_user_groups(1, Context) ->
    MgrId = m_rsc:rid(acl_user_group_managers, Context),
    Groups = m_edge:objects(1, hasusergroup, Context),
    case {MgrId, lists:member(MgrId, Groups)} of
        {undefined, _} -> Groups;
        {_, true} -> Groups;
        {_, false} -> Groups ++ [MgrId]
    end;
has_user_groups(UserId, Context) ->
    case m_edge:objects(UserId, hasusergroup, Context) of
        [] ->
            [ m_rsc:rid(acl_user_group_members, Context) ];
        Us ->
            Us
    end.

% Fetch all collaboration groups the current is member of.
-spec has_collab_groups(#context{}) -> list(integer()).
has_collab_groups(#context{acl=#aclug{collab_groups=Ids}}) ->
    Ids;
has_collab_groups(_Context) ->
    [].

% Fetch all collaboration groups the user is member of.
-spec has_collab_groups(integer()|undefined, #context{}) -> list(integer()).
has_collab_groups(undefined, _Context) ->
    [];
has_collab_groups(UserId, Context) ->
    m_edge:subjects(UserId, hascollabmanager, Context)
    ++ m_edge:subjects(UserId, hascollabmember, Context).

%% @doc Check if the user is manager of a collaboration group.
is_collab_group_manager(_GroupId, #context{acl=#aclug{collab_groups=[]}}) ->
    false;
is_collab_group_manager(GroupId, #context{user_id=UserId, acl=#aclug{collab_groups=CollabGroups}} = Context) ->
    lists:member(GroupId, CollabGroups)
    andalso lists:member(GroupId, m_edge:subjects(UserId, hascollabmanager, Context)).

%% @doc Check if the user is a member of the collaboration group
is_collab_group_member(CGId, #context{acl=#aclug{collab_groups=CollabGroups}}) ->
    lists:member(CGId, CollabGroups);
is_collab_group_member(_CGId, _Context) ->
    false.

%% @doc Check if the user can insert the category in some content group
can_insert(_Cat, #context{acl=admin}) ->
    true;
can_insert(_Cat, #context{user_id=1}) ->
    true;
can_insert(Cat, #context{acl=#aclug{collab_groups=[]}} = Context) ->
    can_insert_with_ug(Cat, Context);
can_insert(Cat, Context) ->
    CatId = m_rsc:rid(Cat, Context),
    case mod_acl_user_groups:await_lookup({CatId, insert, collab}, Context) of
        true -> true;
        false -> false;
        undefined -> false
    end
    orelse can_insert_with_ug(CatId, Context).

can_insert_with_ug(Cat, Context) ->
    CatId = m_rsc:rid(Cat, Context),
    lists:any(fun(GId) ->
                 case mod_acl_user_groups:await_lookup({CatId, insert, GId}, Context) of
                    true -> true;
                    false -> false;
                    undefined -> false
                 end
              end,
              user_groups(Context)).

%% @doc Check if the user can view/update/delete/link the resource
can_rsc(undefined, _Action, _Context) ->
    false;
can_rsc(Id, view, Context) when is_integer(Id) ->
    CatId = m_rsc:p_no_acl(Id, category_id, Context),
    CGId = m_rsc:p_no_acl(Id, content_group_id, Context),
    UGs = user_groups(Context),
    (
        can_rsc_1(Id, view, CGId, CatId, UGs, Context)
        andalso (
            is_collab_group_member(CGId, Context)
            orelse m_rsc:p_no_acl(Id, is_published_date, Context)
        )
    )
    orelse can_rsc_1(Id, update, CGId, CatId, UGs, Context);
can_rsc(Id, Action, Context) when is_integer(Id); Id =:= insert_rsc ->
    CatId = m_rsc:p_no_acl(Id, category_id, Context),
    CGId = m_rsc:p_no_acl(Id, content_group_id, Context),
    UGs = user_groups(Context),
    can_rsc_1(Id, Action, CGId, CatId, UGs, Context);
can_rsc(Id, Action, Context) ->
    can_rsc(m_rsc:rid(Id, Context), Action, Context).

can_rsc_1(Id, Action, CGId, CatId, UGs, #context{acl=#aclug{collab_groups=CollabGroups}} = Context) ->
    (
        lists:member(CGId, CollabGroups)
        andalso can_rsc_collab_member(Id, Action, CGId, CatId, Context)
    )
    orelse (
        lists:member(Id, CollabGroups)
        andalso is_collab_group_member_action_allowed(Id, Action, Context)
    )
    orelse can_rsc_non_collab_rules(Id, Action, CGId, CatId, UGs, Context)
    orelse can_rsc_for_collab(Id, Action, CGId, CatId, UGs, Context);
can_rsc_1(Id, Action, CGId, CatId, UGs, Context) ->
    can_rsc_non_collab_rules(Id, Action, CGId, CatId, UGs, Context)
    orelse can_rsc_for_collab(Id, Action, CGId, CatId, UGs, Context).


% Check collaboration rules if the content group is a collaboration group
can_rsc_for_collab(Id, Action, CGId, CatId, UGs, Context) ->
    m_rsc:is_a(CGId, acl_collaboration_group, Context)
    andalso (
        can_rsc_for_all_collab(Id, Action, CatId, UGs, Context)
        orelse can_rsc_collab_group_content(Action, CGId, UGs, Context)
    ).


% User is member of collab group CGId, check ACL rules for collaboration groups
can_rsc_collab_member(Id, Action, CGId, CatId, Context) ->
    case mod_acl_user_groups:await_lookup({collab, {CatId, Action, false}, collab}, Context) of
        true -> true;
        false -> false;
        undefined -> false
    end
    orelse (
        case mod_acl_user_groups:await_lookup({collab, {CatId, Action, true}, collab}, Context) of
            true -> true;
            false -> false;
            undefined -> false
        end
        andalso (
            is_owner(Id, Context)
            orelse is_collab_group_manager(CGId, Context)
        )
    )
    orelse (
        % Members can see each other
        Action =:= view
        andalso m_rsc:is_a(Id, person, Context)
        andalso lists:member(CGId, has_collab_groups(Id, Context))
    ).


% User is member of collab group: check configured permissions for the collab group resource itself.
is_collab_group_member_action_allowed(_CGId, view, _Context) ->
    true;
is_collab_group_member_action_allowed(CGId, update, Context) ->
    case m_config:get_value(mod_acl_user_groups, collab_group_update, Context) of
        <<"member">> -> true;
        <<"manager">> -> is_collab_group_manager(CGId, Context);
        _ -> false
    end;
is_collab_group_member_action_allowed(CGId, link, Context) ->
    case m_config:get_value(mod_acl_user_groups, collab_group_link, Context) of
        <<"member">> -> true;
        <<"manager">> -> is_collab_group_manager(CGId, Context);
        _ -> false
    end;
is_collab_group_member_action_allowed(_CGId, _Action, _Context) ->
    false.


% If the content-group is a collab group then check if the user has permission to perform
% the action on all collaboration groups.
can_rsc_for_all_collab(Id, Action, CatId, UGs, Context) ->
    CollabId = m_rsc:rid(acl_collaboration_group, Context),
    can_rsc_non_collab_rules(Id, Action, CollabId, CatId, UGs, Context).

% If the user can update/link/delete a collaboration group then the user is
% considered to be a manager of the content group and can update/link/delete
% all content in the collaboration group.
can_rsc_collab_group_content(view, _CGId, _UGs, _Context) ->
    false;
can_rsc_collab_group_content(Action, CGId, UGs, Context) ->
    % Do not consider 'is_owner' exceptions here.
    CatId = m_rsc:p_no_acl(CGId, category_id, Context),
    can_rsc_ug(CGId, CatId, Action, false, UGs, Context).

% Perform a check using the normal ACL rules
can_rsc_non_collab_rules(Id, Action, CGId, CatId, UGs, Context) ->
    can_rsc_ug(CGId, CatId, Action, false, UGs, Context)
    orelse (
        is_owner(Id, Context)
        andalso can_rsc_ug(CGId, CatId, Action, true, UGs, Context)
    ).

% Check against the ACL rules for user-groups and content-groups
can_rsc_ug(CGId, CatId, Action, IsOwner, UGs, Context) ->
    lists:any(fun(GId) ->
                 case mod_acl_user_groups:await_lookup({CGId, {CatId, Action, IsOwner}, GId}, Context) of
                    true -> true;
                    false -> false;
                    undefined -> false
                 end
              end,
              UGs).


is_owner(insert_rsc, _Context) ->
    true;
is_owner(Id, #context{user_id=UserId} = Context) ->
    case z_notifier:first(#acl_is_owner{
            id=Id,
            creator_id=m_rsc:p_no_acl(Id, creator_id, Context),
            user_id=UserId
        },
        Context)
    of
        undefined -> false;
        true -> true;
        false -> false
    end.

%% @doc Check if an edge can be made, special checks for the hasusergroup edge
can_edge(#acl_edge{predicate=hasusergroup, subject_id=MemberId, object_id=UserGroupId}, Context) ->
    m_rsc:is_a(UserGroupId, acl_user_group, Context)
    andalso can_rsc(UserGroupId, link, Context)
    andalso can_rsc(MemberId, view, Context)
    andalso can_module(use, mod_acl_user_groups, Context);
can_edge(#acl_edge{predicate=hascollabmanager, subject_id=CollabGroupId, object_id=UserId}, Context) ->
    m_rsc:is_a(CollabGroupId, acl_collaboration_group, Context)
    andalso can_rsc(CollabGroupId, update, Context)
    andalso can_rsc(UserId, view, Context);
can_edge(#acl_edge{predicate=hascollabmember, subject_id=CollabGroupId, object_id=UserId}, Context) ->
    m_rsc:is_a(CollabGroupId, acl_collaboration_group, Context)
    andalso can_rsc(CollabGroupId, update, Context)
    andalso can_rsc(UserId, view, Context);
can_edge(#acl_edge{predicate=P, subject_id=SubjectId, object_id=ObjectId}, Context) when is_atom(P) ->
    can_rsc(SubjectId, link, Context)
    andalso can_rsc(ObjectId, view, Context).

%% @doc Check if the user can upload a file of the given mime type and size
can_media(Mime, undefined, Context) ->
    can_media(Mime, 0, Context);
can_media(Mime, Size, Context) ->
    MaxSize = max_upload_size(Context) * 1024 * 1024,
    case MaxSize >= Size of
        true ->
            Cat = m_media:mime_to_category(Mime),
            can_insert_category(Cat, Context);
        false ->
            false
    end.

%% @doc See if the user can do something with the given module
can_module(Action, ModuleName, Context) ->
    lists:any(fun(GId) ->
                 case mod_acl_user_groups:await_lookup({ModuleName, Action, GId}, Context) of
                    true -> true;
                    false -> false;
                    undefined -> false
                 end
              end,
              user_groups(Context)).
