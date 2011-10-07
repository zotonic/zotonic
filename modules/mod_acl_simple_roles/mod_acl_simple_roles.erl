
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-05
%% @doc Simple ACL module based on roles.

%% Copyright 2010 Marc Worrell
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

-module(mod_acl_simple_roles).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("ACL Simple Roles").
-mod_description("Simple role based access control.  Use this for a site with different editor roles.").
-mod_prio(500).

%% interface functions
-export([
    observe_acl_is_allowed/2,
    observe_acl_can_see/2,
    observe_acl_logon/2,
    observe_acl_logoff/2,
    observe_acl_rsc_update_check/3,
    observe_rsc_update/3,
    datamodel/0
]).

-include("zotonic.hrl").

-record(acl_user, {modules, categories, roles, view_all, only_update_own, file_mimes, file_size, visible_for}).

-define(ROLE_MEMBER, role_member).


%% @doc Check if the user is allowed to perform Action on Object
%% @todo #acl_edge
observe_acl_is_allowed(#acl_is_allowed{action=view, object=Id}, #context{user_id=undefined} = Context) when is_integer(Id) ->
    is_view_public(Id, Context);
observe_acl_is_allowed(#acl_is_allowed{}, #context{user_id=undefined}) ->
	undefined;
% Logged on users
observe_acl_is_allowed(#acl_is_allowed{action=view, object=Id}, Context) when is_integer(Id) ->
    can_view(Id, Context);
observe_acl_is_allowed(#acl_is_allowed{action=insert, object=#acl_media{mime=Mime, size=Size}}, Context) -> 
    can_media(Mime, Size, Context);
observe_acl_is_allowed(#acl_is_allowed{action=insert, object=#acl_rsc{category=Cat}}, Context) -> 
    can_insert(Cat, Context);
observe_acl_is_allowed(#acl_is_allowed{action=insert, object=Cat}, Context) when is_atom(Cat) -> 
    can_insert(Cat, Context);
observe_acl_is_allowed(#acl_is_allowed{action=update, object=Id}, Context) when is_integer(Id) ->
	case m_rsc:p_no_acl(Id, is_authoritative, Context) of
		true -> can_edit(Id, Context);
		_ -> undefined
	end;
observe_acl_is_allowed(#acl_is_allowed{action=delete, object=Id}, Context) when is_integer(Id) ->
	can_edit(Id, Context);
observe_acl_is_allowed(#acl_is_allowed{action=Action, object=ModuleName}, Context) when Action == use; Action == admin ->
	can_module(Action, ModuleName, Context);
observe_acl_is_allowed(#acl_is_allowed{object=#acl_edge{} = Edge}, Context) ->
    can_edge(Edge, Context);
observe_acl_is_allowed(#acl_is_allowed{}, _Context) ->
    undefined.

%% @doc Return the max visible_for an user can see, used for pruning during searches
observe_acl_can_see(#acl_can_see{}, #context{user_id=undefined}) ->
	?ACL_VIS_PUBLIC;
observe_acl_can_see(#acl_can_see{}, _Context) ->
	?ACL_VIS_USER.
	
%% @doc Let the user log on, this is the moment to start caching information.
observe_acl_logon(#acl_logon{id=UserId}, Context) ->
    logon(UserId, Context).
	
%% @doc Let the user log off, clean up any cached information.
observe_acl_logoff(#acl_logoff{}, Context) ->
	Context#context{acl=undefined, user_id=undefined}.

%% @doc Filter the properties before an update. Return filtered/updated resource proplist or
%% the tuple {error, Reason}
observe_acl_rsc_update_check(#acl_rsc_update_check{}, {error, Reason}, _Context) ->
	{error, Reason};
observe_acl_rsc_update_check(#acl_rsc_update_check{id=insert_rsc}, Props, Context) ->
	PropsPubl = case proplists:get_value(is_published, Props) of
		undefined -> z_utils:prop_replace(is_published, false, Props);
		_ -> Props
	end,
	PropsVis = constrain_visible_for(insert_rsc, PropsPubl, Context),
	case proplists:get_value(is_authoritative, PropsVis) of
		undefined -> z_utils:prop_replace(is_authoritative, true, PropsVis);
		_ -> PropsVis
	end;
observe_acl_rsc_update_check(#acl_rsc_update_check{id=Id}, Props, Context) ->
	constrain_visible_for(Id, Props, Context).

    % Make sure that the user doesn't try to change the visibility beyond what that user is allowed to do.
    % Retain any visibility the rsc already had.
    constrain_visible_for(Id, Props, Context) ->
        case proplists:get_value(visible_for, Props) of
    		undefined ->
                case min_visible(Context) of
                    N when is_integer(N) ->
                        z_utils:prop_replace(visible_for, N, Props);
                    undefined ->
                        Props
                end;
    		Vis -> 
    		    CurrVis = case Id of 
    		                insert_rsc -> ?ACL_VIS_USER;
    		                _ -> m_rsc:p_no_acl(Id, visible_for, Context)
    		              end,
    		    NewVis = z_convert:to_integer(Vis),
    		    case NewVis < CurrVis of
    		        true ->
            		    case min_visible(Context) of
            		        N when is_integer(N) ->
            		            z_utils:prop_replace(visible_for, erlang:max(N, NewVis), Props);
            		        undefined ->
            		            Props
            		    end;
            		false ->
            		    Props
            	end
    	end.

    min_visible(#context{user_id=?ACL_ADMIN_USER_ID}) ->
        ?ACL_VIS_PUBLIC;
    min_visible(#context{acl=admin}) ->
        ?ACL_VIS_PUBLIC;
    min_visible(#context{acl=ACL}) ->
        case ACL of
            #acl_user{visible_for=VisFor} -> VisFor;
            _ -> ?ACL_VIS_USER
        end.
        


%% @doc Check if the update contains information for a acl role.  If so then modify the acl role
%% information so that it is easier to handle.
%% @spec observe_rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
observe_rsc_update(#rsc_update{}, {Changed, Props}, _Context) ->
    case       proplists:is_defined(acl_cat, Props) 
        orelse proplists:is_defined(acl_mod, Props) 
        orelse proplists:is_defined(acl_mime, Props) 
        orelse proplists:is_defined(acl_view_all, Props)
        orelse proplists:is_defined(acl_file_upload_size, Props)
        orelse proplists:is_defined(acl_only_update_own, Props) of

        true ->
            Cats = proplists:get_all_values(acl_cat, Props),
            Mods = proplists:get_all_values(acl_mod, Props),
            Mimes = proplists:get_all_values(acl_mime, Props),
            VisibleFor = z_convert:to_integer(proplists:get_value(acl_visible_for, Props, 0)),
            FileSize = z_convert:to_integer(proplists:get_value(acl_file_upload_size, Props, 0)),
            ReadAll = z_convert:to_bool(proplists:get_value(acl_view_all, Props, false)),
            OnlyOwn = z_convert:to_bool(proplists:get_value(acl_only_update_own, Props, false)),
            Cats1 = [ z_convert:to_atom(C) || C <- Cats, C /= <<>> ],
            Mods1 = [ z_convert:to_atom(M) || M <- Mods, M /= <<>> ],
            Mimes1 = [ z_convert:to_binary(M) || M <- Mimes, M /= <<>> ],
            Props1 = proplists:delete(acl_cat, 
                        proplists:delete(acl_mod,
                            proplists:delete(acl_file_upload_size,
                                proplists:delete(acl_view_all,
                                    proplists:delete(acl_visible_for,
                                        proplists:delete(acl_only_update_own,
                                            proplists:delete(acl_mime, Props))))))),
            Props2 = [{acl, [   {categories, Cats1}, 
                                {modules, Mods1}, 
                                {view_all,ReadAll},
                                {only_update_own, OnlyOwn},
                                {file_upload_size, FileSize},
                                {file_mime, Mimes1},
                                {visible_for, VisibleFor}
                            ]} | Props1],
            {true, Props2};
        false ->
            {Changed, Props}
    end.


%% @doc On logon, cache ACL information in the context record
%% @todo Cache this information in the session (oid) so that we don't need to refetch it
%% on every request (each request is a continuation with a new logon).
logon(UserId, Context) ->
    % Fetch roles the user is member of
    ContextAdmin = z_acl:sudo(Context),
    case m_edge:subjects(UserId, acl_role_member, ContextAdmin) of
        [] ->
            %% When not member of a role then fallback to the member or anonymous role.
            case m_rsc:name_to_id(?ROLE_MEMBER, ContextAdmin) of
                {ok, RoleMember} -> logon_roles(UserId, [RoleMember], Context, ContextAdmin);
                {error, _} -> logon_roles(UserId, [], Context, ContextAdmin)
            end;
        Roles ->
            logon_roles(UserId, Roles, Context, ContextAdmin)
    end.


    logon_roles(UserId, Roles, Context, ContextAdmin) ->
        RolesFiltered = [ R || R <- Roles, 
                                m_rsc:is_a(R, acl_role, ContextAdmin),
                                m_rsc:p_no_acl(R, is_published, ContextAdmin) ],
        % Merge all role's categories and modules
        ACLs = [ m_rsc:p_no_acl(R, acl, ContextAdmin) || R <- RolesFiltered ],
        {Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor} 
                = combine(ACLs, [], [], false, undefined, 0, [], 3),
        Context#context{user_id=UserId,
                        acl=#acl_user{categories=lists:flatten(Cats), 
                                      modules=lists:flatten(Mods),
                                      roles=RolesFiltered,
                                      view_all=ViewAll,
                                      only_update_own=OnlyOwn,
                                      file_size=FileSize,
                                      file_mimes=lists:flatten(FileMime),
                                      visible_for=VisibleFor}}.

    combine([], Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor) ->
        {Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor};
    combine([undefined|Rest], Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor) ->
        combine(Rest, Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor);
    combine([ACL|Rest], Cats, Mods, ViewAll, OnlyOwn, FileSize, FileMime, VisibleFor) ->
        OnlyOwn1 = case {OnlyOwn, proplists:get_value(only_update_own, ACL, false)} of
                        {undefined, UpdOwn} -> UpdOwn;
                        {true, true} -> true;
                        {_, _} -> false
                   end,
        combine(Rest,
                [proplists:get_value(categories, ACL, [])|Cats],
                [proplists:get_value(modules, ACL, [])|Mods],
                ViewAll orelse proplists:get_value(view_all, ACL, false),
                OnlyOwn1,
                erlang:max(proplists:get_value(file_upload_size, ACL, 0), FileSize),
                [proplists:get_value(file_mime, ACL, [])|FileMime],
                erlang:min(proplists:get_value(visible_for, ACL, 0), VisibleFor)).


%% @doc Check if an user can see something (only called for authenticated users)
can_view(Id, Context) ->
    case can_view_all(Context) == true
        orelse can_edit(Id, Context) == true
        orelse is_view_community(Id, Context) == true of
        true -> true;
        false -> undefined
    end.


%% @doc Check if the user has 'view_all' permission
can_view_all(#context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
can_view_all(#context{acl=undefined}) ->
    undefined;
can_view_all(#context{acl=Acl}) ->
    case Acl#acl_user.view_all of 
        true -> true;
        _ -> undefined
    end.


%% @doc Check if the user can edit a rsc id
can_edit(_Id, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
can_edit(_Id, #context{acl=undefined}) ->
    undefined;

%% @doc A user can edit himself 
can_edit(UserId, #context{user_id=UserId, acl=#acl_user{only_update_own=true}}) when UserId /= undefined ->
    true;
%% @doc A user can edit content he created
can_edit(Id, #context{user_id=UserId, acl=#acl_user{only_update_own=true}} = Context) when UserId /= undefined ->
    case m_rsc:p_no_acl(Id, creator_id, Context) of
        UserId -> true;
        _ -> undefined
    end;

can_edit(Id, #context{acl=Acl} = Context) ->
    IsA = m_rsc:p_no_acl(Id, is_a, Context),
    can_edit1(IsA, Acl#acl_user.categories).

    can_edit1([], _Allowed) -> undefined;
    can_edit1([{Cat,_}|Cats], Allowed) -> 
        case lists:member(Cat, Allowed) of
            true -> true;
            false -> can_edit1(Cats, Allowed)
        end.

%% @doc Check if the user can use a module
can_module(_Action, _Module, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
can_module(_Action, _Module, #context{acl=undefined}) ->
    undefined;
can_module(use, Module, #context{acl=Acl}) ->
    case lists:member(Module, Acl#acl_user.modules) of
        true -> true;
        false -> undefined
    end;
can_module(_Action, _Module, _Context) ->
    undefined.



%% @doc Check if the category (or one of its parents) is in the category access list of the user
can_insert(_Cat, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
can_insert(_Cat, #context{acl=undefined}) ->
    undefined;
can_insert(Cat, #context{acl=Acl} = Context) ->
    case lists:member(Cat, Acl#acl_user.categories) of
        true -> 
            true;
        false ->
            IsA = m_category:is_a(Cat, Context),
            can_insert1(IsA,  Acl#acl_user.categories)
    end.
    
    can_insert1([], _Allowed) ->
        undefined;
    can_insert1([Cat|Cats], Allowed) ->
        case lists:member(Cat, Allowed) of
            true -> true;
            false -> can_insert1(Cats, Allowed)
        end.

%% @doc Check if a rsc is public viewable
is_view_public(Id, Context) ->
    is_view_level(Id, 0, Context).

is_view_community(Id, Context) ->
    is_view_level(Id, 1, Context).
    
is_view_level(Id, Level, Context) ->
    Acl = m_rsc:get_acl_props(Id, Context),
    case Acl#acl_props.is_published of
        false -> 
            false;
        true ->
            case Acl#acl_props.visible_for =< Level of
                false ->
                    false;
                true ->
                    Date = calendar:local_time(),
                    Acl#acl_props.publication_start =< Date andalso Acl#acl_props.publication_end >= Date
            end
    end.


%% @doc Check if the user has edit permission on a edge. The user must be able to edit the subject and view the object.
can_edge(_, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
can_edge(#acl_edge{predicate=acl_role_member, subject_id=SubjectId, object_id=ObjectId}, Context) ->
    case can_insert(acl_role, Context) == true
        andalso can_edit(SubjectId, Context) == true 
        andalso can_view(ObjectId, Context) == true of
        true -> true;
        false -> undefined
    end;
can_edge(#acl_edge{subject_id=SubjectId, object_id=ObjectId}, Context) ->
    case can_edit(SubjectId, Context) == true
        andalso can_view(ObjectId, Context) == true of
        true -> true;
        false -> undefined
    end.
    

can_media(Mime, Size, #context{acl=ACL}) ->
    case ACL of
        #acl_user{file_size=MaxSize, file_mimes=Allowed} ->
            case Size =< MaxSize * 1024 of
                true ->
                    case lists:member(<<"*/*">>, Allowed)
                         orelse lists:member(z_convert:to_binary(Mime), Allowed) of
                        true -> true;
                        false ->
                            case lists:member(make_wildcard(Mime), Allowed) of
                                true -> true;
                                false -> undefined
                            end
                    end;
                false ->
                    undefined
            end;
        _ -> undefined
    end.
    
    make_wildcard(Mime) ->
        [Type|_] = string:tokens(z_convert:to_list(Mime), "/"),
        list_to_binary(Type ++ "/*").



%% @doc The datamodel for the role based ACL
datamodel() ->
    [{categories,
      [
       {acl_role,
        meta,
        [{title, <<"ACL Role">>}]}
      ]
     },

     {predicates,
      [{acl_role_member,
        [{title, <<"ACL Role Member">>}],
        [{acl_role, person}, {acl_role, institution}]
       }]
     },
     
     {resources,
         [
            {?ROLE_MEMBER, acl_role,
             [{visible_for, 1},
              {title, "ACL role for members"},
              {summary, "The rights of this role are assigned to members (logged on) when they are not member of any other ACL role.  Make the user member of another role to overrule this role."},
              {acl, [   {view_all, false},
                        {only_update_own, false},
                        {file_upload_size, 1024},
                        {file_mime,["image/jpeg", "image/png", "image/gif"]},
                        {categories,[article,image]},
                        {modules,[]}]}
             ]
            }
         ]
     }
    ].

