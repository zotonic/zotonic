%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-05
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
-behaviour(gen_server).

-mod_title("ACL Simple Roles").
-mod_description("Simple role based access control.  Use this for a site with different editor roles.").
-mod_prio(500).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe/2,
	observe/3,
	rsc_update/3,
	datamodel/0
]).

-include("zotonic.hrl").

-record(acl_user, {modules, categories, roles, view_all}).
-record(state, {context}).

-define(ROLE_MEMBER, role_member).


%% @doc Check if the user is allowed to perform Action on Object
%% @todo #acl_edge
observe({acl_is_allowed, view, Id}, #context{user_id=undefined} = Context) when is_integer(Id) ->
    is_view_public(Id, Context);
observe({acl_is_allowed, _Action, _Object}, #context{user_id=undefined}) ->
	undefined;
% Logged on users
observe({acl_is_allowed, view, Id}, Context) when is_integer(Id) ->
    can_view(Id, Context);
observe({acl_is_allowed, insert, #acl_media{mime=Mime, size=_Size}}, Context) -> 
    case Mime of
        "image/" ++ _ -> can_insert(image, Context);
        "audio/" ++ _ -> can_insert(audio, Context);
        "video/" ++ _ -> can_insert(video, Context);
        "text/" ++ _ -> can_insert(document, Context);
        _ -> can_insert(media, Context)
    end;
observe({acl_is_allowed, insert, #acl_rsc{category=Cat}}, Context) -> 
    can_insert(Cat, Context);
observe({acl_is_allowed, insert, Cat}, Context) when is_atom(Cat) -> 
    can_insert(Cat, Context);
observe({acl_is_allowed, update, Id}, Context) when is_integer(Id) ->
	case m_rsc:p(Id, is_authoritative, Context) of
		true -> can_edit(Id, Context);
		_ -> undefined
	end;
observe({acl_is_allowed, delete, Id}, Context) when is_integer(Id) ->
	can_edit(Id, Context);
observe({acl_is_allowed, Action, ModuleName}, Context) when Action == use; Action == admin ->
	can_module(Action, ModuleName, Context);
observe({acl_is_allowed, _Action, #acl_edge{} = Edge}, Context) ->
    can_edge(Edge, Context);
observe({acl_is_allowed, _Action, _Object}, _Context) ->
    undefined;

%% @doc Return the max visible_for an user can see, used for pruning during searches
observe({acl_can_see, _Action, _Object}, #context{user_id=undefined}) ->
	?ACL_VIS_PUBLIC;
observe({acl_can_see}, _Context) ->
	?ACL_VIS_USER;
	
%% @doc Let the user log on, this is the moment to start caching information.
observe({acl_logon, UserId}, Context) ->
    logon(UserId, Context);
	
%% @doc Let the user log off, clean up any cached information.
observe({acl_logoff}, Context) ->
	Context#context{acl=undefined, user_id=undefined}.

%% @doc Filter the properties before an update. Return filtered/updated resource proplist or
%% the tuple {error, Reason}
observe({acl_rsc_update_check, _Id}, {error, Reason}, _Context) ->
	{error, Reason};
observe({acl_rsc_update_check, insert_rsc}, Props, _Context) ->
	PropsPubl = case proplists:get_value(is_published, Props) of
		undefined -> z_utils:prop_replace(is_published, false, Props);
		_ -> Props
	end,
	PropsVis = case proplists:get_value(visible_for, PropsPubl) of
		undefined -> z_utils:prop_replace(visible_for, ?ACL_VIS_PUBLIC, PropsPubl);
		_ -> PropsPubl
	end,
	case proplists:get_value(is_authoritative, PropsVis) of
		undefined -> z_utils:prop_replace(is_authoritative, true, PropsVis);
		_ -> PropsVis
	end;
observe({acl_rsc_update_check, _id}, Props, _Context) ->
	Props.



%% @doc Check if the update contains information for a acl role.  If so then modify the acl role
%% information so that it is easier to handle.
%% @spec rsc_update({rsc_update, ResourceId, OldResourceProps}, {Changed, UpdateProps}, Context) -> {NewChanged, NewUpdateProps}
rsc_update({rsc_update, _Id, _OldProps}, {Changed, Props}, _Context) ->
    case       proplists:is_defined(acl_cat, Props) 
        orelse proplists:is_defined(acl_mod, Props) 
        orelse proplists:is_defined(acl_view_all, Props) of

        true ->
            Cats = proplists:get_all_values(acl_cat, Props),
            Mods = proplists:get_all_values(acl_mod, Props),
            ReadAll = z_convert:to_bool(proplists:get_value(acl_view_all, Props, false)),
            Cats1 = [ z_convert:to_atom(C) || C <- Cats, C /= <<>> ],
            Mods1 = [ z_convert:to_atom(M) || M <- Mods, M /= <<>> ],
            Props1 = proplists:delete(acl_cat, 
                        proplists:delete(acl_mod, Props)),
            Props2 = [{acl, [{categories, Cats1}, {modules, Mods1}, {view_all,ReadAll}]} | Props1],
            {true, Props2};
        false ->
            {Changed, Props}
    end.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    ContextAdmin = z_acl:sudo(Context),
    
    %% Manage our data model
    z_datamodel:manage(?MODULE, datamodel(), ContextAdmin),

    z_notifier:observe(acl_is_allowed, {?MODULE, observe}, ContextAdmin),
    z_notifier:observe(acl_logon, {?MODULE, observe}, ContextAdmin),
    z_notifier:observe(acl_logoff, {?MODULE, observe}, ContextAdmin),
    z_notifier:observe(acl_rsc_update_check, {?MODULE, observe}, ContextAdmin),
    z_notifier:observe(acl_can_see, {?MODULE, observe}, ContextAdmin),
    z_notifier:observe(rsc_update, {?MODULE, rsc_update}, ContextAdmin),
    {ok, #state{context=ContextAdmin}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    z_notifier:detach(acl_is_allowed, {?MODULE, observe}, State#state.context),
    z_notifier:detach(acl_logon, {?MODULE, observe}, State#state.context),
    z_notifier:detach(acl_logoff, {?MODULE, observe}, State#state.context),
    z_notifier:detach(acl_rsc_update_check, {?MODULE, observe}, State#state.context),
    z_notifier:detach(acl_can_see, {?MODULE, observe}, State#state.context),
    z_notifier:detach(rsc_update, {?MODULE, rsc_update}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc On logon, cache ACL information in the context record
%% @todo Cache this information in the session (oid) so that we don't need to refetch it
%% on every request (each request is a continuation with a new logon).
logon(UserId, Context) ->
    % Fetch roles the user is member of
    ContextAdmin = z_acl:sudo(Context),
    case m_edge:subjects(UserId, acl_role_member, ContextAdmin) of
        [] ->
            %% When not member of a role then fallback to the member or anonymous role.
            case m_rsc:name_to_id(?ROLE_MEMBER, Context) of
                {ok, RoleMember} -> logon_roles(UserId, [RoleMember], Context, ContextAdmin);
                {error, _} -> logon_roles(UserId, [], Context, ContextAdmin)
            end;
        Roles ->
            logon_roles(UserId, Roles, Context, ContextAdmin)
    end.


    logon_roles(UserId, Roles, Context, ContextAdmin) ->
        RolesFiltered = [ R || R <- Roles, 
                                m_rsc:is_a(R, acl_role, ContextAdmin),
                                m_rsc:p(R, is_published, ContextAdmin) ],
        % Merge all role's categories and modules
        ACLs = [ m_rsc:p(R, acl, ContextAdmin) || R <- RolesFiltered ],
        {Cats, Mods, ViewAll} = combine(ACLs, [], [], false),
        Context#context{user_id=UserId,
                        acl=#acl_user{categories=lists:flatten(Cats), 
                                      modules=lists:flatten(Mods),
                                      roles=RolesFiltered,
                                      view_all=ViewAll}}.

    combine([], Cats, Mods, ViewAll) ->
        {Cats, Mods, ViewAll};
    combine([undefined|Rest], Cats, Mods, ViewAll) ->
        combine(Rest, Cats, Mods, ViewAll);
    combine([ACL|Rest], Cats, Mods, ViewAll) ->
        combine(Rest,
                [proplists:get_value(categories, ACL, [])|Cats],
                [proplists:get_value(modules, ACL, [])|Mods],
                ViewAll orelse proplists:get_value(view_all, ACL, false)).


%% @doc Check if an user can see something
can_view(Id, Context) ->
    case can_view_all(Context) == true
        orelse can_edit(Id, Context) == true
        orelse is_view_public(Id, Context) == true of
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
can_edit(Id, #context{acl=Acl} = Context) ->
    IsA = m_rsc:p(Id, is_a, Context),
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
	Acl = m_rsc:get_acl_props(Id, Context),
    case Acl#acl_props.is_published of
        false -> 
            false;
        true ->
            case Acl#acl_props.visible_for == 0 of
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
        [{acl_role, person}]
       }]
     },
     
     {resources,
         [
            {?ROLE_MEMBER, acl_role,
             [{visible_for, 1},
              {title, "ACL role for members"},
              {summary, "The rights of this role are assigned to members (logged on) when they are not member of any other ACL role.  Make the user member of another role to overrule this role."},
              {acl, [{view_all, false},{categories,[]},{modules,[]}]}
             ]
            }
         ]
     }
    ].

