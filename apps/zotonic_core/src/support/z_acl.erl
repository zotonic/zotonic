%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2024 Marc Worrell
%% @doc Access control for Zotonic.  Interfaces to modules implementing the ACL events.
%% @end

%% Copyright 2010-2024 Marc Worrell
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

-module(z_acl).
-author("Marc Worrell <marc@worrell.nl>").

-export([is_allowed/3,
         is_allowed_prop/4,
         is_allowed_link/4,
         maybe_allowed/3,

         rsc_visible/2,
         rsc_prop_visible/3,
         rsc_editable/2,
         rsc_deletable/2,
         rsc_linkable/2,

         cache_key/1,

         user/1,
         user_groups/1,

         is_read_only/1,
         set_read_only/2,

         is_admin_editable/1,
         is_admin/1,
         is_sudo/1,
         sudo/1,
         sudo/2,
         anondo/1,
         anondo/2,
         logon/2,
         logon/3,
         logon_prefs/2,
         logon_prefs/3,
         logon_refresh/1,
         logoff/1,

         flush/1
        ]).

-include_lib("zotonic.hrl").

-type acl() :: list( operationrequest() ).
-type operationrequest() :: {action(), object()}.
-type action() :: use
                | admin
                | view
                | insert
                | update
                | delete
                | link
                | atom().
-type object() :: m_rsc:resource()
                | #acl_rsc{}
                | #acl_edge{}
                | #acl_media{}
                | any().
-type maybe_boolean() :: undefined
                       | boolean().

-export_type([
    acl/0,
    operationrequest/0,
    action/0,
    object/0,
    maybe_boolean/0
]).

-define(is_update_action(A), A =:= admin; A =:= insert; A =:= update; A =:= delete; A =:= link).

%% @doc Check if an action is allowed for the current actor. If the ACL is inconclusive and
%% returns 'undefined' then the action is not allowed.
-spec is_allowed(Action, Object, Context) -> IsAllowed when
    Action :: action(),
    Object :: object(),
    Context :: z:context(),
    IsAllowed :: boolean().
is_allowed(_Action, _Object, #context{ acl = admin }) ->
    true;
is_allowed(UpdateAction, _Object, #context{ acl_is_read_only = true }) when ?is_update_action(UpdateAction) ->
    false;
is_allowed(_Action, _Object, #context{ user_id=?ACL_ADMIN_USER_ID }) ->
    true;
is_allowed(link, Object, Context) ->
    is_allowed(insert, #acl_edge{subject_id=Object, predicate=relation, object_id=Object}, Context);
is_allowed(Action, Object, Context) ->
    case maybe_allowed(Action, Object, Context) of
        undefined -> false;
        true -> true;
        false -> false
    end.

%% @doc Check if an action is allowed for the current actor. Can return an inconclusive answer
%% with 'undefined'. The caller has then to decide what to do.
-spec maybe_allowed(Action, Object, Context) -> MaybeIsAllowed when
    Action :: action(),
    Object :: object(),
    Context :: z:context(),
    MaybeIsAllowed :: maybe_boolean().
maybe_allowed(_Action, _Object, #context{ acl = admin }) ->
    true;
maybe_allowed(UpdateAction, _Object, #context{ acl_is_read_only = true }) when ?is_update_action(UpdateAction) ->
    false;
maybe_allowed(_Action, _Object, #context{user_id = ?ACL_ADMIN_USER_ID}) ->
    true;
maybe_allowed(Action, Object, Context) ->
    z_notifier:first(#acl_is_allowed{action=Action, object=Object}, Context).

%% @doc Check if an action on a property of a resource is allowed for the current actor. If the ACL
%% is inconclusive and returns 'undefined' then the property is assumed to be visible. This is
%% different then the is_allowed for resources, where an inconclusive answer is assumed to be that
%% the resource is not visible.
-spec is_allowed_prop(Action, Object, Property, Context) -> IsAllowed when
    Action :: action(),
    Object :: object(),
    Property :: atom() | binary(),
    Context :: z:context(),
    IsAllowed :: true | false.
is_allowed_prop(_Action, _Object, _Property, #context{ acl = admin }) ->
    true;
is_allowed_prop(UpdateAction, _Object, _Property, #context{ acl_is_read_only = true }) when ?is_update_action(UpdateAction) ->
    false;
is_allowed_prop(_Action, _Object, _Property, #context{ user_id = ?ACL_ADMIN_USER_ID }) ->
    true;
is_allowed_prop(Action, Object, Property, Context) when is_atom(Property) ->
    is_allowed_prop(Action, Object, atom_to_binary(Property, utf8), Context);
is_allowed_prop(Action, Object, Property, Context) ->
    case z_notifier:first(#acl_is_allowed_prop{action=Action, object=Object, prop=Property}, Context) of
        undefined -> true; % Note, the default behaviour is different for props!
        true -> true;
        false -> false
    end.

%% @doc Check if it is allowed to create an edge between the subject and object using the predicate.
-spec is_allowed_link(Subject, Predicate, Object, Context) -> boolean() when
    Subject :: m_rsc:resource(),
    Predicate :: m_rsc:resource(),
    Object :: m_rsc:resource(),
    Context :: z:context().
is_allowed_link(SubjectId, PredicateId, ObjectId, Context) when
    is_integer(SubjectId), is_integer(PredicateId), is_integer(ObjectId) ->
    try
        case z_acl:rsc_visible(SubjectId, Context)
            andalso z_acl:rsc_visible(ObjectId, Context)
            andalso z_acl:rsc_visible(PredicateId, Context)
            andalso m_rsc:is_a(PredicateId, predicate, Context)
        of
            true ->
                {ok, PredName} = m_predicate:id_to_name(PredicateId, Context),
                z_acl:is_allowed(
                    insert,
                    #acl_edge{
                        subject_id = SubjectId,
                        predicate = PredName,
                        object_id = ObjectId
                    },
                    Context);
            false ->
                false
        end
    catch
        error:badarg -> false
    end;
is_allowed_link(undefined, _Predicate, _ObjectId, _Context) -> false;
is_allowed_link(_Subject, undefined, _ObjectId, _Context) -> false;
is_allowed_link(_Subject, _Predicate, undefined, _Context) -> false;
is_allowed_link(Subject, Predicate, Object, Context) ->
    SubjectId = m_rsc:rid(Subject, Context),
    PredicateId = m_rsc:rid(Predicate, Context),
    ObjectId = m_rsc:rid(Object, Context),
    is_allowed_link(SubjectId, PredicateId, ObjectId, Context).

%% @doc Check if a resource is visible for the current user. Non existing
%% resources are visible.
-spec rsc_visible( m_rsc:resource(), z:context() ) -> boolean().
rsc_visible(undefined, _Context) ->
    true;
rsc_visible(_Id, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
rsc_visible(_Id, #context{acl=admin}) ->
    true;
rsc_visible(Id, Context) when is_integer(Id) ->
    case z_memo:is_enabled(Context) of
        true ->
            case z_memo:get({rsc_visible, Id}) of
                undefined ->
                    Visible = is_allowed(view, Id, Context),
                    z_memo:set({rsc_visible, Id}, Visible),
                    Visible;
                Visible ->
                    Visible
            end;
        false ->
            is_allowed(view, Id, Context)
    end;
rsc_visible(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> true;
        RscId -> rsc_visible(RscId, Context)
    end.


%% @doc Check if a property of the resource is visible for the current user. If the
%% resource does not exist then the peoperty is visible.
-spec rsc_prop_visible(Resource, Property, Context) -> IsVisible when
    Resource :: m_rsc:resource(),
    Property :: atom() | binary(),
    Context :: z:context(),
    IsVisible :: boolean().
rsc_prop_visible(undefined, _Property, _Context) ->
    true;
rsc_prop_visible(_Id, _Property, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
rsc_prop_visible(_Id, _Property, #context{acl=admin}) ->
    true;
rsc_prop_visible(Id, Property, Context) when is_atom(Property) ->
    rsc_prop_visible(Id, atom_to_binary(Property, utf8), Context);
rsc_prop_visible(Id, Property, Context) when is_integer(Id) ->
    case z_memo:is_enabled(Context) of
        true ->
            case z_memo:get({rsc_prop_visible, Id, Property}) of
                undefined ->
                    Visible = is_allowed_prop(view, Id, Property, Context),
                    z_memo:set({rsc_prop_visible, Id, Property}, Visible),
                    Visible;
                Visible ->
                    Visible
            end;
        false ->
            is_allowed_prop(view, Id, Property, Context)
    end;
rsc_prop_visible(RscName, Property, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_prop_visible(RscId, Property, Context)
    end.

%% @doc Check if a resource can be edited by the current user. Non existing
%% resources are not editable.
-spec rsc_editable(m_rsc:resource(), z:context()) -> boolean().
rsc_editable(undefined, _Context) ->
    false;
rsc_editable(_Id, #context{ acl = admin }) ->
    true;
rsc_editable(Id, Context) when is_integer(Id) ->
    is_allowed(update, Id, Context);
rsc_editable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_editable(RscId, Context)
    end.

%% @doc Check if a resource can be deleted by the current user. Non existing
%% resources are not deletable.
-spec rsc_deletable(m_rsc:resource(), z:context()) -> boolean().
rsc_deletable(undefined, _Context) ->
    false;
rsc_deletable(_Id, #context{ user_id = undefined }) ->
    false;
rsc_deletable(Id, #context{ acl = admin } = Context) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_protected">>, Context));
rsc_deletable(Id, Context) when is_integer(Id) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_protected">>, Context))
        andalso is_allowed(delete, Id, Context);
rsc_deletable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_deletable(RscId, Context)
    end.

%% @doc Check if an connection can be added to the resource. Returns true if
%% the ACL allows adding a 'relation' edge from the resource to itself.
-spec rsc_linkable(m_rsc:resource(), z:context()) -> boolean().
rsc_linkable(undefined, _Context) ->
    false;
rsc_linkable(_Id, #context{ acl = admin }) ->
    true;
rsc_linkable(Id, Context) when is_integer(Id) ->
    is_allowed(link, Id, Context);
rsc_linkable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> is_allowed(link, RscId, Context)
    end.

%% @doc Return a term that can be used as the ACL part of cache key.
-spec cache_key( z:context() ) -> { m_rsc:resource_id() | undefined, any()}.
cache_key(Context) ->
    {Context#context.user_id, Context#context.acl}.


%% @doc Return the id of the current user.
-spec user(z:context()) -> m_rsc:resource_id() | undefined.
user(#context{user_id = UserId}) ->
    UserId.

%% @doc Return the list of user groups the current context is member of.
-spec user_groups(z:context()) -> [ m_rsc:resource_id() ].
user_groups(Context) ->
    case z_notifier:first(#acl_user_groups{}, Context) of
        undefined -> [];
        L when is_list(L) -> L
    end.

%% @doc Check if the current access permissions are set to read-only.
%% This is an authorization option for the current z.auth cookie or
%% bearer token.
-spec is_read_only( z:context() ) -> boolean().
is_read_only(#context{ acl = admin }) ->
    % Sudo is never read only.
    false;
is_read_only(#context{ acl_is_read_only = IsReadOnly }) ->
    IsReadOnly.

%% @doc Set the current context to read only. Models can use this
%% state to prevent updates to data.
-spec set_read_only( boolean(), z:context() ) -> z:context().
set_read_only(IsReadOnly, Context) ->
    Context#context{ acl_is_read_only = IsReadOnly }.

%% @doc Check if the current context acl is set using a sudo.
-spec is_sudo( z:context() ) -> boolean().
is_sudo(#context{ acl = admin }) ->
    true;
is_sudo(_) ->
    false.

%% @doc Call a function with admin privileges. If the function is a MFA
%% then the sudo-context is appended to the argument list as the last
%% function argument.
-spec sudo(Fun, ContextOrSite) -> Value when
    Fun :: { module(), atom() }
         | mfa()
         | fun( (SudoContext) -> any() ),
    ContextOrSite :: z:context() | atom(),
    SudoContext :: z:context(),
    Value :: any().
sudo({M,F}, Context) ->
    erlang:apply(M, F, [set_admin(Context)]);
sudo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_admin(Context)]);
sudo(F, Context) when is_function(F, 1) ->
    F(set_admin(Context)).

%% @doc Return a context with sudo permissions set. The user of the context
%% stays the same, except when there is ACL set, then the user is set to
%% the id of the admin user (1).
-spec sudo(ContextOrSite) -> SudoContext when
    ContextOrSite :: z:context() | atom(),
    SudoContext :: z:context().
sudo(Context) ->
    set_admin(Context).

%% @doc Return a context with sudo permissions set. The user of the context
%% stays the same, except when there is no ACL set, then the user is set to
%% the id of the admin user (1).
-spec set_admin(ContextOrSite) -> SudoContext when
    ContextOrSite :: z:context() | atom(),
    SudoContext :: z:context().
set_admin(#context{ acl = undefined } = Context) ->
    Context#context{ acl = admin, user_id = ?ACL_ADMIN_USER_ID };
set_admin(#context{ acl = admin } = Context) ->
    Context;
set_admin(#context{} = Context) ->
    Context#context{ acl = admin };
set_admin(Site) when is_atom(Site) ->
    set_admin(z_context:new(Site)).

%% @doc Check if an admin is logged on and the read only flag is not set.
%% Exception for sudo, where updates are always allowed.
-spec is_admin_editable( z:context() ) -> boolean().
is_admin_editable(#context{ acl = admin }) -> true;
is_admin_editable(#context{ acl_is_read_only = true }) -> false;
is_admin_editable(Context) -> is_admin(Context).

%% @doc Check if the current user is an admin or a sudo action
-spec is_admin( z:context() ) -> boolean().
is_admin(#context{ user_id = ?ACL_ADMIN_USER_ID }) -> true;
is_admin(#context{ acl = admin }) -> true;
is_admin(#context{ acl = undefined, user_id = undefined }) -> false;
is_admin(Context) -> is_allowed(use, mod_admin_config, Context).


%% @doc Call a function as the anonymous user. The acl and user is removed
%% from the context. If the function is a MFA then the anonymous context
%% is added as the last argument.
-spec anondo(Fun, Context) -> Value when
    Fun :: { module(), atom() }
         | mfa()
         | fun( (AnonContext) -> any() ),
    Context :: z:context(),
    AnonContext :: z:context(),
    Value :: any().
anondo({M,F}, Context) ->
    erlang:apply(M, F, [set_anonymous(Context)]);
anondo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_anonymous(Context)]);
anondo(F, Context) when is_function(F, 1) ->
    F(set_anonymous(Context)).

%% @doc Make the context an anymous context by stripping the acl and user
%% from the context.
-spec anondo(Context) -> AnonContext when
    Context :: z:context(),
    AnonContext :: z:context().
anondo(Context) ->
    set_anonymous(Context).

%% @doc Make the context an anymous context by stripping the acl and user
%% from the context.
-spec set_anonymous(Context) -> AnonContext when
    Context :: z:context(),
    AnonContext :: z:context().
set_anonymous(#context{ acl = undefined, user_id = undefined } = Context) ->
    Context;
set_anonymous(Context) ->
    Context#context{ acl = undefined, user_id = undefined }.


%% @doc Set the context to the user's context, with the given user id and the
%% access permissions of the user. Note that the user's preferences
%% are not set, use logon_prefs/2 to set those.
-spec logon(User, Context) -> UserContext when
    User :: m_rsc:resource(),
    Context :: z:context(),
    UserContext :: z:context().
logon(Id, #context{ user_id = Id } = Context) ->
    Context;
logon(Id, Context) ->
    logon(Id, #{}, Context).

%% @doc Set the context to the user's context, with the given user id and the
%% access permissions of the user. The options are passed to the ACL module. Check
%% the selected ACL module(s) for supported options. Note that the user's preferences
%% are not set, use logon_prefs/3 to set those.
-spec logon(User, Options, Context) -> UserContext when
    User :: m_rsc:resource(),
    Options :: map(),
    Context :: z:context(),
    UserContext :: z:context().
logon(Id, Options, Context) ->
    UserId = m_rsc:rid(Id, Context),
    case z_notifier:first(#acl_logon{ id = UserId, options = Options }, Context) of
        undefined ->
            Context#context{
                acl = undefined,
                user_id = UserId
            };
        #context{} = NewContext ->
            NewContext
    end.

%% @doc Refresh the authentication of the current user
-spec logon_refresh(z:context()) -> z:context().
logon_refresh(#context{ user_id = Id } = Context) when is_integer(Id) ->
    logon(Id, #{}, Context);
logon_refresh(Context) ->
    Context.


%% @doc Log the user with the id on, fill acl and set all user preferences (like timezone and language)
-spec logon_prefs(User, Context) -> UserContext when
    User :: m_rsc:resource_id(),
    Context :: z:context(),
    UserContext :: z:context().
logon_prefs(Id, Context) ->
    logon_prefs(Id, #{}, Context).

%% @doc Log the user with the id on, fill acl and set all user preferences (like timezone
%% and language). The options are passed to the ACL module. Check the selected ACL module(s)
%% for supported options.
-spec logon_prefs(User, Options, Context) -> UserContext when
    User :: m_rsc:resource(),
    Options :: map(),
    Context :: z:context(),
    UserContext :: z:context().
logon_prefs(Id, Options, Context) ->
    z_notifier:foldl(#user_context{ id = Id }, logon(Id, Options, Context), Context).


%% @doc Log off, reset the acl field of the context. Call the #acl_logoff notification
%% if a user is defined. This allows the ACL module to make adjustments to the context.
-spec logoff(UserContext) -> AnonContext when
    UserContext :: z:context(),
    AnonContext :: z:context().
logoff(#context{ user_id = undefined, acl = undefined} = Context) ->
    Context;
logoff(Context) ->
    case z_notifier:first(#acl_logoff{}, Context) of
        undefined -> Context#context{ user_id = undefined, acl = undefined};
        #context{} = NewContext -> NewContext
    end.


%% @doc Flush the memo cache of ACL lookups for the given resource id.
-spec flush(Id) -> ok when
    Id :: m_rsc:resource_id().
flush(Id) ->
     z_memo:delete({rsc_visible, Id}),
     ok.

