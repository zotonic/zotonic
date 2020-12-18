%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2017 Marc Worrell
%% @doc Access control for Zotonic.  Interfaces to modules implementing the ACL events.

%% Copyright 2010-2017 Marc Worrell
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

         is_admin/1,
         sudo/1,
         sudo/2,
         anondo/1,
         anondo/2,
         logon/2,
         logon/3,
         logon_prefs/2,
         logon_prefs/3,
         logoff/1
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

%% @doc Check if an action is allowed for the current actor.
-spec is_allowed(action(), object(), z:context()) -> boolean().
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

-spec maybe_allowed(action(), object(), z:context()) -> maybe_boolean().
maybe_allowed(_Action, _Object, #context{ acl = admin }) ->
    true;
maybe_allowed(UpdateAction, _Object, #context{ acl_is_read_only = true }) when ?is_update_action(UpdateAction) ->
    false;
maybe_allowed(_Action, _Object, #context{user_id = ?ACL_ADMIN_USER_ID}) ->
    true;
maybe_allowed(Action, Object, Context) ->
    z_notifier:first(#acl_is_allowed{action=Action, object=Object}, Context).

%% @doc Check if an action on a property of a resource is allowed for the current actor.
-spec is_allowed_prop(action(), object(), atom() | binary(), z:context()) -> true | false | undefined.
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
        Other -> Other
    end.


%% @doc Check if the resource is visible for the current user
-spec rsc_visible( m_rsc:resource(), z:context() ) -> boolean().
rsc_visible(undefined, _Context) ->
    true;
rsc_visible(Id, #context{user_id = UserId}) when Id =:= UserId andalso is_integer(UserId) ->
    %% Can always see myself
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


%% @doc Check if a property of the resource is visible for the current user
-spec rsc_prop_visible(m_rsc:resource(), atom() | binary(), z:context()) -> boolean().
rsc_prop_visible(undefined, _Property, _Context) ->
    true;
rsc_prop_visible(Id, _Property, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
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

%% @doc Check if the resource is editable by the current user
-spec rsc_editable(m_rsc:resource(), z:context()) -> boolean().
rsc_editable(undefined, _Context) ->
    false;
rsc_editable(Id, #context{user_id=Id}) when is_integer(Id) ->
    %% Can always edit myself
    true;
rsc_editable(_Id, #context{acl=admin}) ->
    true;
rsc_editable(Id, Context) when is_integer(Id) ->
    is_allowed(update, Id, Context);
rsc_editable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_editable(RscId, Context)
    end.

%% @doc Check if the resource is deletable by the current user
-spec rsc_deletable(m_rsc:resource(), z:context()) -> boolean().
rsc_deletable(undefined, _Context) ->
    false;
rsc_deletable(_Id, #context{user_id=undefined}) ->
    false;
rsc_deletable(Id, #context{acl=admin} = Context) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_protected">>, Context));
rsc_deletable(Id, Context) when is_integer(Id) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, <<"is_protected">>, Context))
        andalso is_allowed(delete, Id, Context);
rsc_deletable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_deletable(RscId, Context)
    end.

%% @doc Check if the resource is connected to another resource by the current user
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

-spec is_read_only( z:context() ) -> boolean().
is_read_only(#context{ acl_is_read_only = IsReadOnly }) ->
    IsReadOnly.

-spec set_read_only( boolean(), z:context() ) -> z:context().
set_read_only(IsReadOnly, Context) ->
    Context#context{ acl_is_read_only = IsReadOnly }.


%% @doc Call a function with admin privileges.
-spec sudo( Fun, z:context() ) -> any()
    when Fun :: { module(), atom() }
             | mfa()
             | fun( (z:context()) -> any() ).
sudo({M,F}, Context) ->
    erlang:apply(M, F, [set_admin(Context)]);
sudo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_admin(Context)]);
sudo(F, Context) when is_function(F, 1) ->
    F(set_admin(Context)).

-spec sudo(z:context()) -> z:context().
sudo(Context) ->
    set_admin(Context).

-spec set_admin(z:context()) -> z:context().
set_admin(#context{ acl = undefined } = Context) ->
    Context#context{ acl = admin, user_id = ?ACL_ADMIN_USER_ID };
set_admin(Context) ->
    Context#context{ acl = admin }.

%% @doc Check if the current user is an admin or a sudo action
-spec is_admin( z:context() ) -> boolean().
is_admin(#context{ user_id = ?ACL_ADMIN_USER_ID }) -> true;
is_admin(#context{ acl = admin }) -> true;
is_admin(#context{ acl = undefined, user_id = undefined }) -> false;
is_admin(Context) -> is_allowed(use, mod_admin_config, Context).


%% @doc Call a function as the anonymous user.
-spec anondo(Fun, z:context()) -> any()
    when Fun :: { module(), atom() }
             | mfa()
             | fun( (z:context()) -> any() ).
anondo({M,F}, Context) ->
    erlang:apply(M, F, [set_anonymous(Context)]);
anondo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_anonymous(Context)]);
anondo(F, Context) when is_function(F, 1) ->
    F(set_anonymous(Context)).

-spec anondo( z:context() ) -> z:context().
anondo(Context) ->
    set_anonymous(Context).

-spec set_anonymous( z:context() ) -> z:context().
set_anonymous(#context{ acl = undefined, user_id = undefined } = Context) ->
    Context;
set_anonymous(Context) ->
    Context#context{ acl = undefined, user_id = undefined }.


%% @doc Log the user with the id on, fill the acl field of the context
-spec logon(m_rsc:resource() | undefined, z:context()) -> z:context().
logon(Id, #context{ user_id = Id } = Context) ->
    Context;
logon(Id, Context) ->
    logon(Id, #{}, Context).

-spec logon(m_rsc:resource() | undefined, map(), z:context()) -> z:context().
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

%% @doc Log the user with the id on, fill acl and set all user preferences (like timezone and language)
-spec logon_prefs(m_rsc:resource_id(), z:context()) -> z:context().
logon_prefs(Id, Context) ->
    logon_prefs(Id, #{}, Context).

-spec logon_prefs(m_rsc:resource_id(), map(), z:context()) -> z:context().
logon_prefs(Id, Options, Context) ->
    z_notifier:foldl(#user_context{ id = Id }, logon(Id, Options, Context), Context).


%% @doc Log off, reset the acl field of the context
-spec logoff( z:context() ) -> z:context().
logoff(#context{ user_id = undefined, acl = undefined} = Context) ->
    Context;
logoff(Context) ->
    case z_notifier:first(#acl_logoff{}, Context) of
        undefined -> Context#context{ user_id = undefined, acl = undefined};
        #context{} = NewContext -> NewContext
    end.

