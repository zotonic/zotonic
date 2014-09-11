%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2013 Marc Worrell
%% @doc Access control for Zotonic.  Interfaces to modules implementing the ACL events.

%% Copyright 2010-2013 Marc Worrell
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
         maybe_allowed/3,

         rsc_visible/2,
         rsc_prop_visible/3,
         rsc_editable/2,
         rsc_deletable/2,

         rsc_update_check/3,

         args_to_visible_for/1,
         set_visible_for/2,
         can_see/1,
         cache_key/1,

         user/1,
         is_admin/1,
         sudo/1,
         sudo/2,
         anondo/1,
         anondo/2,
         logon/2,
         logon_prefs/2,
         logoff/1,

         wm_is_authorized/2,
         wm_is_authorized/3,
         wm_is_authorized/4,
         wm_is_authorized/5
        ]).

-export_type([acl/0]).

-include_lib("zotonic.hrl").

-type acl() :: list(operationrequest()).
-type operationrequest() :: {action(), object()}.
-type action() :: atom().
-type object() :: m_rsc:resource().

%% @doc Check if an action is allowed for the current actor.
-spec is_allowed(term(), term(), #context{}) -> true | false.
is_allowed(_Action, _Object, #context{acl=admin}) ->
    true;
is_allowed(_Action, _Object, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
is_allowed(Action, Object, Context) ->
    case maybe_allowed(Action, Object, Context) of
        undefined -> false;
        true -> true;
        false -> false
    end.

-spec maybe_allowed(term(), term(), #context{}) -> true | false | undefined.
maybe_allowed(_Action, _Object, #context{acl=admin}) ->
    true;
maybe_allowed(_Action, _Object, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
maybe_allowed(Action, Object, Context) ->
    z_notifier:first(#acl_is_allowed{action=Action, object=Object}, Context).

%% @doc Check if an action on a property of a resource is allowed for the current actor.
-spec is_allowed_prop(term(), term(), atom(), #context{}) -> true | false | undefined.
is_allowed_prop(_Action, _Object, _Property, #context{acl=admin}) ->
    true;
is_allowed_prop(_Action, _Object, _Property, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
is_allowed_prop(Action, Object, Property, Context) ->
    case z_notifier:first(#acl_is_allowed_prop{action=Action, object=Object, prop=Property}, Context) of
        undefined -> true; % Note, the default behaviour is different for props!
        Other -> Other
    end.


%% @doc Check if the resource is visible for the current user
rsc_visible(undefined, _Context) ->
    true;
rsc_visible(Id, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
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
rsc_prop_visible(undefined, _Property, _Context) ->
    true;
rsc_prop_visible(Id, _Property, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
    true;
rsc_prop_visible(_Id, _Property, #context{user_id=?ACL_ADMIN_USER_ID}) ->
    true;
rsc_prop_visible(_Id, _Property, #context{acl=admin}) ->
    true;
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
rsc_editable(_Id, #context{user_id=undefined}) ->
    %% Anonymous visitors can't edit anything
    false;
rsc_editable(undefined, _Context) ->
    false;
rsc_editable(Id, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
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
rsc_deletable(undefined, _Context) ->
    false;
rsc_deletable(_Id, #context{user_id=undefined}) ->
    false;
rsc_deletable(Id, #context{acl=admin} = Context) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, is_protected, Context));
rsc_deletable(Id, Context) when is_integer(Id) ->
    not z_convert:to_bool(m_rsc:p_no_acl(Id, is_protected, Context))
        andalso is_allowed(delete, Id, Context);
rsc_deletable(RscName, Context) ->
    case m_rsc:rid(RscName, Context) of
        undefined -> false;
        RscId -> rsc_deletable(RscId, Context)
    end.


%% @doc Filter the properties of an update.  This is before any escaping.
rsc_update_check(Id, Props, Context) ->
    z_notifier:foldl(#acl_rsc_update_check{id=Id}, Props, Context).


%% @doc Set the acl fields of the context for the 'visible_for' setting.  Used when rendering scomps.
%% @spec set_visible_for(integer(), context()) -> context()
%% @todo Change this for the pluggable ACL
set_visible_for(_VisibleFor, #context{user_id=undefined} = Context) ->
    Context;
set_visible_for(?ACL_VIS_PUBLIC, Context) ->
    Context#context{user_id=undefined, acl=undefined};
set_visible_for(?ACL_VIS_COMMUNITY, Context) ->
    Context#context{user_id=?ACL_ANY_USER_ID, acl=undefined};
set_visible_for(?ACL_VIS_GROUP, Context) ->
    Context#context{acl=undefined};
set_visible_for(?ACL_VIS_USER, Context) ->
    Context.


%% @doc Return the max visible_for the current user can see
can_see(#context{user_id=undefined}) ->
    ?ACL_VIS_PUBLIC;
can_see(#context{user_id=?ACL_ADMIN_USER_ID}) ->
    ?ACL_VIS_USER;
can_see(#context{acl=admin}) ->
    ?ACL_VIS_USER;
can_see(#context{user_id=?ACL_ANY_USER_ID}) ->
    ?ACL_VIS_COMMUNITY;
can_see(Context) ->
    case z_notifier:first(#acl_can_see{}, Context) of
        undefined -> [];
        CanSee -> CanSee
    end.


%% @doc Translate "visible_for" parameter to the appropriate visibility level.
%% @spec args_to_visible_for(proplist()) -> 0 | 1 | 2 | 3
args_to_visible_for(Args) ->
    case proplists:get_value(visible_for, Args) of
        undefined   -> ?ACL_VIS_USER;
        "user"      -> ?ACL_VIS_USER;
        3           -> ?ACL_VIS_USER;
        "group"     -> ?ACL_VIS_GROUP;
        2           -> ?ACL_VIS_GROUP;
        "community" -> ?ACL_VIS_COMMUNITY;
        1           -> ?ACL_VIS_COMMUNITY;
        "world"     -> ?ACL_VIS_PUBLIC;
        "public"    -> ?ACL_VIS_PUBLIC;
        0           -> ?ACL_VIS_PUBLIC
    end.


%% @doc Return a term that can be used as the ACL part of cache key.
%% @spec cache_key(Context) -> term()
cache_key(Context) ->
    {Context#context.user_id, Context#context.acl}.


%% @doc Return the id of the current user.
user(#context{user_id=UserId}) ->
    UserId.


%% @doc Call a function with admin privileges.
%% @spec sudo(FuncDef, #context{}) -> FuncResult
sudo({M,F}, Context) ->
    erlang:apply(M, F, [set_admin(Context)]);
sudo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_admin(Context)]);
sudo(F, Context) when is_function(F, 1) ->
    F(set_admin(Context)).

sudo(Context) ->
    set_admin(Context).

set_admin(#context{acl=undefined} = Context) ->
    Context#context{acl=admin, user_id=?ACL_ADMIN_USER_ID};
set_admin(Context) ->
    Context#context{acl=admin}.


%% @doc Check if the current user is the admin or a sudo action
is_admin(#context{user_id=?ACL_ADMIN_USER_ID}) -> true;
is_admin(#context{acl=admin}) -> true;
is_admin(_) -> false.


%% @doc Call a function as the anonymous user.
%% @spec anondo(FuncDef, #context{}) -> FuncResult
-spec anondo({atom(),atom()}|{atom(),atom(),list()}|function(), #context{}) -> any().
anondo({M,F}, Context) ->
    erlang:apply(M, F, [set_anonymous(Context)]);
anondo({M,F,A}, Context) ->
    erlang:apply(M, F, A ++ [set_anonymous(Context)]);
anondo(F, Context) when is_function(F, 1) ->
    F(set_anonymous(Context)).

anondo(Context) ->
    set_anonymous(Context).

-spec set_anonymous(#context{}) -> #context{}.
set_anonymous(Context) ->
    Context#context{acl=undefined, user_id=undefined}.


%% @doc Log the user with the id on, fill the acl field of the context
-spec logon(pos_integer(), #context{}) -> #context{}.
logon(Id, Context) ->
    case z_notifier:first(#acl_logon{id=Id}, Context) of
        undefined -> Context#context{acl=undefined, user_id=Id};
        #context{} = NewContext -> NewContext
    end.

%% @doc Log the user with the id on, fill acl and set all user preferences (like timezone and language)
-spec logon_prefs(pos_integer(), #context{}) -> #context{}.
logon_prefs(Id, Context) ->
    z_notifier:foldl(#user_context{id=Id}, z_acl:logon(Id, Context), Context).


%% @doc Log off, reset the acl field of the context
-spec logoff(#context{}) -> #context{}.
logoff(#context{user_id=undefined, acl=undefined} = Context) ->
    Context;
logoff(Context) ->
    case z_notifier:first(#acl_logoff{}, Context) of
        undefined -> Context#context{user_id=undefined, acl=undefined};
        #context{} = NewContext -> NewContext
    end.


%% @doc Convenience function, check if the current user has enough permissions, if not then
%% redirect to the logon page.
-spec wm_is_authorized(boolean() | acl(), #context{}) -> webzmachine:reply().
wm_is_authorized(true, Context) ->
    ?WM_REPLY(true, Context);
wm_is_authorized(false, Context) ->
    wm_is_authorized(false, logon, Context);
wm_is_authorized(ACLs, Context) when is_list(ACLs) ->
    wm_is_authorized(ACLs, logon, Context).

-spec wm_is_authorized(boolean() | acl(), Redirect | ReqData, #context{}) -> webzmachine:reply() when
      Redirect :: atom(),
      ReqData :: webzmachine:reqdata().
wm_is_authorized(true, _Redirect, Context) ->
    wm_is_authorized(true, Context);
wm_is_authorized(false, Redirect, Context) ->
    ContextLocation = wm_set_location(Redirect, Context),
    ?WM_REPLY({halt, 302}, ContextLocation);
wm_is_authorized(ACLs, Redirect, Context) when is_list(ACLs), is_atom(Redirect) ->
    wm_is_authorized(wm_is_allowed(ACLs, Context), Redirect, Context);
wm_is_authorized(ACLs, ReqData, Context) when is_list(ACLs) ->
    wm_is_authorized(ACLs, ?WM_REQ(ReqData, Context));
wm_is_authorized(Action, Object, Context) ->
    wm_is_authorized([{Action, Object}], Context).

-spec wm_is_authorized(action(), object(), webzmachine:reqdata(), #context{}) -> webzmachine:reply().
wm_is_authorized(Action, Object, ReqData, Context) ->
    wm_is_authorized([{Action, Object}], ?WM_REQ(ReqData, Context)).

-spec wm_is_authorized(action(), object(), Redirect, ReqData, #context{}) -> webzmachine:reply() when
      Redirect :: atom(),
      ReqData :: webzmachine:reqdata().
wm_is_authorized(Action, Object, Redirect, ReqData, Context) ->
    wm_is_authorized([{Action, Object}], Redirect, ?WM_REQ(ReqData, Context)).

-spec wm_set_location(Redirect::atom(), #context{}) -> #context{}.
wm_set_location(Redirect, Context) ->
    RequestPath = wrq:raw_path(z_context:get_reqdata(Context)),
    Location = z_dispatcher:url_for(Redirect, [{p,RequestPath}], Context),
    z_context:set_resp_header("Location", Location, Context).

%% Check list of {Action,Object} ACL pairs
-spec wm_is_allowed(acl(), #context{}) -> boolean().
wm_is_allowed([], _Context) ->
    true;
wm_is_allowed([{Action,Object}|ACLs], Context) ->
    case is_allowed(Action, Object, Context) of
        true ->
            wm_is_allowed(ACLs, Context);
        false ->
            %% When the resource doesn't exist then we let the request through
            %% This will enable a 404 response later in the http flow checks.
            case {Action, Object} of
                {view, undefined} -> wm_is_allowed(ACLs, Context);
                {view, false} -> wm_is_allowed(ACLs, Context);
                {view, Id} ->  
                    case m_rsc:exists(Id, Context) of
                        true -> false;
                        false -> wm_is_allowed(ACLs, Context)
                    end;
                _ -> false
            end
    end.
