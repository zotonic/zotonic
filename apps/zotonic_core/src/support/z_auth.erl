%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-24
%%
%% @doc Handle authentication of zotonic users.  Also shows the logon screen when authentication is required.

%% Copyright 2009 Marc Worrell
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

-module(z_auth).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    is_auth/1,

    logon/2,
    logon_switch/2,
    confirm/2,
    logon_pw/3,
    logoff/1,

    switch_user/2,

    is_enabled/2
]).


-include_lib("zotonic.hrl").


%% @doc Check if the visitor has been authenticated. Assumes a completely initalized context.
-spec is_auth(z:context()) -> boolean().
is_auth(#context{ user_id = undefined }) -> false;
is_auth(_) -> true.

%% @doc Logon a username/password combination, checks passwords with m_identity.
-spec logon_pw( binary(), binary(), z:context() ) -> {boolean(), z:context()}.
logon_pw(Username, Password, Context) ->
    case m_identity:check_username_pw(Username, Password, Context) of
        {ok, Id} ->
            case logon(Id, Context) of
                {ok, Context1} -> Context1;
                {error, _Reason} -> {false, Context}
            end;
        {error, _Reason} -> {false, Context}
    end.


%% @doc Set the user to 'confirmed'.
-spec confirm( m_rsc:resource_id(), z:context() ) -> {ok, z:context()} | {error, user_not_enabled}.
confirm(UserId, Context) ->
    % check if auth_user_id == userId??
    case is_enabled(UserId, Context) of
        true ->
            Context1 = z_notifier:foldl(#auth_confirm{}, Context, Context),
            z_notifier:notify(#auth_confirm_done{}, Context1),
            {ok, Context1};
        false ->
            {error, user_not_enabled}
    end.

%% @doc Logon an user whose id we know, invalidate the current session id.
%%      This sets a cookie with the new session id in the Context.
-spec logon( m_rsc:resource_id(), z:context() ) -> {ok, z:context()} | {error, user_not_enabled}.
logon(UserId, Context) ->
    case is_enabled(UserId, Context) of
        true ->
            Context1 = z_acl:logon_prefs(UserId, Context),
            Context2 = z_notifier:foldl(#auth_logon{ id = UserId }, Context1, Context1),
            {ok, Context2};
        false ->
            {error, user_not_enabled}
    end.

%% @doc Allow an admin user to switch to another user account.
-spec logon_switch( m_rsc:resource_id(), z:context() ) -> {ok, z:context()} | {error, eacces}.
logon_switch(UserId, Context) ->
    case m_rsc:exists(UserId, Context) andalso z_acl:is_admin(Context) of
        true ->
            Context1 = z_acl:logon_prefs(UserId, Context),
            Context2 = z_notifier:foldl(#auth_logon{ id = UserId }, Context1, Context1),
            {ok, Context2};
        false ->
            {error, eacces}
    end.


%% @doc Request the client's auth worker to re-authenticate as a new user
-spec switch_user( m_rsc:resource_id(), z:context() ) -> ok | {error, eacces}.
switch_user(1, _Context) ->
    {error, eacces};
switch_user(UserId, Context) when is_integer(UserId) ->
    case z_acl:is_admin(Context) of
        true ->
            z_mqtt:publish(
                    [ <<"~client">>, <<"model">>, <<"auth">>, <<"post">>, <<"switch-user">> ],
                    #{ user_id => UserId },
                    Context),
            ok;
        false ->
            {error, eacces}
    end.


%% @doc Forget about the user being logged on.
-spec logoff(z:context()) -> z:context().
logoff(Context) ->
    Logoff = #auth_logoff{
        id = z_acl:user(Context)
    },
    Context1 = z_notifier:foldl(Logoff, Context, Context),
    z_acl:logoff(Context1).


%% @doc Check if the user is enabled, an user is enabled when the rsc is published and within its publication date range.
-spec is_enabled( m_rsc:resource_id(), z:context() ) -> boolean().
is_enabled(UserId, Context) ->
    case z_notifier:first(#user_is_enabled{id=UserId}, Context) of
        undefined ->
            Acl = m_rsc:get_acl_props(UserId, Context),
            case Acl#acl_props.is_published of
                false ->
                    false;
                true ->
                    Date = calendar:universal_time(),
                    Acl#acl_props.publication_start =< Date andalso Acl#acl_props.publication_end >= Date
            end;
        Other when is_boolean(Other) ->
            Other
    end.


