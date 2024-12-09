%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Handle authentication of zotonic users.  Also shows the logon screen when authentication is required.
%% @end

%% Copyright 2009-2024 Marc Worrell
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
    logon_switch/3,
    logon_redirect/3,
    confirm/2,
    logon_pw/3,
    logoff/1,

    switch_user/2,
    publish_user_session/1,
    unpublish_user_session/1,

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
            Context1 = z_notifier:foldl(#auth_confirm{ id = UserId }, Context, Context),
            z_notifier:notify(#auth_confirm_done{ id = UserId }, Context1),
            {ok, Context1};
        false ->
            {error, user_not_enabled}
    end.

%% @doc Logon a user whose id we know, set all user prefs in the context.
-spec logon( m_rsc:resource_id(), z:context() ) -> {ok, z:context()} | {error, user_not_enabled}.
logon(UserId, Context) ->
    case is_enabled(UserId, Context) of
        true ->
            Context1 = z_acl:logon_prefs(UserId, Context),
            Context2 = z_notifier:foldl(#auth_logon{ id = UserId }, Context1, Context1),
            m_identity:set_visited(UserId, Context2),
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

%% @doc Allow an admin user to switch to another user account.
-spec logon_switch( m_rsc:resource_id(), m_rsc:resource_id(), z:context() ) -> {ok, z:context()} | {error, eacces}.
logon_switch(UserId, SudoUserId, Context) ->
    case m_rsc:exists(UserId, Context) andalso z_acl:is_admin( z_acl:logon(SudoUserId, Context) ) of
        true ->
            Context1 = z_acl:logon_prefs(UserId, Context),
            Context2 = z_notifier:foldl(#auth_logon{ id = UserId }, Context1, Context1),
            {ok, Context2};
        false ->
            {error, eacces}
    end.

%% @doc Logon a user and redirect the user agent. The MQTT websocket MUST be connected.
-spec logon_redirect( m_rsc:resource_id(), binary() | undefined, z:context() ) -> ok | {error, term()}.
logon_redirect(UserId, Url, Context) ->
    case z_notifier:first(#auth_client_logon_user{
            user_id = UserId,
            url = Url
        }, Context)
    of
        undefined -> {error, no_handler};
        ok -> ok;
        {error, _} = Error -> Error
    end.

%% @doc Request the client's auth worker to re-authenticate as a new user
-spec switch_user( m_rsc:resource_id(), z:context() ) -> ok | {error, eacces}.
switch_user(UserId, Context) when is_integer(UserId) ->
    case z_notifier:first(#auth_client_switch_user{
            user_id = UserId
        }, Context)
    of
        undefined -> {error, no_handler};
        ok -> ok;
        {error, _} = Error -> Error
    end.

%% @doc Forget about the user being logged on.
-spec logoff(z:context()) -> z:context().
logoff(Context) ->
    Logoff = #auth_logoff{
        id = z_acl:user(Context)
    },
    Context1 = z_notifier:foldl(Logoff, Context, Context),
    z_acl:logoff(Context1).


%% @doc Publish the current IP address and session-id to the sessions topic
%% of the user. This enables tracking where users are active. This _must_ be
%% a session initiated from a remote IP address. If no remote IP address is
%% known then the user session is not logged.
-spec publish_user_session(Context) -> ok when
    Context :: z:context().
publish_user_session(Context) ->
    case is_auth(Context) of
        true ->
            case z_context:session_id(Context) of
                {ok, SessionId} ->
                    case m_req:get(peer_ip, Context) of
                        undefined ->
                            {error, no_peer_ip};
                        PeerIP ->
                            Peer = z_convert:to_binary(inet:ntoa(PeerIP)),
                            Payload = #{
                                <<"session_id">> => SessionId,
                                <<"user_agent">> => m_req:get(user_agent, Context),
                                <<"ip_address">> => Peer,
                                <<"timestamp">> => calendar:universal_time()
                            },
                            z_mqtt:publish(
                                [ <<"~user">>, <<"sessions">>, SessionId ],
                                Payload,
                                #{ retain => true },
                                Context)
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, no_user}
    end.

%% @doc Remove the retained user session value, this removes the session from the overview
%% of active sessions. Called when resetting the authentication cookies.
-spec unpublish_user_session(Context) -> ok when
    Context :: z:context().
unpublish_user_session(Context) ->
    case is_auth(Context) of
        true ->
            case z_context:session_id(Context) of
                {ok, SessionId} ->
                    z_mqtt:publish(
                        [ <<"~user">>, <<"sessions">>, SessionId ],
                        undefined,
                        #{ retain => true },
                        Context);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, no_user}
    end.


%% @doc Check if the user is enabled, a user is enabled when the rsc is published and within its publication date range.
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


