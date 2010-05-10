%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-24
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
    is_auth_recent/1,
    
    logon/2,
    logon_pw/3,
    logoff/1,
    logon_from_session/1    
]).

-include_lib("zotonic.hrl").


%% @doc Check if the visitor has been authenticated. Assumes a completely initalized context.
%% @spec is_auth(#context) -> bool()
is_auth(#context{user_id=undefined}) ->
    false;
is_auth(_) ->
    true.

is_auth_recent(#context{user_id=undefined}) ->
    false;
is_auth_recent(_) ->
    true.


%% @doc Logon a username/password combination, checks passwords with m_identity.
%% @spec logon_pw(Username, Password, Context) -> {bool(), NewContext}
logon_pw(Username, Password, Context) ->
    case m_identity:check_username_pw(Username, Password, Context) of
        {ok, Id} -> {true, logon(Id, Context)};
        {error, _Reason} -> {false, Context}
    end.


%% @doc Logon an user whose id we know
logon(UserId, Context) ->
    Context1 = z_acl:logon(UserId, Context),
	Context2 = z_session_manager:rename_session(Context1),
    z_context:set_session(auth_user_id, UserId, Context2),
    z_context:set_session(auth_timestamp, calendar:universal_time(), Context2),
    z_notifier:notify(auth_logon, Context),
    Context2.


%% @doc Forget about the user being logged on.
%% @spec logoff(Context) -> NewContext
logoff(Context) ->
    ContextLogOff = z_notifier:foldl(auth_logoff, Context, Context),
    z_context:set_session(auth_user_id, none, ContextLogOff),
    z_acl:logoff(ContextLogOff).
    
    
%% @doc Called after z_context:ensure_session. 
%% Check if the session contains an authenticated user id. 
%% When found then the user_id of the context is set.
%% Also checks any automatic logon methods like "remember me" cookies.
%% @spec logon_from_session(#context) -> #context
logon_from_session(Context) ->
    case z_context:get_session(auth_user_id, Context) of
        none ->
        	z_memo:set_userid(undefined),
            Context;
        undefined ->
            % New session, check if some module wants to log on
        	case z_notifier:first(auth_autologon, Context) of
        	    undefined -> 
            	    z_memo:set_userid(undefined),
        	        z_context:set_session(auth_user_id, none, Context);
        	    {ok, UserId} ->
            	    z_memo:set_userid(UserId),
                    logon(UserId, Context)
            end;
        UserId ->
        	z_memo:set_userid(UserId),
            z_acl:logon(UserId, Context)
    end.


