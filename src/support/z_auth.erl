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
    output_logon/1,
    
    logon_pw/3,
    logoff/1,
    logon_from_session/1,
    
    wm_is_authorized/2,
    wm_is_authorized/5,
    wm_output_logon/1,
    
    event/2
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
        {ok, Id} ->
            Context1 = z_acl:logon(Id, Context),
            z_context:set_session(auth_user_id, Id, Context1),
            z_context:set_session(auth_timestamp, calendar:universal_time(), Context1),
            {true, Context1};
        {error, _Reason} ->
            {false, Context}
    end.


%% @doc Forget about the user being logged on.
%% @spec logoff(Context) -> NewContext
logoff(Context) ->
    z_context:set_session(auth_user_id, undefined, Context),
    z_acl:logoff(Context).
    
    
%% @doc Check if the session contains an authenticated user id. Called after z_context:ensure_session. When found
%% then the user_id of the context is set.
%% @spec logon_from_session(#context) -> #context
logon_from_session(Context) ->
	% Enable simple memo functionality
	AuthUserId = z_context:get_session(auth_user_id, Context),
	z_memo:set_userid(AuthUserId),
	% When there is an user, perform the user logon
    case AuthUserId of
        undefined -> Context;
        UserId -> z_acl:logon(UserId, Context)
    end.


%% @doc Convenience function to be called from the is_authorized/2 callback in webmachine resources.
wm_is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1), 
    case Context2#context.user_id of
        undefined -> 
            ContextLogon = output_logon(Context2), 
            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);
        _ -> 
            ?WM_REPLY(true, Context2)
    end.    


%% @doc Convenience function to be called from the is_authorized/2 callback in webmachine resources.
%% The ArgName is the name of the id in the query string or other parameters.  When it is an atom
%% then the context is checked before the query args are checked.
wm_is_authorized(NeedAuth, What, ArgName, ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1), 
    case NeedAuth andalso Context2#context.user_id == undefined of
        true -> 
            ContextLogon = output_logon(Context2), 
            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);

        false -> 
            Id = case ArgName of
                N when is_integer(N) ->
                    N;
                A when is_atom(A) -> 
                    case z_context:get(A, Context2) of
                        undefined ->
                            z_convert:to_integer(z_context:get_q(A, Context2));
                        FromDisp ->
                            case m_rsc:name_to_id(FromDisp, Context2) of
                                {ok, FromDispId} -> FromDispId;
                                {error, _Reason} -> undefined
                            end
                    end;
                _ ->
                    z_convert:to_integer(z_context:get_q(ArgName, Context2))
            end,
            case m_rsc:exists(Id, Context2) of
                false -> 
                    ?WM_REPLY(true, Context2);
                true ->
                    Allow = case What of
                        visible -> z_acl:rsc_visible(Id, Context2);
                        editable -> z_acl:rsc_editable(Id, Context2)
                    end,
                    case Allow of
                        false ->
                            ContextLogon = output_logon(Context2), 
                            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);
                        true ->  
                            ?WM_REPLY(true, Context2)
                    end
            end
    end.


%% @doc Render the logon screen and return the www_authenticate response for webmachine.
%% @spec wm_output_logon(Context) -> {bool(), ReqData, NewContext}
wm_output_logon(Context) ->
    ContextLogon = output_logon(Context), 
    ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon).


%% @doc Render the logon screen to the reqdata of the context.
%% @spec output_logon(Context) -> LogonContext
output_logon(Context) ->
    Html = z_template:render("admin_logon.tpl", [], Context),
    {Data, ContextOut} = z_context:output(Html, Context),
    RD1 = z_context:get_reqdata(ContextOut),
    RD2 = wrq:append_to_resp_body(Data, RD1),
    RD3 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD2),
    z_context:set_reqdata(RD3, ContextOut).



%% @doc Handle logon events. When successful then reload the current page
%% @spec event(Event, Context) -> NewContext
event({submit, logon, _TriggerId, _TargetId}, Context) ->
    Context1 = z_session_manager:rename_session(Context),
    Username = z_context:get_q("zp-username", Context1),
    Password = z_context:get_q("zp-password", Context1),
    case logon_pw(Username, Password, Context1) of
        {true, ContextLogon} -> 
            z_render:wire({reload, []}, ContextLogon);
        {_, ContextLogon} ->
            z_render:growl("Unknown username or password. Please try again.", ContextLogon)
    end.
    
