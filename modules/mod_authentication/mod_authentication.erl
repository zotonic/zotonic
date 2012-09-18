%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-05-07
%% @doc Authentication and identification of users.

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

-module(mod_authentication).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Authentication").
-mod_description("Handles authentication and identification of users.").
-mod_prio(500).
-mod_depends([base, acl]).
-mod_provides([authentication]).

%% gen_server exports
-export([
    observe_logon_submit/2,
    observe_auth_autologon/2,
	observe_auth_logoff_done/2,
	observe_auth_logon_done/2
]).

-include("zotonic.hrl").

%% @doc Check the logon event for the Zotonic native username/password registration.
observe_logon_submit(#logon_submit{query_args=Args}, Context) ->
    Username = proplists:get_value("username", Args),
    Password = proplists:get_value("password", Args),
    case Username /= undefined andalso Password /= undefined of
        true ->
            case m_identity:check_username_pw(Username, Password, Context) of
                {ok, Id} ->
                    case Password of
                        [] ->
                            %% When empty password existed in identity table, prompt for a new password.
                            %% FIXME do real password expiration here.
                            {expired, Id};
                        _ -> {ok, Id}
                    end;
                E -> E
            end;
        false ->
            undefined
    end.
    
observe_auth_autologon(auth_autologon, Context) ->
    case controller_logon:get_rememberme_cookie(Context) of
        undefined -> undefined;
        {ok, UserId} -> {ok, UserId}
    end.

observe_auth_logoff_done(auth_logoff_done, Context) ->
	reload_other_pages(Context).

observe_auth_logon_done(auth_logon_done, Context) ->
	reload_other_pages(Context).

reload_other_pages(Context) ->
	Pages = z_session:get_pages(Context),
	ContextPush = z_render:wire({reload, []}, z_context:new(Context)),
	{Script, _} = z_script:split(ContextPush),
	[ z_session_page:add_script(Script, Pid) || Pid <- Pages, Pid /= Context#context.page_pid ].
