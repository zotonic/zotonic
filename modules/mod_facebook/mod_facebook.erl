%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-09
%% @doc Facebook integration. Adds Facebook login and other functionalities.

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

-module(mod_facebook).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Facebook").
-mod_description("Adds Facebook login and other Facebook related features.").
-mod_prio(400).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-export([
    observe_auth_logoff/3
]).
-export([get_appid_secret/1]).

-include("zotonic.hrl").

-record(state, {context}).

% Default facebook appid and secret from the Zotonic Dev application.
-define(FACEBOOK_APPID, "106094309435783").
-define(FACEBOOK_APPSECRET, "50fb8d9d1ea8013c4c1640632c3ab706").

%% @doc Reset the received facebook access token (as set in the session)
observe_auth_logoff(auth_logoff, AccContext, _Context) ->
    AccContext1 = case z_context:get_session(facebook_logon, AccContext) of
        true ->
            z_script:add_script(
                        "FB.logout(function() { window.location = '/'; }); setTimeout(function() { window.location='/'; }, 8000)", 
                        AccContext);
        _ ->
            AccContext
    end,
    z_context:set_session(facebook_logon, false, AccContext1),
    z_context:set_session(facebook_access_token, undefined, AccContext1).


%% @doc Return the facebook appid and secret
get_appid_secret(Context) ->
    { z_convert:to_list(m_config:get_value(mod_facebook, appid, ?FACEBOOK_APPID, Context)),
      z_convert:to_list(m_config:get_value(mod_facebook, appsecret, ?FACEBOOK_APPSECRET, Context)) }.


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
    ContextSudo = z_acl:sudo(Context),
    {ok, #state{context=ContextSudo}}.

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
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

            