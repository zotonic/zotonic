%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-07
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
-behaviour(gen_server).

-mod_title("Authentication").
-mod_description("Handles authentication and identification of users.").
-mod_prio(500).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([observe/2]).

-include("zotonic.hrl").

-record(state, {context}).


%% @doc Check the logon event for the Zotonic native username/password registration.
observe({logon_submit, Args}, Context) ->
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
    end;
observe(auth_autologon, Context) ->
    case resource_logon:get_rememberme_cookie(Context) of
        undefined -> undefined;
        {ok, UserId} -> {ok, UserId}
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
    ContextSudo = z_acl:sudo(Context),
    z_notifier:observe(logon_submit, {?MODULE, observe}, Context),
    z_notifier:observe(auth_autologon, {?MODULE, observe}, Context),
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
terminate(_Reason, State) ->
    z_notifier:detach(logon_submit, {?MODULE, observe}, State#state.context),
    z_notifier:detach(auth_autologon, {?MODULE, observe}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

