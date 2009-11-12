%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-02
%% @doc Support for editing groups in the admin module.  Also hooks into the rsc update function to
%% save the specific fields for groups and to manage group membership.

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

-module(mod_admin_group).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Admin group support").
-mod_description("Adds support for editing groups and group membership to the admin.").
-mod_prio(600).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    rsc_update/3
]).

-include_lib("zotonic.hrl").

-record(state, {context}).


%% @doc Check if the update contains information for a group.  If so then update
%% the group information in the db and remove it from the update props.
%% @spec rsc_update({rsc_update, ResourceId, OldResourceProps}, {DidUpdate, UpdateProps}, Context) -> {NewDidUpdate, NewUpdateProps}
rsc_update({rsc_update, Id, _OldProps}, {Changed, Props}, Context) ->
    case       proplists:is_defined(group_is_admin, Props) 
        orelse proplists:is_defined(group_is_supervisor, Props)
        orelse proplists:is_defined(group_is_community_publisher, Props)
        orelse proplists:is_defined(group_is_public_publisher, Props) of

        true ->
            %% Should check if this updates the props, but for now we take the simple approach.
            m_group:update_noflush(Id, Props, Context),

            % Remove all the group props
            Props1 = proplists:delete(group_is_admin, 
                        proplists:delete(group_is_supervisor, 
                            proplists:delete(group_is_community_publisher, 
                                proplists:delete(group_is_public_publisher, Props)))),
            {true, Props1};
        false ->
            {Changed, Props}
    end.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
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
    z_notifier:observe(rsc_update,   {?MODULE, rsc_update}, Context),
    {ok, #state{context=z_context:new(Context)}}.


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


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
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
    z_notifier:detach(rsc_update, {?MODULE, rsc_update}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

