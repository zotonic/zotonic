%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-10-12

%% @doc Automatically convert project's SCSS files to CSS.

%% Copyright 2011 Arjan Scherpenisse
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

-module(z_sass_converter).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).


%% interface functions
-export([
]).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Context=#context{}) ->
    case os:cmd("which sass") of
        [] ->
            {error, "sass not found"};
        _ ->
            Name = z_utils:name_for_host(?MODULE, z_context:site(Context)),
            gen_server:start_link({local, Name}, ?MODULE, Context, [])
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Context) ->
    process_flag(trap_exit, true),
    OutPath = filename:join(z_path:site_dir(Context), "lib/css"),

    SASSPath = filename:join(z_path:site_dir(Context), "lib/sass"),
    Path0 = case filelib:is_dir(SASSPath) of
               true -> SASSPath;
               false -> undefined
           end,
    SCSSPath = filename:join(z_path:site_dir(Context), "lib/scss"),

    Path1 = case filelib:is_dir(SCSSPath) of
                true -> SCSSPath;
                false -> Path0
            end,

    case Path1 of 
        undefined -> ignore;
        _ ->
            Cmd = "sass --watch " ++ Path1 ++ ":" ++ OutPath,
            ?DEBUG("Starting sass: " ++ Cmd),
            Port = erlang:open_port({spawn, Cmd}, [stream, exit_status]),
            {ok, {Context, Port}}
    end.

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
    ?DEBUG(_Info),
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, {_, Port}) ->
    ?DEBUG(Port),

    true = erlang:port_close(Port),
    ?DEBUG("closed"),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

