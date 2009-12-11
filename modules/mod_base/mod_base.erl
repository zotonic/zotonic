%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-08
%% @doc The base module, implementing basic Zotonic scomps, actions, models and validators.

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

-module(mod_base).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Zotonic Base").
-mod_description("Base supplies all basic scomps, actions and validators.").
-mod_prio(9999).

-include_lib("zotonic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
	media_stillimage/2
]).

-record(state, {context}).

%% @doc Return the filename of a still image to be used for image tags.
%% @spec media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
media_stillimage({media_stillimage, Props}, Context) ->
    Mime = proplists:get_value(mime, Props),
	case z_media_preview:can_generate_preview(Mime) of
		true ->
			%% Let media preview handle this.
			undefined;
		false ->
			%% Serve an existing icon.
			[A,B] = string:tokens(z_convert:to_list(Mime), "/"),
			case z_module_indexer:find(lib, "images/mimeicons/"++A++[$-,B]++".png", Context) of
				{ok, File} -> {ok, File};
				{error, enoent} ->
					case z_module_indexer:find(lib, "images/mimeicons/"++A++".png", Context) of
						{ok, File} -> {ok, File};
						{error, enoent} -> {ok, "lib/images/mimeicons/application-octet-stream.png"}
					end
			end
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
    z_notifier:observe(media_stillimage, {?MODULE, media_stillimage}, Context),
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
    z_notifier:detach(media_stillimage, {?MODULE, media_stillimage}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

