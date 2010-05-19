%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-05-19
%% @doc Translation support for i18n.  Generates .po files by scanning templates.

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

-module(mod_translation).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Translation").
-mod_description("Generate .po files containing translatable texts by scanning templates.").
-mod_prio(500).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).
-export([event/2, generate/1]).

-include("zotonic.hrl").

-record(state, {context}).

%% @doc Start rescanning all templates for translation tags.
event({postback, translation_generate, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> generate(Context) end),
            z_render:growl("Started building the .po templates. This may take a while.", Context);
        false ->
            z_render:growl_error("Sorry, you don't have permission to scan for translations.", Context)
    end;
event({postback, translation_reload, _TriggerId, _TargetId}, Context) ->
    case z_acl:is_allowed(use, ?MODULE, Context) of
        true ->
            spawn(fun() -> z_trans_server:load_translations(Context) end),
            z_render:growl("Reloading all .po template in the background.", Context);
        false ->
            z_render:growl_error("Sorry, you don't have permission to reload translations.", Context)
    end.

% @doc Generate all .po templates for the given site
generate(#context{} = Context) ->
    translation_po:generate(translation_scan:scan(Context));
generate(Host) when is_atom(Host) ->
    translation_po:generate(translation_scan:scan(z_context:new(Host))).


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

