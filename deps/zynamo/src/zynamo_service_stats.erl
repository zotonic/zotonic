%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%%
%% @doc Zynamo stats, accessible as a service

%% Copyright 2011 Marc Worrell
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

-module(zynamo_service_stats).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
]).

-include_lib("zynamo.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> 
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
-spec init(list()) -> {ok, undefined}.
init(_Args) ->
    zynamo_manager:set_service(zynamo, stats, self(), undefined),
    {ok, #state{}}.


handle_call({handoff_check, _Node}, _From, State) ->
    {reply, {ok, done}, State};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(#zynamo_service_command{
                command=Command
            }=SC, State) ->
    #zynamo_command{command=Cmd, key=Key} = Command,
    Reply = case Cmd of
                get -> collect_stats(Key);
                _ -> {error, operation_not_supported}
            end,
    zynamo_request:reply(Reply, SC),
    {noreply, State};

handle_cast({handoff_done, _Node, _Command}, State) ->
    {norepy, State};
    
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, _NewVersion, State) ->
    {ok, State}.


collect_stats(K) when not is_list(K) ->
    collect_stats([K]);
collect_stats(Ks) ->
    StatKeys = lists:flatten([ statz:match(K) || K <- Ks ]),
    lists:foldl(
        fun(S, Acc) ->
            case statz:summary(S) of
                {ok, Summary} -> [{S, Summary}|Acc];
                _ -> Acc
            end
        end,
        [],
        StatKeys).

