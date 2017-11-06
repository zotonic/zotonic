%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%%
%% @doc Terminal notifications, shows text messages on the developer's screen.

%% Copyright 2017 Marc Worrell
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


-module(zotonic_filehandler_terminal).

-behaviour(gen_server).

-export([
    notify/1
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").

-record(state, {}).


-spec notify( string() | binary() | undefined ) -> ok.
notify(undefined) ->
    ok;
notify(Message) ->
    gen_server:cast(?MODULE, {notify, z_string:trim(Message)}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ------------------------------------- gen_server callbacks ------------------------------

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_msg}, State}.

handle_cast({notify, Message}, State) ->
    % TODO: drop messages if there are too many queued
    terminal_notifier(os:type(), z_string:trim(Message)),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ------------------------------------- Internal functions -------------------------------------


terminal_notifier(_OS, <<>>) ->
    ok;
terminal_notifier(_OS, "") ->
    ok;
terminal_notifier({unix, darwin}, Msg) ->
    exec:run("which terminal-notifier && terminal-notifier -group zotonic -title Zotonic -message " ++ z_utils:os_escape(Msg), []);
terminal_notifier({unix, _Arch}, Msg) ->
    exec:run("which notify-send && notify-send -t 2000 \"Zotonic\" " ++ z_utils:os_escape(Msg), []);
terminal_notifier(_OS, _Msg) ->
    ok.

