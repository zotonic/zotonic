%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014-2017 Arjan Scherpenisse
%%
%% @doc Handle changed files

%% Copyright 2014-2017 Arjan Scherpenisse
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

-module(zotonic_filewatcher_handler).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-behaviour(gen_server).

-export([
    file_changed/2,
    re_exclude/0,
    is_file_blacklisted/1,
    is_file_blacklisted/2
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
-include_lib("zotonic_filewatcher.hrl").

-record(state, {
    events = [] :: map(),  % binary() => verb()
    startline = undefined :: undefined | pos_integer(),
    deadline = undefined :: undefined | pos_integer(),
    is_changed = false :: boolean()
}).


%% Which files do we not consider at all in the file_changed handler
-define(FILENAME_BLACKLIST_RE,
        "_flymake$"
        "|node_modules"
        "|\\.#"
        "|\\.bea#"
        "|/priv/files/"
        "|/\\.git/"
        "|/\\.gitignore"
        "|\\.hg/"
        "|/log/"
        "|\\.log$"
        "|\\.dump$"
        "|/mnesia/"
        "|/\\.rebar3"
        "|\\.DS_Store").


-define(TIMER_DELAY, 300).
-define(DEADLINE_DELAY, 250).
-define(MAX_DELAY, 5000).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Called when a file is changed on disk, buffers changes before sending
%%      a batch to the zotonic_filehandler.
-spec file_changed(zotonic_filehandler:verb(), file:filename_all()) -> ok.
file_changed(Verb, F) when is_list(F) ->
    file_changed(Verb, unicode:characters_to_binary(F));
file_changed(Verb, F) when is_binary(F) ->
    lager:debug("[filewatcher] ~p of '~s'", [Verb, F]),
    case is_file_blacklisted(F) of
        true ->
            ok;
        false ->
            gen_server:cast(?MODULE, {file_changed, Verb, F})
    end.

-spec re_exclude() -> string().
re_exclude() ->
    ?FILENAME_BLACKLIST_RE.

-spec is_file_blacklisted(binary()|string()) -> boolean().
is_file_blacklisted(<<".", _/binary>>) ->
    true;
is_file_blacklisted(F) when is_binary(F) ->
    case binary:last(F) of
        $# -> true;
        _ -> re:run(F, re_compiled()) =/= nomatch
    end;
is_file_blacklisted("." ++ _) ->
    true;
is_file_blacklisted(F) when is_list(F) ->
    case lists:last(F) of
        $# -> true;
        _ -> re:run(F, re_compiled()) =/= nomatch
    end.

re_compiled() ->
    case erlang:get(blacklist_re) of
        undefined ->
            {ok, RE} = re:compile(?FILENAME_BLACKLIST_RE),
            erlang:put(blacklist_re, RE),
            RE;
        RE ->
            RE
    end.

%% @doc Called by zotonic_filewatcher_handler
is_file_blacklisted("priv", "files") -> true;
is_file_blacklisted("priv", "mnesia") -> true;
is_file_blacklisted("priv", "log") -> true;
is_file_blacklisted(<<"priv">>, <<"files">>) -> true;
is_file_blacklisted(<<"priv">>, <<"mnesia">>) -> true;
is_file_blacklisted(<<"priv">>, <<"log">>) -> true;
is_file_blacklisted(_Dir, "." ++ _) -> true;
is_file_blacklisted(_Dir, <<".", _/binary>>) -> true;
is_file_blacklisted(_Dir, File) -> is_file_blacklisted(File).


%% ------------------------------ gen_server Callbacks -----------------------------

-spec init(term()) -> {ok, #state{}}.
init(_) ->
    {ok, #state{
        events = #{},
        is_changed = false
    }}.

handle_call(Msg, _From, State) ->
    {stop, {unknown_msg, Msg}, State}.

handle_cast({file_changed, Verb, F}, #state{ events = Es } = State) ->
    State1 = State#state{ events = add_event(Es, Verb, F) },
    State2 = set_timer(State1),
    {noreply, State2};

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(maybe_send, #state{ is_changed = false } = State) ->
    {noreply, State};
handle_info(maybe_send, #state{ deadline = Deadline } = State) ->
    Now = msec(),
    case Deadline > Now of
        true ->
            {noreply, State};
        false ->
            send_changes(State#state.events),
            State1 = State#state{
                events = #{},
                is_changed = false
            },
            {noreply, State1}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% ------------------------------ Internal Functions -----------------------------

-spec send_changes(map()) -> ok.
send_changes(Es) ->
    Msg = #filewatcher_changes{
        changes = Es
    },
    zotonic_notifier:first(?SYSTEM_NOTIFIER, filewatcher_changes, Msg, undefined).

-spec add_event(map(), zotonic_filehandler:verb(), binary()) -> map().
add_event(Es, Verb, Filename) ->
    OldVerb = maps:get(Filename, Es, undefined),
    Es#{ Filename => select_verb(Verb, OldVerb) }.


%% @doc Select the verb to be passed if there are multiple updates to a file.
-spec select_verb(NewVerb, PrevVerb) -> Verb
    when NewVerb :: zotonic_filehandler:verb(),
         PrevVerb :: zotonic_filehandler:verb() | undefined,
         Verb :: zotonic_filehandler:verb().
select_verb(Verb, undefined) -> Verb;
select_verb(delete, _Verb) -> delete;
select_verb(create, _Verb) -> create;
select_verb(modify, Verb) -> Verb.



set_timer(#state{ is_changed = false } = State) ->
    erlang:send_after(?TIMER_DELAY, self(), maybe_send),
    Now = msec(),
    State#state{
        startline = Now,
        deadline = Now + ?DEADLINE_DELAY,
        is_changed = true
    };
set_timer(#state{ startline = Startline } = State) ->
    erlang:send_after(?TIMER_DELAY, self(), maybe_send),
    Now = msec(),
    CurrentDelay = Now - Startline,
    if
        CurrentDelay > ?MAX_DELAY ->
            State#state{
                is_changed = true
            };
        true ->
            State#state{
                deadline = Now + ?DEADLINE_DELAY,
                is_changed = true
            }
    end.

msec() ->
    {Mega, Secs, Micro} = os:timestamp(),
    ((Mega * 1000000) + Secs) * 1000 + (Micro div 1000).


