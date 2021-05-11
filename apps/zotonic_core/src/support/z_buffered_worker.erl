%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc A worker which gets its jobs from an ets buffer.

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(z_buffered_worker).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

%% Api exports

-export([
    start_link/3,
    stop/1,
    push/2,
    flush/1
]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {
    name,
    handler,
    timeout,
    handler_state
}).

-record(counter, {
   name,
   value = 0
}).

-record(entry, {
   count,
   value
}).

%%
%% Api
%%

%% @doc Start the worker, the handler module will be called when new data entries are pushed in the buffer.
%%
%% The server calls Handler:init(pid(), Args) -> {ok, Timeout, HandlerState} when it is started.
%% Timeout will be used as flush interval.
%%
-spec start_link( atom(), module(), term() ) -> {ok, pid()} | {error, term()}.
start_link(Name, Handler, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Handler, Args], []).

%% @doc Stop the worker, all pending work will be lost.
-spec stop( atom() ) -> ok.
stop(Name) ->
    gen_server:call(Name, stop).

%% @doc Push a message to the named worker, when the worker is flushed the pushed messages
%% are handled in-order.
-spec push( atom(), term() ) -> true.
push(Name, Msg) ->
    ets:insert(Name, #entry{count=get_next(Name), value=Msg}).

%% @doc Flush the worker,
-spec flush( atom() ) -> ok | {error, badarg}.
flush(Name) ->
    case whereis(Name) of
        undefined -> {error, badarg};
        Pid when is_pid(Pid) -> Pid ! flush
    end.

%%
%% gen_server callbacks
%%

init([Name, Handler, Args]) ->
    Name = ets:new(Name, [ordered_set, named_table, public,
            {keypos, 2},
            {write_concurrency, true}]),
    ets:insert(Name, #counter{name=next}),
    {ok, Timeout, HandlerState} = Handler:init(self(), Args),
    erlang:send_after(Timeout, self(), flush),
    {ok, #state{name=Name, handler=Handler, timeout=Timeout, handler_state=HandlerState}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush, #state{name=Name, handler=Handler, handler_state=HandlerState, timeout=Timeout}=State) ->
    [#counter{value=Upto}] = ets:lookup(Name, next),
    flush(Name, Upto, Handler, HandlerState),
    Handler:handle_flush_done(self(), HandlerState),
    erlang:send_after(Timeout, self(), flush),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Helpers
%%


% @doc Return a sequence number for the next element
%
-spec get_next(atom()) -> integer().
get_next(Name) ->
    ets:update_counter(Name, next, 1).

% @doc Flush the buffer upto the given value.
%
flush(Name, Upto, Handler, HandlerState) ->
    flush(Name, Upto, Handler, HandlerState, ets:first(Name)).

flush(_Name, _Upto, _Handler, _HandlerState, '$end_of_table') ->
    ok;
flush(Name, Upto, Handler, HandlerState, Key) when is_integer(Key) andalso Key =< Upto ->
    [#entry{count=Count, value=Value}] = ets:lookup(Name, Key),
    true = ets:delete(Name, Key),
    Handler:handle_value(self(), Count, Value, HandlerState),
    flush(Name, Upto, Handler, HandlerState, ets:next(Name, Key));
flush(Name, Upto, Handler, HandlerState, Atom) when is_atom(Atom) ->
    % Skip over counters
    flush(Name, Upto, Handler, HandlerState, ets:next(Name, Atom));
flush(_Name, _Upto, _Handler, _HandlerState, _Key) ->
    ok.

