%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Signal and slot mechanism for use in templates.

%% Copyright 2010 Maas-Maarten Zeeman
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

-module(mod_signal).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-behaviour(gen_server).

-mod_title("Signal and Slots").
-mod_author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").
-mod_description("Signal and slot mechanism for use in templates.").
-mod_prio(500).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, stop/1]).

-include("zotonic.hrl").

-export([connect/3, disconnect/3, emit/2, emit_script/2, emit_script/3, slots/2, item_count/1, slot_count/1, emit_signal/3]).

% export for the tests
-export([key/1, key/2]).

-define(SLOT_TABLE, slots).

-record(state, {slots}).


% @doc Connect signal to slot
%
connect(Signal, Slot, Context) ->
    gen_server:call(name(Context), {'connect', Signal, Slot}).

% @doc Disconnect signal and slot
%
disconnect(Signal, Slot, Context) ->
    gen_server:call(name(Context), {'disconnect', Signal, Slot}),
    disconnect_slot(Slot).

disconnect_slot(Slot) when is_pid(Slot) ->
    Slot ! disconnected;
disconnect_slot(_Slot) ->
    ok.

% @doc Emit the signal. Calls the connected slots..
%
emit(Signal, Context) ->
    Slots = slots(Signal, Context),
    AsyncContext = z_context:prune_for_async(Context),
    lists:foreach(fun(Slot) -> 
              try
                  emit_signal(Signal, Slot, AsyncContext) 
              catch M:E ->
                  ?ERROR("Error emitting signal %p to slot %p. %p:%p. Disconnecting...", [Signal, Slot, M, E]),
                  disconnect(Signal, Slot, AsyncContext)
              end
          end, Slots).

emit_script(Signal, Context) ->
    {Scripts, CleanContext} = z_script:split(Context),
    emit_script(Signal, Scripts, CleanContext).
    
emit_script(Signal, Script, Context) ->
    Slots = slots(Signal, Context),
    lists:foreach(fun(Slot) -> 
            try
                emit_signal_script(Script, Slot) 
            catch M:E ->
                ?ERROR("Error emitting signal %p to slot %p. %p:%p. Disconnecting...", [Signal, Slot, M, E]),
                disconnect(Signal, Slot, z_context:prune_for_async(Context))
            end
        end, Slots).


% @doc Emit a single signal
%
emit_signal(Signal, Slot, Context) when is_function(Slot, 2) ->
    Slot(Signal, Context);
emit_signal(Signal, Slot, Context) when is_pid(Slot) ->
    Slot ! {signal, Signal, Context}.

emit_signal_script(_Script, Slot) when is_function(Slot, 2) ->
    nop;
emit_signal_script(Script, Slot) when is_pid(Slot) ->
    Slot ! {script, Script}.


% @doc Return the slots connected to this signal
%
slots(Signal, Context) ->
    Table = slot_table_name(Context),
    SignalType = signal_type(Signal),

    % collect the tags
    Tags = ets:lookup(Table, SignalType),
    
    % Get the list of keys we have to 
    Keys = [key(Signal, Tag) || {tags, _S, Tag} <- Tags],

    slots1([], Table, Keys).

    slots1(Slots, _Table, []) ->
        [Slot || {slot, _K, Slot} <- lists:flatten(Slots)];
    slots1(Slots, Table, [undefined|T]) ->
        slots1(Slots, Table, T);
    slots1(Slots, Table, [H|T]) ->
        slots1([ets:lookup(Table, H) | Slots], Table, T).

% @doc Return how many items there 
%
item_count(Context) ->
    case ets:info(slot_table_name(Context)) of 
        undefined -> 0;
        Info ->
            proplists:get_value(size, Info, 0)
    end.

% @doc Return how many slots there are connected
%
slot_count(Context) ->
    ets:select_count(slot_table_name(Context), [{{slot, '$1', '_'}, [], [true]}]).

% @doc Stop the module..
%
stop(Context) ->
    gen_server:call(name(Context), stop).

% @doc
%
start_link(Args) when is_list(Args) ->
    Host = proplists:get_value(host, Args),

    % Create the table in the supervisor and make it an heir...
    Name = name(?MODULE, Host),
    SlotTable = ensure_slot_table(Name),

    %% Give away the ownership of the table to the module...
    %
    case gen_server:start_link({local, Name}, ?MODULE, Args, []) of
        {ok, P} ->
            ets:give_away(SlotTable, P, slot_table),
            {ok, P};
        {already_started, P} ->
            ets:give_away(SlotTable, P, slot_table),
            {already_started, P};
        R -> R
    end.

%% Gen server stuff.
init(_Args) ->
    {ok, #state{slots=[]}}.

handle_call({'connect', Signal, Slot}, _From, State) ->
    Key = {SignalType, SignalProps} = key(Signal),

    SignalTags = proplists:get_keys(SignalProps),

    TagSet = ets:lookup(State#state.slots, SignalType),
    
    Objects = case lists:member({tags, SignalType, SignalTags}, TagSet) of
                  true ->
                      {slot, Key, Slot};
                  false ->
                      [{slot, Key, Slot}, {tags, SignalType, SignalTags}]
              end,
                  
    % Important, this is one atomic operation! The table is read
    % asynchronously, so it must be updated in one operation.
    ets:insert(State#state.slots, Objects),

    {reply, ok, State};

handle_call({'disconnect', Signal, Slot}, _From, State) ->
    Key = key(Signal),
    ets:delete_object(State#state.slots, {slot, Key, Slot}),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info({'ETS-TRANSFER', Table, _FromPid, slot_table}, State) ->
    {noreply, State#state{slots=Table}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, State) ->
    ets:delete(State#state.slots),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Support functions

% Return the name of this module
%
name(Context) ->
    name(?MODULE, Context#context.host).
name(Module, Host) ->
    z_utils:name_for_host(Module, Host).

% Return the name of the slot table
%
slot_table_name(Name) when is_atom(Name) ->
    z_utils:name_for_host(?SLOT_TABLE, Name);
slot_table_name(Context) ->
    slot_table_name(name(Context)).

% Make sure there is a slot_table. 
%
ensure_slot_table(Name) ->
    SlotTable = slot_table_name(Name),
    case ets:info(SlotTable) of
        undefined ->
            ets:new(SlotTable, [named_table, bag, {keypos, 2}, protected, {heir, self(), []}]);
        _ ->
            SlotTable
    end.

% get the type of signal
%
signal_type(SignalType) when is_atom(SignalType) ->
    SignalType;
signal_type({SignalType, Props}) when is_atom(SignalType), is_list(Props) ->
    SignalType.

% Return the key used for this signal..
%
key(SignalType) when is_atom(SignalType) ->
    {SignalType, []};
key(Signal={SignalType, Props}) when is_atom(SignalType), is_list(Props) ->
    key(Signal, proplists:get_keys(Props)). 

key(SignalType, []) when is_atom(SignalType) ->
    {SignalType, []};
key({SignalType, Props}, []) when is_atom(SignalType), is_list(Props) ->
    {SignalType, []};
key({SignalType, Props}, Tags) when is_atom(SignalType), is_list(Props), is_list(Tags) ->
    case key_props([], Props, lists:sort(Tags)) of
        undefined -> undefined;
        KeyProps ->
            {SignalType, lists:reverse(KeyProps)}
    end.

    key_props(undefined, _Props, _Tags) ->
        undefined;
    key_props(Acc, _Props, []) ->
        Acc;
    key_props(Acc, Props, [H | T]) ->
        case proplists:lookup(H, Props) of
            none -> undefined;
            Item -> key_props([Item | Acc], Props, T)
        end.
