%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%%
%% @doc Simple in-memory k/v store for zynamo. Every zynamo node has this service enabled.

%% Copyright 2010-2011 Marc Worrell
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

-module(zynamo_kv).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
]).

-include_lib("zynamo.hrl").

-record(state, {data, handoff, local}).
-record(data, {key, version, is_gone, bucket_nr, value}).


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
    zynamo_manager:set_service(zynamo, kv, self(), undefined),
    {ok, #state{
            data=ets:new(zynamo_kv, [ordered_set,protected]),
            local=ets:new(zynamo_local, [set, protected]),
            handoff=ets:new(zynamo_handoff, [set,protected])
    }}.


handle_call({handoff_check, Node}, _From, State) ->
    do_handoff_check(Node, State);

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(#zynamo_service_command{
                command=Command,
                handoff=Handoff
            }=SC, State) ->
    Reply = case Command#zynamo_command.command of
                get ->
                    do_get(Command, State);
                list ->
                    do_list(Command, State);
                Upd when Upd =:= put; Upd =:= delete ->
                    do_update(Command, Handoff, State);
                _Other ->
                    {error, operation_not_supported}
            end,
    zynamo_request:reply(Reply, SC),
    {noreply, State};

handle_cast({handoff_done, Node, Command}, State) ->
    do_handoff_done(Node, Command, State),
    {noreply, State};

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVersion, _NewVersion, State) ->
    {ok, State}.


%% ====================================================================
%% internal support routines
%% ====================================================================


do_get(#zynamo_command{key=Key}, State) ->
    case ets:lookup(State#state.data, Key) of
        [] -> {ok, not_found, undefined};
        [{_Key, #data{is_gone=false, version=Vers, value=Val}}] -> {ok, Vers, Val};
        [{_Key, #data{is_gone=true, version=Vers}}] -> {ok, gone, Vers}
    end.


do_list(#zynamo_command{value=Args}, State) ->
    #zynamo_list_args{value=WithValue, version=WithVersion, offset=Offset, limit=Limit} = Args,
    % Walk to the Key at Offset
    case offset(State#state.data, ets:first(State#state.data), Offset) of
        '$end_of_table' -> {ok, []};
        FirstKey ->
            Keys = limit(State#state.data, FirstKey, Limit, [FirstKey]),
            case WithValue or WithVersion of
                true ->
                    {ok, [
                        begin
                            [{_K,Data}] = ets:lookup(State#state.data, Key),
                            case {WithVersion, WithValue} of
                                {true, true} ->
                                    {Key, Data#data.version, Data#data.value};
                                {true, false} ->
                                    {Key, Data#data.version, undefined};
                                {false, true} ->
                                    {Key, undefined, Data#data.value}
                            end
                        end
                        || Key <- Keys
                    ]};
                false ->
                    {ok, [ {Key, undefined, undefined} || Key <- Keys ]}
            end
    end.

    offset(_Tab, Key, 0) ->
        Key;
    offset(Tab, Key, Offset) ->
        case ets:next(Tab, Key) of
            '$end_of_table' -> '$end_of_table';
            Next -> offset(Tab, Next, Offset-1)
        end.

    limit(_Tab, _Key, 0, Acc) ->
        lists:reverse(Acc);
    limit(Tab, Key, Limit, Acc) ->
        case ets:next(Tab, Key) of
            '$end_of_table' -> lists:reverse(Acc);
            Next -> limit(Tab, Next, case Limit of all -> all; _ -> Limit-1 end, [Next|Acc])
        end.


do_update(Command, Handoff, State) ->
    #zynamo_command{command=Cmd, key=Key, value=Value, version=Version, bucket_nr=BucketNr} = Command,
    IsHandoffOnly = not lists:member(node(), Handoff),
    case is_newer(Key, Version, State#state.data) of
        true ->
            Data = #data{
                key=Key,
                version=Version,
                bucket_nr=BucketNr,
                is_gone=(Cmd =:= delete),
                value=Value
            },
            ets:insert(State#state.data, [{Key, Data}]),
            ets:insert(State#state.handoff, [{{N, Key}, Version, Cmd} || N <- Handoff -- [node()] ]),
            case IsHandoffOnly of
                true -> ets:delete(State#state.local, Key);
                false -> ets:insert(State#state.local, {Key})
            end,
            {ok, Version};
        {false, OldVersion} ->
            % We can receive a handoff for data we already have.
            case zynamo_version:is_equal(Version, OldVersion) of
                true ->
                    % Set the local flag when this update is for the current node
                    case IsHandoffOnly of
                        false ->
                            case ets:lookup(State#state.local, Key) of
                                [{_K}] -> nop;
                                [] -> ets:insert(State#state.local, [{Key}])
                            end;
                        true ->
                            nop
                    end,
                    ets:insert(State#state.handoff, [{{N, Key}, Version, Cmd} || N <- Handoff -- [node()] ]),
                    {ok, Version};
                false ->
                    {error, {conflict, OldVersion}}
            end
    end.
    
do_handoff_check(Node, State) ->
    case ets:match(State#state.handoff, {{Node,'$1'}, '$2', '$3'}, 1) of
        '$end_of_table' -> 
            {reply, {ok, done}, State};
        {[[Key,Version,Command]], _Continuation} ->
            case ets:lookup(State#state.data, Key) of
                [{_Key, #data{version=Version} = Data}] ->
                    % Re-create the handoff command
                    Cmd = #zynamo_command{
                        command=Command,
                        key=Key,
                        version=Version,
                        bucket_nr=Data#data.bucket_nr,
                        value=Data#data.value
                    },
                    {reply, {ok, Cmd}, State};
                _Other ->
                    % Old handoff data, drop it
                    ets:delete(State#state.handoff, {Node,Key}),
                    do_handoff_check(Node, State)
            end
    end.


do_handoff_done(Node, ZynamoCommand, State) ->
    #zynamo_command{key=Key, version=Version, command=Command} = ZynamoCommand,
    case ets:lookup(State#state.handoff, {Node, Key}) of
        [{{_N,_K}, Version, Command}] ->
            ets:delete(State#state.handoff, {Node, Key}),
            case ets:lookup(State#state.local, Key) of
                [{Key}] ->
                    nop;
                [] ->
                    % Check if this was the last handoff, if so delete the entry
                    case ets:match(State#state.handoff, [{{'_', Key}, '_', '_'}], 1) of
                        '$end_of_table' -> 
                            ets:delete(State#state.data, Key);
                        _Match ->
                            nop
                    end
            end;
        _ ->
            % Not there, or a newer handoff request
            nop
    end.

%% @doc Check if the received version is newer than the one we have already.
is_newer(Key, Version, DataTable) ->
    case ets:lookup(DataTable, Key) of
        [] -> 
            true;
        [{_K, #data{version=PrevVers}}] -> 
            case zynamo_version:is_newer(Version, PrevVers) of
                true -> true;
                false -> {false, PrevVers}
            end
    end.
