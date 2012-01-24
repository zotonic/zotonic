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

-module(zynamo_service_kv).
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

handle_call({is_sync_wanted, _SecondsSinceLastSync}, _From, State) ->
    {reply, {ok, true}, State};

handle_call(sync_n, _From, State) ->
    {reply, {ok, 3}, State};

handle_call(Message, _From, State) ->
    {reply, {error, {unknown_call, Message}}, State}.

handle_cast(#zynamo_service_command{
                command=Command,
                handoff=Handoff
            }=SC, State) ->
    Reply = case Command#zynamo_command.command of
                get ->
                    do_get(Command, State);
                list ->
                    do_list(Command, State);
                list_hash ->
                    % Used when syncing, return a list of hashes of list ranges
                    case do_list(Command, State) of
                        #zynamo_service_result{value='$end_of_table'} ->
                            #zynamo_service_result{value=[]};
                        #zynamo_service_result{value=L} ->
                            #zynamo_service_result{value=zynamo_hash:hash_sync_list(L)};
                        Other ->
                            Other
                    end;
                Upd when Upd =:= put; Upd =:= delete ->
                    do_update(Command, Handoff, State);
                _Other ->
                    #zynamo_service_result{status=operation_not_supported}
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
        [] -> 
            #zynamo_service_result{
                is_found=false,
                is_gone=false
            };
        [{_Key, #data{is_gone=false, version=Vers, value=Val}}] -> 
            #zynamo_service_result{
                is_gone=false,
                is_found=true,
                version=Vers,
                value=Val
            };
        [{_Key, #data{is_gone=true, version=Vers}}] -> 
            #zynamo_service_result{
                is_gone=true,
                is_found=true,
                version=Vers
            }
    end.


do_list(#zynamo_command{value=Args}, State) ->
    case Args#zynamo_list_args.offset of
        undefined -> do_list_offset_int(0, Args, State);
        N when is_integer(N) -> do_list_offset_int(N, Args, State);
        {key, Key} -> do_list_offset_key(Key, Args, State);
        _Other -> #zynamo_service_result{status=unsupported_offset}
    end.
    
% Walk to the Key at Offset
do_list_offset_int(Offset, Args, State) ->
    #zynamo_list_args{return_value=WithValue, return_version=WithVersion, return_gone=IsListGone, limit=Limit} = Args,
    case offset(State#state.data, ets:first(State#state.data), Offset) of
        '$end_of_table' ->
            #zynamo_service_result{
                value='$end_of_table'
            };
        FirstKey ->
            Keys = limit(State#state.data, FirstKey, Limit, [FirstKey]),
            #zynamo_service_result{
                value=add_value_version(WithValue, WithVersion, IsListGone, Keys, State)
            }
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

% Take Limit keys, starting with the given key
do_list_offset_key(Key, Args, State) ->
    #zynamo_list_args{return_value=WithValue, return_version=WithVersion, return_gone=IsListGone, limit=Limit} = Args,
    Keys = case Key of
                undefined -> limit_key(State#state.data, ets:first(State#state.data), Limit, []);
                _ -> limit_key(State#state.data, ets:next(State#state.data, Key), Limit, [])
           end,
    #zynamo_service_result{
        value=add_value_version(WithValue, WithVersion, IsListGone, Keys, State)
    }.

    limit_key(_Data, _Key, N, Acc) when N =< 0 ->
        lists:reverse(Acc);
    limit_key(_Data, '$end_of_table', _N, Acc) ->
        lists:reverse(Acc);
    limit_key(Data, Key, N, Acc) ->
        limit_key(Data, ets:next(Data, Key), N-1, [Key|Acc]).
    

add_value_version(WithValue, WithVersion, IsListGone, Keys, State) ->
    case WithValue or WithVersion or not IsListGone of
        true ->
            lists:foldr(
                    fun(Key, Acc) ->
                        [{_K,Data}] = ets:lookup(State#state.data, Key),
                        case IsListGone or not Data#data.is_gone of
                            true ->
                                case {WithVersion, WithValue} of
                                    {true, true} ->
                                        [
                                            #zynamo_service_result{
                                                key=Key,
                                                version=Data#data.version,
                                                value=Data#data.value
                                            } | Acc
                                        ];
                                    {true, false} ->
                                        [
                                            #zynamo_service_result{
                                                key=Key,
                                                version=Data#data.version
                                            } | Acc
                                        ];
                                    {false, true} ->
                                        [
                                            #zynamo_service_result{
                                                key=Key,
                                                value=Data#data.value
                                            } | Acc
                                        ];
                                    {false, false} ->
                                        [ #zynamo_service_result{key=Key}|Acc ]
                                end;
                            false ->
                                Acc
                        end
                    end,
                    [],
                    Keys);
        false ->
            [ #zynamo_service_result{key=Key} || Key <- Keys ]
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
            % Set the local flag when this update is for the current node
            case IsHandoffOnly of
                false -> ets:insert(State#state.local, {Key});
                true -> nop
            end,
            #zynamo_service_result{key=Key, version=Version, is_gone=(Cmd =:= delete)};
        {false, OldVersion} ->
            % We can receive a handoff for data we already have.
            case zynamo_version:is_equal(Version, OldVersion) of
                true ->
                    % Set the local flag when this update is for the current node
                    case IsHandoffOnly of
                        false -> ets:insert(State#state.local, [{Key}]);
                        true -> nop
                    end,
                    insert_handoff(State#state.handoff, Key, Version, Cmd, lists:delete(node(),Handoff)),
                    #zynamo_service_result{key=Key, version=Version};
                false ->
                    #zynamo_service_result{status=conflict, key=Key, version=OldVersion}
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


%% @doc Insert a handoff command, check if we don't have a newer handoff registered.
insert_handoff(_Handoff, _Key, _Version, _Cmd, []) ->
    ok;
insert_handoff(Handoff, Key, Version, Cmd, [N|Nodes]) ->
    case ets:lookup(Handoff, {N,Key}) of
        [] ->
            ets:insert(Handoff, {{N, Key}, Version, Cmd});
        [{_,HandoffVersion,_}] ->
            case zynamo_version:is_newer(Version, HandoffVersion) of
                true ->
                    ets:insert(Handoff, {{N, Key}, Version, Cmd});
                false ->
                    nop
            end
    end,
    insert_handoff(Handoff, Key, Version, Cmd, Nodes).


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
