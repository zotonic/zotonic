%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2024 Marc Worrell
%% @doc Process buffering uploaded files till they are complete.
%% @end

%% Copyright 2021-2024 Marc Worrell
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

-module(z_fileuploader).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/3,
    start_link/4,
    stop/1,
    status/1,
    exists/1,
    upload/3
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


-include_lib("zotonic_core/include/zotonic.hrl").


-type block() :: { Start :: non_neg_integer(), End :: non_neg_integer() }.

-record(state, {
    name :: binary(),
    user_id :: m_rsc:resource_id() | undefined,
    start_msec :: non_neg_integer(),

    filename :: binary(),
    size :: non_neg_integer(),

    file :: file:filename_all(),
    fd :: file:io_device(),

    received :: non_neg_integer(),
    blocks :: [ block() ]
}).

% Timeout after an hour of inactivity.
-define(TIMEOUT, 3600000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Create a new process managing a file upload.
-spec start_link( file:filename_all(), pos_integer(), z:context() ) -> {ok, pid()} | {error, term()}.
start_link(Filename, Size, Context) ->
    start_link(z_ids:id(), Filename, Size, Context).

%% @doc Create a new process managing a file upload.
-spec start_link( binary(), file:filename_all(), pos_integer(), z:context() ) -> {ok, pid()} | {error, term()}.
start_link(Name, Filename, Size, Context) ->
    gen_server:start_link(?MODULE, [Name, Filename, Size, Context], []).


%% @doc Return the topics and status for the file upload.
-spec status( pid() | binary() ) -> {ok, map()} | {error, enoent}.
status(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, status);
status(Name) when is_binary(Name) ->
    case gproc:lookup_local_name({?MODULE, Name}) of
        undefined -> {error, enoent};
        Pid -> status(Pid)
    end.

%% @doc Check if an uploader exists
-spec exists( binary() | undefined ) -> boolean().
exists(Name) when is_binary(Name) ->
    is_pid(gproc:lookup_local_name({?MODULE, Name}));
exists(undefined) ->
    false.


%% @doc Stop an uploader.
-spec stop( pid() | binary() ) -> ok | {error, enoent}.
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop(Name) when is_binary(Name) ->
    case gproc:lookup_local_name({?MODULE, Name}) of
        undefined -> {error, enoent};
        Pid -> stop(Pid)
    end.


-spec upload( pid() | binary(), non_neg_integer(), binary() ) -> {ok, map()} | {error, term()}.
upload(Pid, Offset, Data) when is_pid(Pid) ->
    gen_server:call(Pid, {upload, Offset, Data});
upload(Name, Offset, Data) when is_binary(Name) ->
    case gproc:lookup_local_name({?MODULE, Name}) of
        undefined -> {error, enoent};
        Pid -> upload(Pid, Offset, Data)
    end.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Name, Filename, Size, Context]) ->
    gproc:add_local_name({?MODULE, Name}),
    {ok, {_TmpPid, TmpFile}} = z_tempfile:monitored_new(),
    {ok, Fd} = file:open(TmpFile, [ write, binary ]),
    {ok, #state{
        name = Name,
        start_msec = os:system_time(millisecond),
        user_id = z_acl:user(Context),
        filename = Filename,
        size = Size,
        file = TmpFile,
        fd = Fd,
        received = 0,
        blocks = []
    }, ?TIMEOUT}.

handle_call(status, _From, State) ->
    {reply, {ok, state_status(State)}, State, ?TIMEOUT};
handle_call({upload, Offset, _Data}, _From, State) when Offset < 0 ->
    {reply, {error, offset}, State, ?TIMEOUT};
handle_call({upload, _Offset, Data}, _From, State) when size(Data) =:= 0 ->
    {reply, {error, size}, State, ?TIMEOUT};
handle_call({upload, Offset, Data}, _From, #state{ size = Size } = State)
    when Offset + size(Data) > Size ->
    {reply, {error, offset}, State, ?TIMEOUT};
handle_call({upload, Offset, Data}, _From, State) ->
    try
        {ok, Offset} = file:position(State#state.fd, Offset),
        ok = file:write(State#state.fd, Data),
        Blocks = [ {Offset, Offset + size(Data) - 1} | State#state.blocks ],
        Blocks1 = combine_blocks(Blocks),
        State1 = State#state{
            blocks = Blocks1,
            received = count_size(Blocks1, 0)
        },
        if
            State1#state.size =:= State1#state.received ->
                ?LOG_INFO(#{
                    in => zotonic_mod_fileuploader,
                    text => <<"Upload complete">>,
                    result => ok,
                    filename => State1#state.filename,
                    size => State1#state.size,
                    speed_kbs => speed_kbs(State1#state.start_msec, State1#state.size)
                });
            true ->
                ok
        end,
        {reply, {ok, state_status(State1)}, State1, ?TIMEOUT}
    catch Type:Error ->
        ?LOG_ERROR(#{
            in => zotonic_mod_fileuploader,
            text => <<"fileuploader: error uploading to file">>,
            bytes => size(Data),
            filename => State#state.filename,
            size => State#state.size,
            result => Type,
            reason => Error
        }),
        {reply, {error, fatal}, State, 0}
    end.

handle_cast(stop, State) ->
    self() ! timeout,
    {noreply, State}.

handle_info(timeout, State) ->
    _ = file:close(State#state.fd),
    {stop, normal, State}.


%% ------------------------------------------------------------------
%% Support functions
%% ------------------------------------------------------------------

speed_kbs(Start, Bytes) ->
    case os:system_time(millisecond) - Start of
        0 -> 0;
        Delta -> erlang:round((Bytes / Delta) / 1024 * 1000)
    end.


%% @doc Return the status of this uploader.
state_status(State) ->
    #{
        name => State#state.name,
        filename => State#state.filename,
        uploaded_file => State#state.file,
        is_complete => State#state.size =:= State#state.received,
        size => State#state.size,
        received => State#state.received,
        user_id => State#state.user_id,
        missing => lists:reverse( missing(State#state.blocks, 0, State#state.size-1, []) ),
        blocks => State#state.blocks
    }.

%% @doc Count the total number of bytes uploaded till now. Do not count
%% segments that are uploaded multiple times.
count_size([], Count) ->
    Count;
count_size([ {Start, End} | Bs ], Count) ->
    count_size(Bs, Count + End - Start + 1).

%% @doc Merge overlapping and adjoining blocks
combine_blocks(Blocks) ->
    Bs1 = lists:sort(Blocks),
    lists:reverse( combine(Bs1, []) ).

combine([], Acc) ->
    Acc;
combine([B], Acc) ->
    [ B | Acc ];
combine([ {A,B}, {C,D} | Bs ], Acc) when B+1 >= C ->
    combine([ {A, D} | Bs ], Acc);
combine([ B | Bs ], Acc) ->
    combine(Bs, [ B | Acc ]).

%% @doc Check which block are still missing. The blocks must be
%% sorted on ascending offset.
missing([], Offset, LastByte, Acc) when Offset > LastByte ->
    Acc;
missing([], Offset, LastByte, Acc) when Offset =< LastByte ->
    [ {Offset, LastByte} | Acc ];
missing([ {A,B} | Bs ], Offset, LastByte, Acc) when A =< Offset ->
    missing(Bs, B+1, LastByte, Acc);
missing([ {A,B} | Bs ], Offset, LastByte, Acc) when A > Offset ->
    Acc1 = [ {Offset, A-1} | Acc ],
    missing(Bs, B+1, LastByte, Acc1).
