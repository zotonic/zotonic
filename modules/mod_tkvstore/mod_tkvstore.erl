%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%%
%% @doc Simple (type,key)/value store. Stores data in the store with minimal latency 
%% and (local) serialization of get/put requests.

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

-module(mod_tkvstore).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_title("Typed K/V Store").
-mod_description("Simple typed key/value store used to store structured data.").

-include_lib("zotonic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    pid_observe_tkvstore_get/3,
    pid_observe_tkvstore_put/3,
    pid_observe_tkvstore_delete/3,
    
    writer_loop/2
]).


-record(state, {context, writer_pid, data}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Do a put in the persistent store, replace existing key/value
pid_observe_tkvstore_put(Pid, #tkvstore_put{} = Message, _Context) ->
    gen_server:cast(Pid, Message).

%% @doc Fetch the persistent data of a type/key
pid_observe_tkvstore_get(Pid, #tkvstore_get{} = Message, _Context) ->
    gen_server:call(Pid, Message).

%% @doc Delete the persistent data of a type/key
pid_observe_tkvstore_delete(Pid, #tkvstore_delete{} = Message, _Context) ->
    gen_server:cast(Pid, Message).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    m_tkvstore:init(Context),
    WriterPid = erlang:spawn_link(?MODULE, writer_loop, [self(), Context]),
    {ok, #state{
            context=z_context:new(Context), 
            data=dict:new(), 
            writer_pid=WriterPid
        }}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Fetch persistent data, first check the data dict that is still being written
handle_call(#tkvstore_get{type=Type, key=Key}, _From, State) ->
    case dict:find({Type, Key}, State#state.data) of
        {ok, Data} ->
            % Data is being written, return the data that is not yet in the store
            {reply, Data, State};
        error -> 
            %% @todo Spawn this process when it starts to be a blocker
            {reply, m_tkvstore:get(Type, Key, State#state.context), State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Put request, copy to the writer, keep a local copy of the data
handle_cast(#tkvstore_put{type=Type, key=Key, value=Data}, State) ->
    State#state.writer_pid ! {data, Type, Key, Data},
    {noreply, State#state{ data=dict:store({Type, Key}, Data, State#state.data) }};

%% @doc Delete a value from the tkvstore    
handle_cast(#tkvstore_delete{type=Type, key=Key}, State) ->
    State#state.writer_pid ! {delete, Type, Key},
    {noreply, State#state{ data=dict:store({Type, Key}, undefined, State#state.data) }};

%% @doc Writer loop wrote the data, remove our local copy
handle_cast({written, Type, Key}, State) ->
    {noreply, State#state{ data=dict:erase({Type, Key}, State#state.data) }};

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
    State#state.writer_pid ! stop,
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================




%% @doc Simple writer loop, started as a process
writer_loop(KVStorePid, Context) ->
    receive
        {delete, Type, Key} ->
            m_tkvstore:delete(Type, Key, Context),
            gen_server:cast(KVStorePid, {written, Type, Key}),
            ?MODULE:writer_loop(KVStorePid, Context);
        {data, Type, Key, Data} ->
            m_tkvstore:put(Type, Key, Data, Context),
            gen_server:cast(KVStorePid, {written, Type, Key}),
            ?MODULE:writer_loop(KVStorePid, Context);
        stop ->
            ok
    end.
