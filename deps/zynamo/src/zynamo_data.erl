%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc zynamo data - read/write a value from/to the zynamo ring

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

%% Basic get/put/delete functions for accessing data on the zynamo ring.
%% The routines work with the usual Dynamo quorum and eventual consistency.

-module(zynamo_data).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    get/3,
    get/4,

    put/5,
    put/6,

    list/3,
    list/4
]).


%% @doc Statistics returned on read/write operations.
-record(data_stats, {
    participating :: [ node() ],
    success       :: [ node() ],
    fail          :: [ {node(), Reason :: term()} ],
    timeout       :: [ node() ],
    duration      :: non_neg_integer()
}).

-type data_stats() :: #data_stats{}.

-type data_version() :: {vclock, VectorClock :: term()} | {time, TimeInMsec :: non_neg_integer() | now} | none.


%% @doc Read a value from the zynamo ring. Returns 'maybe' when the quorum has not been reached.
get(Site, Service, Key) ->
    get(Site, Service, Key, []).

-spec get(atom(), atom(), Key::term(), list()) -> {ok, Value :: term(), data_stats()} 
                                        | {maybe, Value :: term(), data_stats()} 
                                        | {multiple, [Value :: term()], data_stats()} 
                                        | {error, Reason :: term()}.
get(Site, Service, Key, Options) ->
    {error, not_implemented}.



%% @doc Read a value from the zynamo ring. Returns 'maybe' when the quorum has not been reached.
put(Site, Service, Key, Version, Value) ->
    put(Site, Service, Key, Version, Value, []).

put(Site, Service, Key, Version, Value, Options) ->
    {error, not_implemented}.


%% @doc List all key/values on the servers, tryes to deduplicate. 
%% Server selection is 'coverage', 'all', {node, [node()]} or {hash, [Key]}.
%% The receiver is a Pid which will be sent messages {list, Reference, data, {Key, Value}}.
%% The final message is {list, Reference, eoi, Stats}.
%% The error message is {list, Reference, error, Reason}.
list(Site, Service, Receiver) ->
    list(Site, Service, Receiver, []).

-spec list(atom(), atom(), {pid, pid(), reference()}, list()) -> ok | {error, Reason :: term()}.
list(Site, Service, Receiver, Options) ->
    {error, not_implemented}.
