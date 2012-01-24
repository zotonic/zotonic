%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc zynamo core type definitions.

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

%% The hash ring of a zotonic system consists of nodes that are responsible for a part of the ring.
%% Keys are hashed to buckets and the buckets have a primary node that is responsible for storing
%% values hashed to the bucket.
%% 
%% When a node is added it will claim a range from the ring.  This range is calculated depending on 
%% the number of nodes in the ring.  All nodes have an equal weight.

%% @doc Statistics returned on ring requests (like put or get.
-record(zynamo_request_stats, {
    protocol_version = 1,
    participating :: [ node() ],
    success       :: [ node() ],
    fail          :: [ {node(), Reason :: term()} ],
    timeout       :: [ node() ],
    duration      :: non_neg_integer()
}).

-type zynamo_request_stats() :: #zynamo_request_stats{}.

-type zynamo_version() :: non_neg_integer()
                        | zynamo_vclock:vclock()
                        | undefined.


-record(zynamo_command, {
    protocol_version = 1,
    ref         :: reference(),
    bucket_nr   :: non_neg_integer(),
    command     :: atom(),
    key         :: term(),
    version     :: zynamo_version(),
    value       :: term(),
    handoff_ref :: term()
}).

-record(zynamo_service_command, {
    protocol_version = 1,
    ref         :: reference(),
    is_first    :: boolean(),
    from        :: pid(),
    handoff     :: [ node() ],
    command     :: #zynamo_command{}
}).


%% @doc Result from a service, sent back to the zynamo_request_fsm
-record(zynamo_service_result, {
    protocol_version = 1,
    status = ok :: atom(),
    node = node(),
    is_found = true :: boolean(),
    is_gone = false :: boolean(),
    key         :: term(),
    version     :: zynamo_version(),
    value       :: term()
}).


%% @doc Result from a zynamo_request
-record(zynamo_result, {
    protocol_version = 1,
    status = ok,
    ref         :: reference(),     % The reference of the command
    is_primary  :: boolean(),       % Was the result from a primary node?
    is_quorum   :: boolean(),       % Was the maximum quorum, considering the ring, reached
    is_gone     :: boolean(),       % Was the key deleted?
    is_found    :: boolean(),       % Was the key found?
    is_multiple :: boolean(),       % Is value a list of multiple values?
    node        :: node(),          % Node from which we got the value (preferably a primary node)
    version     :: zynamo_version(),   % Version iff value is a single value
    value       :: term(),          % Single value or multiple values (depends on command and is_multiple)
    other       :: term(),          % Other #zynamo_service_result tuples, iff best version could not be derived
    stats       :: zynamo_request_stats()   % Optional command statistics
}).

-type zynamo_callback() :: {atom(),atom()} | pid() | function().

-type zynamo_request_option() :: 
                       {node, random 
                            | local_random 
                            | local
                            | [ node() ] 
                            | sha
                            | function()
                            | non_neg_integer()
                            | {atom(),atom()} 
                            | {key, term()}
                            | {key, term(), sha|function()|{atom(),atom()}}
                       }
                     | {node_exclude, [ node() ]}
                     | {request_timeout, non_neg_integer()}
                     | {node_timeout, non_neg_integer()}
                     | {n, non_neg_integer() | all}
                     | {quorum, non_neg_integer() | n}
                     | not_found_is_error
                     | stats
                     | no_handoff
                     | {result, raw|merge}
                     | {final_action, zynamo_callback() | read_repair}.
-type zynamo_request_options() :: list( zynamo_request_option() ).


%% @doc Default timeout for operations is 10 seconds.
-define(ZYNAMO_REQUEST_TIMEOUT, 10000).

-define(ZYNAMO_DEFAULT_N, 3).
-define(ZYNAMO_DEFAULT_QUORUM, 2).

% Number of entries per hash range request
-define(ZYNAMO_SYNC_HASH_LIMIT, 10000).
-define(ZYNAMO_SYNC_HASH_STEP,  100).


% ===========================================================
% Specific values for some #zynamo_command{} records.
% ===========================================================

%% @doc 'value' of a 'list' command.
-record(zynamo_list_args, {
    protocol_version = 1,
    return_value = false :: boolean(),
    return_version = false :: boolean(),
    return_gone = false :: boolean(),
    bucket_range = undefined :: {non_neg_integer(), non_neg_integer()},
    offset  = 0 :: non_neg_integer() | {key, term()},
    limit   = all :: pos_integer() | all
}).


