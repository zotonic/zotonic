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
    participating :: [ node() ],
    success       :: [ node() ],
    fail          :: [ {node(), Reason :: term()} ],
    timeout       :: [ node() ],
    duration      :: non_neg_integer()
}).

-type zynamo_request_stats() :: #zynamo_request_stats{}.

-type zynamo_data_version() :: {vclock, VectorClock :: term()} 
                             | {time, TimeInMsec :: non_neg_integer() | now}
                             | none.

-record(zynamo_command, {
    ref         :: reference(),
    bucket_nr   :: non_neg_integer(),
    command     :: atom(),
    key         :: term(),
    version     :: zynamo_data_version(),
    value       :: term()
}).

-record(zynamo_service_command, {
    ref         :: reference(),
    is_primary  :: boolean(),
    from        :: pid(),
    handoff     :: [ node() ],
    command     :: #zynamo_command{}
}).

-type zynamo_callback() :: {atom(),atom()} | read_repair | pid() | function().

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
                     | {request_timeout, non_neg_integer()}
                     | {node_timeout, non_neg_integer()}
                     | {n, non_neg_integer() | all}
                     | {quorum, non_neg_integer() | n}
                     | no_handoff
                     | {final_action, zynamo_callback()}.
-type zynamo_request_options() :: list( zynamo_request_option() ).


%% @doc Default timeout for operations is 10 seconds.
-define(ZYNAMO_REQUEST_TIMEOUT, 10000).

-define(ZYNAMO_DEFAULT_N, 3).
-define(ZYNAMO_DEFAULT_QUORUM, 2).


% ===========================================================
% Specific values for some #zynamo_command{} records.
% ===========================================================

%% @doc 'value' of a 'list' command.
-record(zynamo_list_args, {
    value   = false :: boolean(),
    version = false :: boolean(),
    offset  = 0 :: non_neg_integer(),
    limit   = all :: pos_integer() | all
}).


