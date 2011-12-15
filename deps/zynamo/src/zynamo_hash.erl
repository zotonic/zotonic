%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2011 Marc Worrell
%% @doc Hash functions over a ring

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


-module(zynamo_hash).

-export([
    nodelist_hash/2,
    nodelist_bucket/2
]).


%% @doc Find the primary node responsible for storing a key, using the default hash function.
%%      List all nodes from that node, including their state.
-spec nodelist_hash(term(), [{integer(),integer(),node()}]) -> [ node() ].
nodelist_hash(Key, Ranges) ->
    nodelist_bucket(hash(Key), Ranges).

%% @doc Find the primary node responsible for storing a bucket. List all nodes from that
%%      bucket onwards.
nodelist_bucket(BucketNr, []) ->
    {BucketNr, []};
nodelist_bucket(BucketNr, Ranges) ->
    {BucketNr, list_from_bucket(BucketNr, Ranges, [])}.
    
    list_from_bucket(_Bucket, [], PrevNodes) ->
       lists:reverse(PrevNodes);
    list_from_bucket(Bucket, [{_Lo,Hi,Node}|T] = NextRanges, PrevNodes) ->
        case Hi >= Bucket of
            true -> [ N || {_,_,N} <- NextRanges ] ++ lists:reverse(PrevNodes);
            false -> list_from_bucket(Bucket, T, [Node|PrevNodes])
        end.

%% @doc Simple but functional hash function.
%% @todo Make this configurable
hash(Key) ->
    <<Sha:160/integer>> = crypto:sha(term_to_binary(Key)),
    Sha rem zynamo_ring:ring_buckets().

