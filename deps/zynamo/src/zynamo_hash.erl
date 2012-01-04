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
    nodelist_bucket/2,
    
    hash/1,

    unique_id/0,
    unique_id_split/1,
    node_id/0
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
-spec hash(term()) -> non_neg_integer().
hash(Key) ->
    <<Sha:160/integer>> = crypto:sha(term_to_binary(Key)),
    Sha rem zynamo_ring:ring_buckets().


%% @doc Generate an unique id, based on a number in the node name or hostname.
-spec unique_id() -> pos_integer().
unique_id() ->
    case node_id() of
        Nr when Nr < 1000 -> 
            {A,B,C} = now(),
            (((A * 1000000)+B)*1000000+C)*1000+Nr;
        _ ->
            error(node_number_too_large)
    end.

%% @doc Return the node id of this node. Should be unique in the cluster
-spec node_id() -> non_neg_integer().
node_id() ->
    [Name,Host] = string:tokens(atom_to_list(node()), "@"),
    case node2nr(hd(string:tokens(Host, "."))) of
        undefined ->
            case node2nr(Name) of
                undefined -> error(node_number_missing);
                Nr -> Nr
            end;
        Nr -> Nr
    end.

node2nr(L) ->
    case [ D || D <- L, D >= $0, D =< $9 ] of
        [] -> undefined;
        N -> list_to_integer(N)
    end.


%% @doc Return the node and timestamp of an unique id.
-spec unique_id_split(pos_integer()) -> {non_neg_integer(), {{integer(), integer(), integer()}, {integer(), integer(), integer()}}}.
unique_id_split(Id) ->
    Nr = Id rem 1000,
    Now = Id div 1000,
    C = Now rem 1000000,
    B = (Now div 1000000) rem 1000000,
    A = (Now div 1000000) div 1000000,
    {Nr, calendar:now_to_universal_time({A,B,C})}.

