%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Version comparison, handles timestamps, sequence numbers and vclocks.

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

%% The hash ring of a zotonic system consists of nodes that are responsible for a part of the ring.
%% Keys are hashed to buckets and the buckets have a primary node that is responsible for storing
%% values hashed to the bucket.
%% 
%% When a node is added it will claim a range from the ring.  This range is calculated depending on 
%% the number of nodes in the ring.  All nodes have an equal weight.

-module(zynamo_version).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_newer/2,
    is_equal/2,
    
    timestamp/0
]).


%% @doc Check if the first arg A is newer than the second arg B.
%%      This functions handles mixes of timestamps well.
is_newer(undefined, undefined) ->
    true;
is_newer(_, undefined) ->
    true;
is_newer(undefined, _) ->
    false;
is_newer(A, B) when is_integer(A), is_integer(B) ->
    A > B;
is_newer(A, B) when is_list(A), is_integer(B) ->
    is_newer(vclock_timestamp(A), B);
is_newer(A, B) when is_integer(A), is_list(B) ->
    is_newer(vclock_timestamp(A), B);
is_newer(A, B) ->
    vclock:descends(A,B).    


is_equal(undefined, undefined) ->
    true;
is_equal(undefined, _) ->
    false;
is_equal(_, undefined) ->
    false;
is_equal(A, B) when is_integer(A), is_integer(B) ->
    A =:= B;
is_equal(A, B) when is_list(A), is_integer(B) ->
    is_equal(vclock_timestamp(A), B);
is_equal(A, B) when is_integer(A), is_list(B) ->
    is_equal(A, vclock_timestamp(B));
is_equal(A, B) ->
    zynamo_vclock:equal(A, B).


%% @doc Convert a timestamp to microseconds.
vclock_timestamp(V) ->
    case zynamo_vclock:timestamp(V) of
        undefined -> undefined;
        T -> T * 1000
    end.

%% Return an unique timestamp in microseconds, based on erlang:now()
timestamp() ->
    {A,B,C} = erlang:now(),
    (A * 1000000 + B)*1000000 + C.
