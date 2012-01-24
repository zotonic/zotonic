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
    newest/1,
    newest_primary/2,
    newest_tuple/2,
    
    is_newer/2,
    is_equal/2,
    
    timestamp/0
]).

-include_lib("zynamo.hrl").


%% @doc Determine the newest value in a list of values. The list may not be empty.
newest([H|T]) ->
    lists:foldl(fun(Vers, Acc) ->
                    case is_newer(Vers, Acc) of
                        true -> Vers;
                        false -> Acc
                    end
                end,
                H,
                T).

%% @take The newest version, and when versions are equal the version of a primary node.
newest_primary([H|T], PrimaryNodes) ->
    lists:foldl(fun(Vers, {Acc,IsPrimary} = Current) ->
                    IsPrimary1 = lists:member(get_node(Vers),PrimaryNodes),
                    case is_newer(Vers, Acc) 
                        orelse (not IsPrimary 
                            andalso IsPrimary1
                            andalso is_equal(Vers, Acc))
                    of
                        true -> {Vers,IsPrimary1};
                        false -> Current
                    end
                end,
                {H, lists:member(get_node(H),PrimaryNodes)},
                T).
    
    get_node(#zynamo_service_result{node=Node}) ->
        Node.


%% @doc Determine the newest pair in a list of {version, term()} pairs. The list may not be empty.
newest_tuple(N, [H|T]) ->
    lists:foldl(fun(VV, Acc) ->
                    Vers = element(N, VV),
                    Best = element(N, Acc),
                    case is_newer(Vers, Best) of
                        true -> VV;
                        false -> Acc
                    end
                end,
                H,
                T).

%% @doc Check if the first arg A is newer than the second arg B.
%%      This functions handles mixes of timestamps well.
is_newer(#zynamo_service_result{is_found=false}, B) ->
    is_newer(undefined, B);
is_newer(A, #zynamo_service_result{is_found=false}) ->
    is_newer(A, undefined);
is_newer(#zynamo_service_result{version=Version}, B) ->
    is_newer(Version, B);
is_newer(A, #zynamo_service_result{version=Version}) ->
    is_newer(A, Version);
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



is_equal(A, A) ->
    true;
is_equal(#zynamo_service_result{is_found=false}, B) ->
    is_equal(undefined, B);
is_equal(A, #zynamo_service_result{is_found=false}) ->
    is_equal(A, undefined);
is_equal(#zynamo_service_result{version=A}, B) ->
    is_equal(A, B);
is_equal(A, #zynamo_service_result{version=B}) ->
    is_equal(A, B);
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
