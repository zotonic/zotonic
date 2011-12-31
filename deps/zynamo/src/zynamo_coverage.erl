%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Calculate which node cover the total key space for a service.

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

-module(zynamo_coverage).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    coverage/3,
    
    split/2
]).


%% @doc Calculate the best coverage for a given N and the nodes running the service.
%% @todo Optionally prefer a list which contains the current node.
-spec coverage(atom(), atom(), pos_integer()) -> {ok, [ node() ]} | {partial, [ node() ]} | {error, term()}.
coverage(Site, Service, N) ->
    {ok, RingNodes} = zynamo_manager:nodes(),
    {ok, ServicePidData} = zynamo_manager:locate_service(Site, Service),
    case lists:sort([ node(Pid) || {Pid,_Data} <- ServicePidData ]) of
        [] ->
            {error, no_nodes};
        ServiceNodes ->
            % Start with a random 'up' node.
            % optionally: start with the current node, iff current node in list of ServiceNodes
            StartNode = nth(ServiceNodes, zynamo_random:uniform(erlang:min(length(ServiceNodes),N+1)) - 1),
            RestServiceNodes = lists:dropwhile(fun(Node) -> Node /= StartNode end, ServiceNodes),
            RestRingNodes = lists:dropwhile(fun(Node) -> Node /= StartNode end, RingNodes),

            % Walk the ring, selecting nodes at the required interval
            select(tl(RestRingNodes)++RingNodes, 
                   tl(RestServiceNodes)++ServiceNodes, 
                   N, N,
                   StartNode,[StartNode], undefined, true)
    end.


    select([R|_], _, _, _, R, Acc, _, true) ->
        {ok, Acc};
    select([R|_], _, _, _, R, Acc, _, false) ->
        {partial, Acc};
    select([R|Rs], [R|Ss], 1, N, StartNode, Acc, _, IsOk) ->
        % Good node, exactly where we want it.
        select(Rs, Ss, N, N, StartNode, [R|Acc], undefined, IsOk);
    select([_|Rs], Ss, 1, N, StartNode, Acc, undefined, _IsOk) ->
        % No last good node to step back to, we have a hole...
        select(Rs, Ss, 1, N, StartNode, Acc, undefined, false);
    select([_|Rs], Ss, 1, N, StartNode, Acc, LastR, IsOk) ->
        % Step back to the last known good node, count from there.
        select(Rs, Ss, N, N, StartNode, [LastR|Acc], undefined, IsOk);
    select([R|Rs], [R|Ss], K, N, StartNode, Acc, _, IsOk) ->
        % A good node, remember it for when we need to step back
        select(Rs, Ss, K-1, N, StartNode, Acc, R, IsOk);
    select([_|Rs], Ss, K, N, StartNode, Acc, LastR, IsOk) ->
        % A bad node, but at a convenient place, skip it.
        select(Rs, Ss, K-1, N, StartNode, Acc, LastR, IsOk).



%% @doc Split Nodes in covering lists of nodes, with step N.
split(Nodes, N) ->
    split(Nodes++Nodes, (length(Nodes)+N-1) div N, lists:duplicate(N, []), []).

split(_Ns, 0, Accs, _Accs2) ->
    Accs;
split(Ns, Ct, [], Accs2) ->
    split(Ns, Ct-1, lists:reverse(Accs2), []);
split([N|Ns], Ct, [Acc|Accs], Accs2) ->
    split(Ns, Ct, Accs, [[N|Acc]|Accs2]).


nth([], _) -> undefined;
nth([H|_], 0) -> H;
nth([_|T], N) -> nth(T, N-1).

