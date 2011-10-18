%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Marc Worrell
%% @doc Simple topological sort of tuples {item, [depends], [provides]}

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

-module(z_toposort).

-export([
    sort/1
]).


%% @doc Return the topological sort of a list.
-spec sort(topoitems()) -> {ok, list()} | {error, {cyclic, list()}}.
-type topoitems() :: {Name::term(), Depends::list(), Provides::list()}.  

sort([]) ->
    {ok, []};
sort(L) ->
    G = digraph:new(),
    Vs = [ {N, digraph:add_vertex(G)} || {N, _, _} <- L ],
    add_node(G, L, L, Vs).


add_node(G, _Nodes, [], Vs) ->
    case digraph_utils:is_acyclic(G) of
        true ->
            SortedVs = digraph_utils:topsort(G),
            digraph:delete(G),
            {ok, vertices_to_nodes(SortedVs, Vs)};
        false ->
            Cycles = digraph_utils:cyclic_strong_components(G),
            digraph:delete(G),
            {error, {cyclic, [ vertices_to_nodes(Components, Vs) || Components <- Cycles ]}}
    end;
add_node(G, Nodes, [{_N, [], _Provides}|L], Vs) ->
    add_node(G, Nodes, L, Vs);
add_node(G, Nodes, [{Node, Depends, _Provides}|L], Vs) ->
    {Node, NVx} = proplists:lookup(Node, Vs), 
    DepNodes = lists:flatten([ find_node(Nodes, [], Depend) || Depend <- Depends ]),
    [ 
      begin
          {N, Vx} = proplists:lookup(N, Vs),
          digraph:add_edge(G, Vx, NVx)
      end
      || N <- DepNodes
    ],
    add_node(G, Nodes, L, Vs).
    
% find_node([], [], D) ->
%     throw({error, {missing_provide, D}});
find_node([], Fs, _D) ->
    Fs;
find_node([{N, _, Provides}|L], Fs, D) ->
    case lists:member(D, Provides) of
        true -> find_node(L, [N|Fs], D);
        false -> find_node(L, Fs, D)
    end.


vertices_to_nodes(Vertices, Nodes) ->
    [ 
        begin
            {value, {N,_}} = lists:keysearch(V, 2, Nodes),
            N
        end
        || V <- Vertices 
    ].
    
