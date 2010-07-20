%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%%
%% @doc Export function for resources.

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

-module(m_rsc_export).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").


-export([full/2,
         simple/2
         ]).


%% @doc Get the full representation of a resource.
full(Id, Context) when is_integer(Id) ->
    case m_rsc:exists(Id, Context) of
        false -> undefined;
        true ->

            Rsc = m_rsc:get_raw(Id, Context),
            %% This should probably be encapsulated in m_edges.
            Edges0 = z_db:assoc("
                                select e.predicate_id, p.name as predicate_name, e.object_id, e.seq
                                from edge e join rsc p on p.id = e.predicate_id
                                where e.subject_id = $1
                                order by e.predicate_id, e.seq, e.id", [Id], Context),
            Edges = [edge_details(E, Context) || E <- Edges0],

            {ok, Category} = z_db:select(category, Id, Context),
            {ok, Medium} = z_db:select(medium, Id, Context),

            Export = [
                      %% Essential fields
                      {id, Id},
                      {uri, m_rsc:p(Id, uri, Context)},
                      %% Parts
                      {rsc, Rsc},
                      {medium, Medium},
                      {category, Category},
                      {edges, Edges}
                     ],

            %% Filter empty lists
            lists:filter(fun({_,L}) -> not(L == []) end, Export)
    end;

full(Id, Context) ->
    full(m_rsc:rid(Id, Context), Context).


%% @doc Given an edge record, add the resource uris for the object and the predicate.
edge_details(Edge, Context) ->
    Edge ++ [{predicate_uri, m_rsc:p(proplists:get_value(predicate_id, Edge), uri, Context)},
             {predicate_title, m_rsc:p(proplists:get_value(predicate_id, Edge), title, Context)},
             {object_uri, m_rsc:p(proplists:get_value(object_id, Edge), uri, Context)},
             {object_title, m_rsc:p(proplists:get_value(object_id, Edge), title, Context)}].


%% Simple export with limited information (e.g. for atom feeds)
simple(_, _) ->
    throw({error, {not_implemented}}).
