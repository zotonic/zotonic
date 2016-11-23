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

%% the rsc_export() format, as returned by m_rsc:export/2
%% [
%%  {id, 112233}, % Local ID
%%  %% Globally unique resource URI
%%  {uri, <<"http://www.example.com/id/112233">>},
%%  {rsc,
%%   %% Resource properties, e.g.:
%%   [{title, <<"Foo">>},
%%    ...
%%   ]
%%  },
%%  {medium,
%%   %% Medium properties, if the item has an embedded medium record.
%%  },
%%  {category,
%%   %% Category properties, if the item is a category.
%%  },
%%  {group,
%%   %% Group access properties (if item is a group)
%%  },
%%  {edges,
%%   %% Edges from this item to other items
%%   [
%%                                                 % Every edge can contain:
%%    [ {id, 32432423} % local edge id
%%      {object_id, 223344}, % local object id
%%      {object_uri, <<"http://...">>},
%%      {object_title, <<"...">>},
%%      {predicate_id, 22}, % local predicate id
%%      {predicate_uri, <<"http://...">>},
%%      {predicate_title, <<"...">>},
%%     ]
%%   ]
%%  }
%% ]


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
            Rsc0 = m_rsc:get_visible(Id, Context),
            Rsc1 = filter_empty(Rsc0),

            Rsc = [
                {category, m_rsc:p(m_rsc:p(Id, category_id, Context), name, Context)},
                {content_group, m_rsc:p(m_rsc:p(Id, content_group_id, Context), name, Context)}
                | Rsc1
            ],

            %% This should probably be encapsulated in m_edges.
            Edges0 = z_db:assoc("
                                select e.predicate_id, p.name as predicate_name, e.object_id, e.seq
                                from edge e
                                        join rsc p on p.id = e.predicate_id
                                where e.subject_id = $1
                                order by e.predicate_id, e.seq, e.id", [Id], Context),
            Edges = [ edge_details(E, Context) || E <- Edges0],
            Medium = m_media:get(Id, Context),

            PreviewUrl = case z_media_tag:url(Id, [{width, 800}, {height, 800}, {upscale, true}, {absolute_url, true}], Context) of
                            {ok, P} -> P;
                            _ -> undefined
                         end,
            Export = [
                %% Essential fields
                {id, Id},
                {uri, m_rsc:p(Id, uri, Context)},

                %% Parts
                {rsc, Rsc},
                {medium, Medium},
                {edges, Edges},
                {preview_url, PreviewUrl}
            ],

            %% Filter empty lists
            filter_empty(privacy_filter(Id, Export, Context))
    end;
full(undefined, _Context) ->
    undefined;
full(Id, Context) ->
    full(m_rsc:rid(Id, Context), Context).


filter_empty(List) ->
    lists:filter(
        fun({_,V}) ->
            not is_empty(V)
        end,
        List).

is_empty({trans, L}) ->
    lists:all(fun({_,V}) -> z_utils:is_empty(V) end, L);
is_empty(V) ->
    z_utils:is_empty(V).

% Rather crude privacy filter - to be fixed in issue 1211
% @todo replace with real privacy control in ACL module
privacy_filter(Id, Export, Context) ->
    case m_rsc:is_a(Id, person, Context) of
        false ->
            Export;
        true ->
            case z_acl:rsc_editable(Id, Context) of
                true -> Export;
                false -> privacy_filter(Export)
            end
    end.

privacy_filter(Export) ->
    Drop = [
        email
    ],
    lists:foldl(
        fun(P, Acc) ->
            proplists:delete(P, Acc)
        end,
        Export,
        Drop
    ).

%% @doc Given an edge record, add the resource uris for the object and the predicate.
edge_details(Edge, Context) ->
    Edge ++ [{predicate_uri, m_rsc:p(proplists:get_value(predicate_id, Edge), uri, Context)},
             {predicate_title, m_rsc:p(proplists:get_value(predicate_id, Edge), title, Context)},
             {object_uri, m_rsc:p(proplists:get_value(object_id, Edge), uri, Context)},
             {object_title, m_rsc:p(proplists:get_value(object_id, Edge), title, Context)}].


%% Simple export with limited information (e.g. for atom feeds)
simple(_, _) ->
    throw({error, {not_implemented}}).
