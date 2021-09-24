%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2021 Arjan Scherpenisse
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
%% #{
%%    <<"id">> := 112233,
%%    <<"uri">> := <<"http://www.example.com/id/112233">>},
%%    <<"is_a">> := [ <<"text">>, <<"article">> ],
%%    <<"resource">> := #{
%%          %% Resource properties, e.g.:
%%          title := <<"Foo">>,
%%          ... more properties
%%    },
%%    <<"medium">> := #{
%%          %% Medium properties, if the item has an embedded medium record.
%%    },
%%    <<"edges">> := #{
%%      %% Edges from this item to other items
%%      <<"depiction">> => #{
%%          <<"predicate">> => #{
%%                <<"id">> => 304,
%%                <<"is_a">> => [ <<"meta">>, <<"predicate">> ],
%%                <<"name">> => <<"depiction">>,
%%                <<"title">> => {trans,[{en,<<"Depiction">>}]},
%%                <<"uri">> => <<"http://xmlns.com/foaf/0.1/depiction">>
%%          },
%%          <<"objects">> => [
%%                #{
%%                    <<"created">> => {{2020,12,23},{15,4,55}},
%%                    <<"object_id">> => #{
%%                         <<"id">> => 28992,
%%                         <<"is_a">> => [ <<"media">>, <<"image">> ],
%%                         <<"name">> => undefined,
%%                         <<"title">> =>
%%                            {trans,[{nl,<<"NL: a.jpg">>},{en,<<"a.jpg">>}]},
%%                         <<"uri">> =>
%%                             <<"https://learningstone.test:8443/id/28992">>
%%                    },
%%                    <<"seq">> => 1
%%                },
%%                ... more objects
%%          ]
%%      },
%%      ... more predicates
%%    }
%% }


-module(m_rsc_export).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(zotonic_model).

-export([
    m_get/3,
    full/2,

    is_id_prop/1
]).

-include_lib("../../include/zotonic.hrl").

m_get([ <<"full">>, Id | Rest ], _Msg, Context) ->
    case full(Id, Context) of
        {ok, Export} ->
            {ok, {Export, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ Id | Rest ], _Msg, Context) ->
    case full(Id, Context) of
        {ok, Export} ->
            {ok, {Export, Rest}};
        {error, _} = Error ->
            Error
    end.

%% @doc Get the full representation of a resource.
-spec full( m_rsc:resource(), z:context() ) -> {ok, map()} | {error, term()}.
full(undefined, _Context) ->
    {error, enoent};
full(Id, Context) when is_integer(Id) ->
    case m_rsc:get(Id, Context) of
        undefined ->
            % Access denied or not found
            case m_rsc:exists(Id, Context) of
                true ->
                    {error, eacces};
                false ->
                    {error, enoent}
            end;
        Rsc0 ->
            ContextNoLang = z_context:set_language('x-default', Context),

            Rsc1 = filter_empty(Rsc0),
            Rsc = replace_ids_with_uris(Rsc1, ContextNoLang),
            Medium = m_media:get(Id, ContextNoLang),
            DepictionUrl = depiction_url(m_media:depiction(Id, ContextNoLang), ContextNoLang),
            PreviewUrl = preview_url(Medium, ContextNoLang),
            DownloadUrl = download_url(Medium, ContextNoLang),

            % URL used as the template for urls to export other resource ids.
            BaseUri0 = z_context:abs_url( z_dispatcher:url_for(id, [ {id, <<"ID">>} ], ContextNoLang), ContextNoLang),
            BaseUri = binary:replace(BaseUri0, <<"/ID">>, <<"/:id">>),

            Export = #{
                %% Essential fields
                <<"id">> => Id,
                <<"name">> => m_rsc:p(Id, <<"name">>, Context),
                <<"is_a">> => [ z_convert:to_binary(A) || A <- m_rsc:is_a(Id, Context) ],
                <<"uri">> => m_rsc:uri(Id, ContextNoLang),
                <<"uri_template">> => BaseUri,

                %% Parts
                <<"resource">> => Rsc,
                <<"medium">> => Medium,
                <<"medium_url">> => DownloadUrl,
                <<"preview_url">> => PreviewUrl,
                <<"depiction_url">> => DepictionUrl,
                <<"edges">> => edges(Id, ContextNoLang)
            },

            %% Filter empty values
            Export1 = filter_empty(privacy_filter(Id, Export, ContextNoLang)),
            {ok, Export1}
    end;
full(Id, Context) ->
    full(m_rsc:rid(Id, Context), Context).


%% @doc Return the list of all outgoing edges, remove edges where the predicate
%% or object-resource is not visible.
edges(Id, Context) ->
    Edges = m_edge:get_edges(Id, Context),
    lists:foldl(
        fun
            ({hasusergroup, _Es}, Acc) ->
                % Do not expose the groups an user is member of.
                % TODO: make this configurable.
                Acc;
            ({Pred, Es}, Acc) when is_atom(Pred) ->
                case z_acl:rsc_visible(Pred, Context) of
                    true ->
                        PredRsc = related_rsc(Pred, Context),
                        PredB = z_convert:to_binary(Pred),
                        Os = lists:filtermap(
                            fun(E) ->
                                ObjId = proplists:get_value(object_id, E),
                                case z_acl:rsc_visible(ObjId, Context) of
                                    true ->
                                        {true, #{
                                            <<"object_id">> => related_rsc(ObjId, Context),
                                            <<"seq">> => proplists:get_value(seq, E),
                                            <<"created">> => proplists:get_value(created, E)
                                        }};
                                    false ->
                                        false
                                end
                            end,
                            Es),
                        Acc#{
                            PredB => #{
                                <<"predicate">> => PredRsc,
                                <<"objects">> => Os
                            }
                        };
                    false ->
                        false
                end
        end,
        #{},
        Edges).


replace_ids_with_uris(Map, Context) when is_map(Map) ->
    maps:fold(
        fun
            (<<"id">>, _V, Acc) ->
                Acc;
            (<<"blocks">>, Blocks, Acc) when is_list(Blocks) ->
                Blocks1 = replace_block_ids_with_uris(Blocks, Context),
                Acc#{
                    <<"blocks">> => Blocks1
                };
            (<<"body", _/binary>> = Body, Text, Acc) ->
                % TODO: replace any embedded id in the body text
                Acc#{
                    Body => Text
                };
            (P, V, Acc) when is_integer(V); is_atom(V); is_binary(V) ->
                case is_id_prop(P) of
                    true ->
                        Acc#{ P => related_rsc(V, Context) };
                    false ->
                        Acc#{ P => V }
                end;
            (P, V, Acc) ->
                Acc#{ P => V }
        end,
        #{},
        Map);
replace_ids_with_uris(V, _Context) ->
    V.

replace_block_ids_with_uris(Blocks, Context) ->
    lists:map(
        fun(B) ->
            B1 = replace_ids_with_uris(B, Context),
            % TODO: replace any embedded id in the body text
            B1
        end,
        Blocks).

% is_id_prop(<<"id">>) -> true;
is_id_prop(<<"rsc_id">>) -> true;
is_id_prop(<<"category_id">>) -> true;
is_id_prop(<<"modifier_id">>) -> true;
is_id_prop(<<"creator_id">>) -> true;
is_id_prop(<<"content_group_id">>) -> true;
is_id_prop(<<"predicate_id">>) -> true;
is_id_prop(<<"object_id">>) -> true;
is_id_prop(<<"subject_id">>) -> true;
is_id_prop(P) ->
    binary:longest_common_suffix([P, <<"_id">>]) =:= 3.


related_rsc(undefined, _Context) ->
    undefined;
related_rsc(Id, Context) when is_integer(Id) ->
    #{
        <<"id">> => Id,
        <<"name">> => m_rsc:p_no_acl(Id, <<"name">>, Context),
        <<"uri">> => m_rsc:uri(Id, Context),
        <<"is_a">> => m_rsc:is_a(Id, Context),
        <<"title">> => m_rsc:p(Id, <<"title">>, Context)
    };
related_rsc(Id, Context) ->
    related_rsc(m_rsc:rid(Id, Context), Context).


% If there is a medium record, then also include a preview url
preview_url(#{ <<"id">> := Id }, Context) ->
    case z_media_tag:url(
        Id,
        [ {width, 800}, {height, 800}, {upscale, true}, {absolute_url, true} ],
        Context)
    of
        {ok, P} -> P;
        _ -> undefined
    end;
preview_url(_, _Context) ->
    undefined.

% If there is a medium record with a file size, then also include a download url
download_url(#{ <<"id">> := Id, <<"size">> := Size }, Context) when Size > 0 ->
    z_dispatcher:url_for(media_attachment, [ {id, Id}, {absolute_url, true} ], Context);
download_url(_, _Context) ->
    undefined.


% Used to make a depiction of the resource, might be the resource itself or an
% attached 'depiction' edge. Useful for external sites to display a preview of the
% resource.
depiction_url(#{ <<"id">> := Id } = Medium, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true -> preview_url(Medium, Context);
        false -> undefined
    end;
depiction_url(undefined, _Context) ->
    undefined.


filter_empty(Map) ->
    maps:filter(
        fun(_K, V) -> not is_empty(V) end,
        Map).

is_empty(#trans{ tr = [] }) ->
    true;
is_empty(#trans{ tr = Tr }) ->
    lists:all(fun({_, V}) -> z_utils:is_empty(V) end, Tr);
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
        <<"email">>
    ],
    lists:foldl(
        fun(P, Acc) ->
            maps:remove(P, Acc)
        end,
        Export,
        Drop
    ).
