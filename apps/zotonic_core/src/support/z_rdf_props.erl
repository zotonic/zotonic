%% @copyright 2021 Driebit BV
%% @doc Extract properties from a compact RDF document encoded by zotonic_rdf.

%% Copyright 2021 Driebit BV
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

-module(z_rdf_props).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    mapping/1,

    extract_resource/2,
    extract_props/1,
    extract_edges/2,

    map_values/1,

    to_simple_value/1
]).

-include("../../include/zotonic.hrl").


%% @doc Check if a property can be mapped to a standard property.
%% This is used in m_rsc:p/3 for fetching namespaced properties.
-spec mapping( binary() ) -> binary() | undefined.
mapping(<<"zotonic:", Prop/binary>>) ->
    Prop;
mapping(Prop) ->
    case maps:get(Prop, mapping(), undefined) of
        undefined ->
            maps:get(Prop, mapping_dates(), undefined);
        Mapped ->
            Mapped
    end.


%% @doc Extract standard Zotonic src import from an RDF document. The document
%% is a "compact" document returned by zotonic_rdf.
-spec extract_resource( RDFDoc, z:context() ) -> {ok, Import} | {error, term()}
    when RDFDoc :: map(),
         Import :: map().
extract_resource(#{ <<"@id">> := Uri } = RDFDoc, Context) when is_binary(Uri) ->
    RDFDoc1 = extract_props(RDFDoc),
    Props1 = RDFDoc1#{
        <<"uri">> => Uri,
        <<"name">> => undefined,
        <<"is_published">> => true,
        <<"is_authoritative">> => true
    },
    Edges = extract_edges(RDFDoc, Context),
    Props2 = maps:without(maps:keys(Edges), Props1),
    {Medium, MediumUrl} = extract_medium(Props2),
    Props3 = maps:without([
            <<"@id">>,
            <<"zotonic:medium_url">>,
            <<"zotonic:medium">>
        ], Props2),
    Rsc = #{
        <<"uri">> => Uri,
        <<"is_a">> => [ maps:get(<<"category_id">>, Props3, <<"other">>) ],
        <<"resource">> => Props3,
        <<"edges">> => Edges,
        <<"medium">> => Medium,
        <<"medium_url">> => MediumUrl
    },
    {ok, Rsc};
extract_resource(_, _) ->
    {error, id}.


%% @doc Extra the medium record for importing an image or other media.
-spec extract_medium( RDFDoc ) -> {Medium, MediumUrl}
    when RDFDoc :: map(),
         Medium :: map() | undefined,
         MediumUrl :: binary() | undefined.
extract_medium(#{ <<"medium_url">> := MediumUrl } = RDFDoc) when MediumUrl =/= <<>> ->
    Created = maps:get(<<"modified">>, RDFDoc,
        maps:get(<<"created">>, RDFDoc, erlang:universaltime())),
    Medium = #{
        <<"created">> => Created
    },
    {Medium, to_value(MediumUrl)};
extract_medium(#{ <<"medium">> := Medium } = RDFDoc) when is_map(Medium) ->
    CreatedDoc = maps:get(<<"modified">>, RDFDoc,
        maps:get(<<"created">>, RDFDoc, erlang:universaltime())),
    CreatedMedium = maps:get(<<"modified">>, Medium,
        maps:get(<<"created">>, Medium,
        maps:get(<<"schema:dateCreated">>, Medium,
        maps:get(<<"dcterms:created">>, Medium,
        CreatedDoc)))),
    Medium1 = Medium#{
        <<"created">> => CreatedMedium
    },
    MediumUrl = maps:get(<<"download_url">>, Medium1, undefined),
    {Medium1, MediumUrl};
extract_medium(_) ->
    {undefined, undefined}.



%% @doc Extract standard Zotonic edges from an RDF document. The document
%% is a "compact" document returned by zotonic_rdf. This collects the @id
%% attributes in the top-level predicates of the document.
-spec extract_edges( RDFDoc, Context ) -> Edges
    when RDFDoc :: map(),
         Edges :: #{ Predicate := [ Edge ]},
         Predicate :: binary(),
         Edge :: map(),
         Context :: z:context().
extract_edges(RDFDoc, Context) ->
    Es = maps:fold(
        fun(K, V, Acc) -> extract_edge(K, V, Acc, Context) end,
        #{},
        RDFDoc),
    maps:fold(
        fun(P, Os, Acc) ->
            Acc#{
                P => #{
                    <<"predicate">> => #{
                        <<"is_a">> => [ <<"meta">>, <<"predicate">> ],
                        <<"uri">> => P
                    },
                    <<"objects">> => Os
                }
            }
        end,
        #{},
        Es).

extract_edge(<<"rdf:type">>, _, Acc, _Context) ->
    Acc;
extract_edge(K, Vs, Acc, Context) when is_list(Vs) ->
    lists:foldr(
        fun(V, VAcc) ->
            extract_edge(K, V, VAcc, Context)
        end,
        Acc,
        Vs);
extract_edge(K, #{ <<"@id">> := Uri }, Acc, Context) ->
    case m_rsc:rid(K, Context) of
        undefined ->
            Acc;
        RId ->
            case m_rsc:is_a(RId, predicate, Context) of
                true ->
                    Es = maps:get(K, Acc, []),
                    Obj = #{
                        <<"object_id">> => #{
                            <<"is_a">> => [ <<"other">> ],
                            <<"uri">> => Uri
                        }
                    },
                    Acc#{ K => lists:flatten([Es, Obj]) };
                false ->
                    Acc
            end
    end;
extract_edge(_K, _, Acc, _Context) ->
    Acc.


%% @doc Extract standard Zotonic properties from an RDF document. The document
%% is a "compact" document returned by zotonic_rdf. All predicates in the
%% document are of the form "namespace:term", for example: "dc:title".
%% The namespaces are the ones defined in zotonic_rdf.
-spec extract_props( RDFDoc ) -> Props
  when RDFDoc :: map(),
       Props :: map().
extract_props(RDFDoc) ->
    Ps1 = map(RDFDoc, mapping_dates(), fun to_date/1, RDFDoc),
    Ps2 = map(RDFDoc, mapping(), fun to_simple_value/1, Ps1),
    maps:fold(
        fun
            (<<"zotonic:", K/binary>>, V, Acc) ->
                Acc#{ K => map_nested_values(V) };
            (K, V, Acc) ->
                Acc#{ K => map_nested_values(V) }
        end,
        #{},
        Ps2).

map(Doc, Mapping, Fun, DocAcc) ->
    maps:fold(
        fun(K, P, Acc) ->
            case maps:find(K, Doc) of
                {ok, V} ->
                    case Fun(V) of
                        error ->
                            Acc;
                        V1 ->
                            Acc1 = maps:remove(K, Acc),
                            Acc1#{ P => V1 }
                    end;
                error ->
                    Acc
            end
        end,
        DocAcc,
        Mapping).


map_nested_values(#{ <<"@id">> := _ } = V) ->
    V;
map_nested_values(#{ <<"@value">> := _ } = V) ->
    case to_value(V) of
        error -> V;
        V1 -> V1
    end;
map_nested_values([ #{ <<"@language">> := _ } | _ ] = V) ->
    case to_value(V) of
        error -> V;
        V1 -> V1
    end;
map_nested_values(V) when is_list(V) ->
    lists:map(fun map_nested_values/1, V);
map_nested_values(V) when is_map(V) ->
    maps:fold(
        fun(K1, V1, Acc) ->
            Acc#{ K1 => map_nested_values(V1) }
        end,
        #{},
        V);
map_nested_values(V) ->
    V.


to_date([ #{ <<"@value">> := V } | _ ]) ->
    try z_datetime:to_datetime(V)
    catch _:_ -> error
    end;
to_date(#{ <<"@value">> := V }) ->
    try z_datetime:to_datetime(V)
    catch _:_ -> error
    end;
to_date([ V | _ ]) when is_binary(V) ->
    try z_datetime:to_datetime(V)
    catch _:_ -> error
    end;
to_date(V) when is_binary(V) ->
    try z_datetime:to_datetime(V)
    catch _:_ -> error
    end;
to_date(_) ->
    error.


%% @doc Translate the given value to a simpler value
-spec to_simple_value( map() ) -> error | term().
to_simple_value(#{ <<"@value">> := Val } = V) when is_binary(Val); is_number(Val); is_boolean(Val) ->
    case to_value(V) of
        error -> Val;
        V1 -> V1
    end;
to_simple_value(V) ->
    to_value(V).


%% @doc Map all values in the RDF doc to values we can handle in the system
map_values(#{ <<"@language">> := _ } = V) ->
    case to_value(V) of
        error -> V;
        V1 -> V1
    end;
map_values([ #{ <<"@language">> := _ } | _ ] = V) ->
    case to_value(V) of
        error -> V;
        V1 -> V1
    end;
map_values([ #{ <<"@value">> := _ } | _ ] = V) ->
    case to_value(V) of
        error -> V;
        V1 -> V1
    end;
map_values(Doc) when is_map(Doc) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{ K => map_values(V) }
        end,
        #{},
        Doc);
map_values(L) when is_list(L) ->
    lists:map(fun map_values/1, L);
map_values(V) ->
    V.


% See https://www.w3.org/TR/xmlschema-2/#built-in-datatypes
%     https://schema.org/DataType
to_value(V) when is_binary(V); is_number(V); is_boolean(V) ->
    V;
to_value(null) ->
    undefined;
to_value(undefined) ->
    undefined;
to_value(#{
        <<"@id">> := URI
    }) ->
    try z_convert:to_binary(URI)
    catch _:_ -> error
    end;
to_value(#{
        <<"@value">> := V,
        <<"@type">> := <<"schema:Number">>
    }) ->
    try z_convert:to_integer(V)
    catch _:_ ->
        try z_convert:to_float(V)
        catch _:_ -> error
        end
    end;
to_value(#{
        <<"@value">> := V,
        <<"@type">> := <<"xsd:integer">>
    }) ->
    try z_convert:to_integer(V)
    catch _:_ -> error
    end;
to_value(#{
        <<"@value">> := V,
        <<"@type">> := T
    })
    when T =:= <<"xsd:boolean">>;
         T =:= <<"schema:Boolean">> ->
    try z_convert:to_bool(V)
    catch _:_ -> error
    end;
to_value(#{
        <<"@value">> := V,
        <<"@type">> := T
    })
    when T =:= <<"xsd:float">>;
         T =:= <<"xsd:double">> ->
    try z_convert:to_float(V)
    catch _:_ -> error
    end;
to_value(#{
        <<"@value">> := V,
        <<"@type">> := T
    })
    when T =:= <<"schema:DateTime">>;
         T =:= <<"schema:Date">>;
         T =:= <<"xsd:datetime">> ->
    to_date(V);
to_value(#{
        <<"@language">> := Lang,
        <<"@value">> := V
    }) ->
    case z_language:to_language_atom(Lang) of
        {ok, LangAtom} ->
            try
                V1 = z_convert:to_binary(V),
                #trans{ tr = simplify_langs([ {LangAtom, V1} ])}
            catch _:_ ->
                error
            end;
        {error, _} ->
            error
    end;
to_value([ #{ <<"@language">> := _ } | _ ] = Vs) ->
    Trans = lists:filtermap(
        fun
            (#{
                <<"@language">> := Lang,
                <<"@value">> := V
            }) ->
                case z_language:to_language_atom(Lang) of
                    {ok, LangAtom} ->
                        try
                            V1 = z_convert:to_binary(V),
                            {true, {LangAtom, V1}}
                        catch _:_ ->
                            false
                        end;
                    {error, _} ->
                        false
                end;
            (_) ->
                false
        end,
        Vs),
    #trans{ tr = simplify_langs(Trans) };
to_value(_) ->
    error.


%% @doc Cleanup languages, map "en-gb" to "en" if there is no base
%% version of that language.
simplify_langs(Tr) ->
    lists:foldl(
        fun({Iso, V}, Acc) ->
            case atom_to_binary(Iso, utf8) of
                <<A,B, $-, _/binary>> ->
                    Base = binary_to_atom(<<A, B>>, utf8),
                    case lists:member(Base, Acc) of
                        false -> [ {Base, V} | Acc ];
                        true -> [ {Iso, V} | Acc ]
                    end;
                _ ->
                    [ {Iso, V} | Acc ]
            end
        end,
        [],
        Tr).


mapping_dates() ->
    #{
        <<"schema:dateCreated">> => <<"created">>,
        <<"schema:dateModified">> => <<"modified">>,
        <<"schema:datePublished">> => <<"publication_start">>,
        <<"schema:startDate">> => <<"date_start">>,
        <<"schema:endDate">> => <<"date_end">>,

        <<"dcterms:created">> => <<"created">>,
        <<"dcterms:modified">> => <<"modified">>
    }.

mapping() ->
    #{
        <<"rdf:type">> => <<"category_id">>,

        <<"schema:givenName">> => <<"name_first">>,
        <<"schema:familyName">> => <<"name_surname">>,
        <<"schema:telephone">> => <<"phone">>,

        <<"schema:addressCountry">> => <<"address_country">>,
        <<"schema:addressRegion">> => <<"address_state">>,
        <<"schema:addressLocality">> => <<"address_city">>,
        <<"schema:streetAddress">> => <<"address_street_1">>,
        <<"schema:postalCode">> => <<"address_postcode">>,
        <<"schema:email">> => <<"email">>,

        <<"schema:license">> => <<"license">>,

        <<"schema:headline">> => <<"title">>,
        <<"schema:subtitle">> => <<"subtitle">>,
        <<"schema:text">> => <<"body">>,
        <<"schema:url">> => <<"website">>,

        <<"dcterms:title">> => <<"title">>,
        <<"dcterms:description">> => <<"summary">>,

        <<"dc:title">> => <<"title">>,
        <<"dc:description">> => <<"summary">>,

        <<"foaf:name">> => <<"title">>,
        <<"foaf:givenName">> => <<"name_first">>,
        <<"foaf:familyName">> => <<"name_surname">>,
        <<"foaf:firstName">> => <<"name_first">>,
        <<"foaf:lastName">> => <<"name_surname">>,
        <<"foaf:gender">> => <<"gender">>,
        <<"foaf:homepage">> => <<"website">>,
        <<"foaf:mbox">> => <<"email">>,
        <<"foaf:phone">> => <<"phone">>,

        <<"geo:lat">> => <<"location_lat">>,
        <<"geo:long">> => <<"location_lng">>
    }.

