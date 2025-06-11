%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023-2025 Marc Worrell
%% @doc Generate the JSON-LD for a webpage.
%% The type of the generated JSON-LD is https://schema.org/WebPage
%% @end

%% Copyright 2023-2025 Marc Worrell
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

-module(seo_jsonld_webpage).

-export([
    generate/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Generate the JSON-LD map for webpage view of the given resource.
-spec generate(Id, Context) -> {ok, JSON} | {error, Reason} when
    Id :: m_rsc:resource(),
    Context :: z:context(),
    JSON :: map(),
    Reason :: term().
generate(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case m_rsc:exists(RId, Context) of
                true ->
                    case z_acl:rsc_visible(RId, Context) of
                        true ->
                            generate_1(RId, Context);
                        false ->
                            {error, eacces}
                    end;
                false ->
                    {error, enoent}
            end
    end.

generate_1(Id, Context) ->
    CustomJSON = case z_trans:lookup_fallback(m_rsc:p(Id, <<"seo_ld_json">>, Context), Context) of
        undefined ->
            #{};
        <<>> ->
            #{};
        SeoJSON ->
            try
                case jsxrecord:decode(z_html:unescape(SeoJSON)) of
                    #{ <<"@context">> := _ } = SeoJSONParsed ->
                        SeoJSONParsed;
                    SeoJSONParsed when is_map(SeoJSONParsed) ->
                        ensure_schema(SeoJSONParsed);
                    _ ->
                        #{}
                end
            catch
                _:_ -> #{}
            end
    end,
    generate_2(Id, CustomJSON, Context).

generate_2(_Id, #{ <<"@context">> := _ } = CustomJSON, _Context) ->
    {ok, CustomJSON};
generate_2(Id, CustomJSON, Context) ->
    PageUrl = m_rsc:p(Id, <<"page_url_abs">>, Context),
    Title = title(Id, Context),
    Description = description(Id, Context),
    SiteTitle = z_convert:to_binary(m_config:get_value(site, title, Context)),
    SiteDescription = first([site_organization, page_about, page_home, home], fun description/2, Context),
    HomeUrl = z_context:abs_url(<<"/">>, Context),
    OrgUrl = first([site_organization, page_home, home], fun url_nolang/2, Context),
    OrgId = case OrgUrl of
        HomeUrl ->
            <<HomeUrl/binary, "#organization">>;
        _ ->
            OrgUrl
    end,
    OrgTitle = case title(site_organization, Context) of
        undefined -> SiteTitle;
        <<>> -> SiteTitle;
        OrgT -> OrgT
    end,
    {ok, RscDoc} = m_rdf:summary_trans(Id, Context),
    JSONPublisher = #{
        <<"@type">> => <<"schema:Organization">>,
        <<"@id">> => OrgId,
        <<"schema:url">> => OrgUrl,
        <<"schema:name">> => OrgTitle,
        <<"schema:description">> => z_html:unescape(SiteDescription)
    },
    JSONWebsite0 = #{
        <<"@type">> => <<"schema:WebSite">>,
        <<"@id">> => HomeUrl,
        <<"schema:url">> => HomeUrl,
        <<"schema:name">> => z_convert:to_binary(m_config:get_value(site, title, Context)),
        <<"schema:description">> => z_html:unescape(SiteDescription),
        <<"schema:publisher">> => OrgId
    },
    JSONWebsite = case search_action(Context) of
        [] ->
            JSONWebsite0;
        Action ->
            JSONWebsite0#{
                <<"schema:potentialAction">> => lists:flatten([Action])
            }
    end,
    JSONDoc =  #{
        <<"@type">> => <<"schema:WebPage">>,
        <<"@id">> => PageUrl,
        <<"schema:url">> => PageUrl,
        <<"schema:name">> => z_html:unescape(Title),
        <<"schema:description">> => z_html:unescape(Description),
        <<"schema:inLanguage">> => z_context:language(Context),
        <<"schema:publisher">> => OrgId,
        <<"schema:datePublished">> => case m_rsc:p(Id, <<"org_pubdate">>, Context) of
            undefined -> m_rsc:p_no_acl(Id, <<"publication_start">>, Context);
            OrgPubDate -> OrgPubDate
        end,
        <<"schema:dateCreated">> => m_rsc:p_no_acl(Id, <<"created">>, Context),
        <<"schema:dateModified">> => m_rsc:p_no_acl(Id, <<"modified">>, Context),
        <<"schema:about">> => maps:remove(<<"@context">>, RscDoc),
        <<"schema:image">> => maps:get(<<"schema:image">>, RscDoc, undefined)
    },
    JSONDoc1 = primary_image(Id, JSONDoc, Context),
    JSONDoc2 = case seo_breadcrumb:find(Id, Context) of
        {ok, []} ->
            JSONDoc1;
        {ok, Breadcrumbs} ->
            JSONDoc1#{
                <<"schema:breadcrumb">> => make_breadcrumbs(Breadcrumbs, Context)
            }
    end,
    JSONDoc3 = case visible(m_edge:subjects(Id, about, Context), Context) of
        [] ->
            JSONDoc2;
        About ->
            AboutItems = lists:map(fun(SubjId) -> make_item(SubjId, Context) end, About),
            JSONDoc2#{
                <<"schema:subjectOf">> => AboutItems
            }
    end,
    JSONDoc4 = case visible(m_edge:subjects(Id, haspart, Context), Context) of
        [] ->
            JSONDoc3;
        IsPartOf ->
            IsPartOfItems = lists:map(fun(SubjId) -> make_item(SubjId, Context) end, IsPartOf),
            JSONDoc3#{
                <<"schema:isPartOf">> => IsPartOfItems
            }
    end,
    JSONDoc5 = maps:merge(JSONDoc4, CustomJSON),
    JSON = #{
        <<"@context">> => maps:get(<<"@context">>, RscDoc),
        <<"@graph">> => [
            JSONPublisher,
            JSONWebsite,
            JSONDoc5
        ]
    },
    {ok, JSON}.

visible(Ids, Context) ->
    lists:filter(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids).

ensure_schema(Map) when is_map(Map) ->
    maps:fold(
        fun
            (<<"@", _/binary>> = K, V, Acc) ->
                Acc#{ K => ensure_schema(V) };
            (K, V, Acc) ->
                V1 = ensure_schema(V),
                case binary:match(K, <<":">>) of
                    nomatch ->
                        Acc#{ <<"schema:", K/binary>> => V1 };
                    _ ->
                        Acc#{ K => V1 }
                end
        end,
        #{},
        Map);
ensure_schema(V) ->
    V.

make_breadcrumbs(Breadcrumbs, Context) ->
    lists:map(
        fun(Breadcrumb) -> make_breadcrumb(Breadcrumb, Context) end,
        Breadcrumbs).

make_breadcrumb(Ids, Context) ->
    {_, List} = lists:foldl(
        fun
            (page_home, {Pos, Acc}) ->
                AbsUrl = case m_rsc:p(homepage(Context), page_url_abs, Context) of
                    undefined -> z_context:abs_url(<<"/">>, Context);
                    Url -> Url
                end,
                Elt = #{
                    <<"@type">> => <<"schema:ListItem">>,
                    <<"schema:position">> => Pos,
                    <<"schema:item">> => #{
                        <<"@type">> => <<"schema:WebPage">>,
                        <<"@id">> => AbsUrl
                    },
                    <<"schema:name">> => z_convert:to_binary(m_config:get_value(site, title, Context))
                 },
                {Pos+1, [Elt|Acc]};
            (Id, {Pos, Acc}) ->
                AbsUrl = m_rsc:p(Id, page_url_abs, Context),
                Elt = #{
                    <<"@type">> => <<"schema:ListItem">>,
                    <<"schema:position">> => Pos,
                    <<"schema:item">> => #{
                        <<"@type">> => <<"schema:WebPage">>,
                        <<"@id">> => AbsUrl
                    },
                    <<"schema:name">> => z_html:unescape(title(Id, Context))
                },
                {Pos+1, [Elt|Acc]}
        end,
        {1, []},
        Ids),
    #{
        <<"@type">> => <<"schema:BreadcrumbList">>,
        <<"schema:itemListElement">> => lists:reverse(List)
    }.

make_item(Id, Context) ->
    AbsUrl = m_rsc:p(Id, <<"page_url_abs">>, Context),
    #{
        <<"@type">> => <<"schema:WebPage">>,
        <<"@id">> => AbsUrl,
        <<"schema:name">> => z_html:unescape(title(Id, Context))
    }.

homepage(Context) ->
    case m_rsc:rid(page_home, Context) of
        undefined -> m_rsc:rid(home, Context);
        RId -> RId
    end.

first([], _Fun, _Context) ->
    undefined;
first([Id|Ids], Fun, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            first(Ids, Fun, Context);
        RId ->
            case Fun(RId, Context) of
                undefined -> first(Ids, Fun, Context);
                <<>> -> first(Ids, Fun, Context);
                Text -> Text
            end
    end.

url_nolang(Id, Context) ->
    m_rsc:p(Id, <<"page_url_abs">>, z_context:set_language('x-default', Context)).


title(Id, Context) ->
    SeoTitle = z_trans:lookup_fallback(m_rsc:p(Id, <<"seo_title">>, Context), Context),
    case z_utils:is_empty(SeoTitle) of
        true ->
            Title = non_empty(m_rsc:p(Id, <<"title">>, Context)),
            case z_trans:lookup_fallback(Title, Context) of
                Empty when Empty =:= <<>>; Empty =:= undefined ->
                    z_trans:trans(<<"Untitled">>, Context);
                T ->
                    T
            end;
        false ->
            SeoTitle
    end.

non_empty(#trans{ tr = Tr }) ->
    Tr1 = [ Trans || Trans = {_Iso, Text} <- Tr, Text =/= <<>> ],
    #trans{ tr = Tr1 };
non_empty(undefined) ->
    <<>>;
non_empty(Text) ->
    Text.


description(Id, Context) ->
    SeoDesc = z_trans:lookup_fallback(m_rsc:p(Id, <<"seo_desc">>, Context), Context),
    Desc = case z_utils:is_empty(SeoDesc) of
        true ->
            z_trans:lookup_fallback(filter_summary:summary(Id, Context), Context);
        false ->
            SeoDesc
    end,
    case Desc of
        undefined -> undefined;
        <<>> -> <<>>;
        _ -> filter_brlinebreaks:brlinebreaks(Desc, Context)
    end.

primary_image(Id, JSONDoc, Context) ->
    Depiction = m_media:depiction(Id, Context),
    case z_media_tag:attributes(
        Depiction,
        [{mediaclass, <<"schema-org-image">>}],
        Context)
    of
        {ok, Attrs} ->
            ImgSrcId = maps:get(<<"id">>, Depiction, undefined),
            ImgSrcUrl = z_context:abs_url(proplists:get_value(src, Attrs), Context),
            ImgNodeId = node_id(Id, <<"#primaryImageOfPage">>, Context),
            ImgTitle = title(ImgSrcId, Context),
            ImgCaption = case proplists:get_value(alt, Attrs) of
                undefined -> ImgTitle;
                <<>> -> ImgTitle;
                ImgC -> ImgC
            end,
            JSONDoc#{
                <<"schema:primaryImageOfPage">> => #{
                    <<"@type">> => <<"schema:ImageObject">>,
                    <<"@id">> => ImgNodeId,
                    <<"schema:contentUrl">> => ImgSrcUrl,
                    <<"schema:url">> => ImgSrcUrl,
                    <<"schema:width">> => proplists:get_value(width, Attrs),
                    <<"schema:height">> => proplists:get_value(height, Attrs),
                    <<"schema:caption">> => ImgCaption,
                    <<"schema:name">> => ImgTitle,
                    <<"schema:description">> => ImgTitle
                }
            };
        {error, _} ->
            JSONDoc
    end.

node_id(Id, Hash, Context) ->
    Url = z_dispatcher:url_for(id, [ {id, Id} ], Context),
    iolist_to_binary([
        z_context:abs_url(Url, Context),
        Hash
    ]).

search_action(Context) ->
    case search_url(Context) of
        <<>> ->
            [];
        Url ->
            #{
                <<"@type">> => <<"schema:SearchAction">>,
                <<"schema:target">> => #{
                    <<"@type">> => <<"schema:EntryPoint">>,
                    <<"schema:urlTemplate">> => z_context:abs_url(Url, Context)
                },
                <<"schema:query-input">> => <<"required name=text">>
            }
    end.

search_url(Context) ->
    case m_config:get_boolean(seo, search_action_hide, Context) of
        true ->
            <<>>;
        false ->
            case z_convert:to_binary(m_config:get_value(seo, search_action_url, Context)) of
                <<>> ->
                    % Can't inject {text} immediately as it would be URL encoded.
                    Url = z_convert:to_binary(z_dispatcher:url_for(search, [ {qs, <<"TEXT">>} ], Context)),
                    binary:replace(Url, <<"TEXT">>, <<"{text}">>);
                Url ->
                    Url
            end
    end.

