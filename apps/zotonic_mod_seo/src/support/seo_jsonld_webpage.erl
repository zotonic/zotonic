%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Generate the JSON-LD for a webpage.
%% The type of the generated JSON-LD is https://schema.org/WebPage
%% @end

%% Copyright 2023 Marc Worrell
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
    SiteDescription = first([site_organization, page_about, page_home, home], fun description/2, Context),
    {ok, RscDoc} = m_rdf:summary(Id, Context),
    JSONDoc =  #{
        <<"@type">> => <<"schema:WebPage">>,
        <<"@id">> => PageUrl,
        <<"schema:url">> => PageUrl,
        <<"schema:name">> => z_html:unescape(Title),
        <<"schema:description">> => z_html:unescape(Description),
        <<"schema:inLanguage">> => z_context:language(Context),
        % <<"schema:potentialAction">> => #{
        %     <<"@type">> => <<"schema:ReadAction">>,
        %     <<"schema:target">> => #{
        %         <<"@type">> => <<"schema:EntryPoint">>,
        %         <<"schema:urlTemplate">> => PageUrl
        %     }
        % },
        <<"schema:datePublished">> => case m_rsc:p(Id, <<"org_pubdate">>, Context) of
            undefined -> m_rsc:p_no_acl(Id, <<"publication_start">>, Context);
            OrgPubDate -> OrgPubDate
        end,
        <<"schema:dateCreated">> => m_rsc:p_no_acl(Id, <<"created">>, Context),
        <<"schema:dateModified">> => m_rsc:p_no_acl(Id, <<"modified">>, Context),
        <<"schema:about">> => maps:remove(<<"@context">>, RscDoc),
        <<"schema:publisher">> => #{
            <<"@type">> => <<"schema:Organization">>,
            <<"schema:url">> => z_context:abs_url(<<"/">>, Context),
            <<"schema:name">> => z_convert:to_binary(m_config:get_value(site, title, Context)),
            <<"schema:description">> => z_html:unescape(SiteDescription)
        }
    },
    JSONDoc1 = case z_media_tag:attributes(
        m_media:depiction(Id, Context),
        [{mediaclass, <<"schema-org-image">>}],
        Context)
    of
        {ok, Attrs} ->
            JSONDoc#{
                <<"schema:primaryImageOfPage">> => #{
                    <<"@type">> => <<"schema:ImageObject">>,
                    <<"schema:url">> => z_context:abs_url(proplists:get_value(src, Attrs), Context),
                    <<"schema:width">> => proplists:get_value(width, Attrs),
                    <<"schema:height">> => proplists:get_value(height, Attrs),
                    <<"schema:caption">> => proplists:get_value(alt, Attrs)
                }
            };
        {error, _} ->
            JSONDoc
    end,
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
    AbsUrl = m_rsc:p(Id, page_url_abs, Context),
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

title(Id, Context) ->
    SeoTitle = z_trans:lookup_fallback(m_rsc:p(Id, <<"seo_title">>, Context), Context),
    case z_utils:is_empty(SeoTitle) of
        true ->
            z_trans:lookup_fallback(m_rsc:p(Id, <<"title">>, Context), Context);
        false ->
            SeoTitle
    end.

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

