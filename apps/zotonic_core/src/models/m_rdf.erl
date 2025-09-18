%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Model for resource representation as JSON-LD RDF data.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(m_rdf).
-moduledoc("
Todo

Not yet documented.
").
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-export([
    m_get/3,

    summary/2,
    summary_trans/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"rsc">>, <<"summary">>, <<"trans">>, Id | Rest ], _Msg, Context) ->
    case summary_trans(Id, Context) of
        {ok, Doc} ->
            {ok, {Doc, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"rsc">>, <<"summary">>, Id | Rest ], _Msg, Context) ->
    case summary(Id, Context) of
        {ok, Doc} ->
            {ok, {Doc, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.


-spec summary(Id, Context) -> {ok, Document} | {error, Reason} when
    Id :: m_rsc:resource(),
    Context :: z:context(),
    Document :: map(),
    Reason :: term().
summary(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_visible(RscId, Context) of
                true -> {ok, summary_1(RscId, false, true, Context)};
                false -> {error, eacces}
            end
    end.

-spec summary_trans(Id, Context) -> {ok, Document} | {error, Reason} when
    Id :: m_rsc:resource(),
    Context :: z:context(),
    Document :: map(),
    Reason :: term().
summary_trans(Id, Context) ->
    ContextLang = z_context:set_language(content_language(Id, Context), Context),
    case m_rsc:rid(Id, ContextLang) of
        undefined ->
            {error, enoent};
        RscId ->
            case z_acl:rsc_visible(RscId, ContextLang) of
                true -> {ok, summary_1(RscId, true, true, ContextLang)};
                false -> {error, eacces}
            end
    end.

sub_summary(Id, IsTransFallback, Context) ->
    Doc = summary_1(Id, IsTransFallback, false, Context),
    maps:remove(<<"@context">>, Doc).

summary_1(Id, IsTransFallback, IsTopDoc, Context) ->
    Type = base_type(Id, Context),
    Summary = filter_brlinebreaks:brlinebreaks(filter_summary:summary(Id, Context), Context),
    DocId = case IsTransFallback of
        true -> z_context:abs_url(z_dispatcher:url_for(id, [ {id, Id} ], Context), Context);
        false -> m_rsc:uri(Id, Context)
    end,
    Doc = #{
        <<"@context">> => zotonic_rdf:namespaces(),
        <<"@id">> => DocId,
        <<"@type">> => Type,
        <<"schema:name">> => trans(Id, <<"title">>, fun z_html:unescape/1, IsTransFallback, Context),
        <<"schema:description">> => trans_1(Summary, fun z_html:unescape/1, IsTransFallback, Context),
        <<"schema:url">> => m_rsc:p(Id, <<"page_url_abs">>, Context)
    },
    Doc1 = remove_undef(maps:merge(Doc, type_props(Type, Id, IsTransFallback, IsTopDoc, Context))),
    % TODO: notification to let modules add extra information.
    Doc1.

base_type(Id, Context) ->
    Type = lists:foldr(
        fun
            (Cat, undefined) -> base_type(Cat);
            (_, Type) -> Type
        end,
        undefined,
        m_rsc:is_a(Id, Context)),
    case Type of
        undefined -> <<"schema:CreativeWork">>;
        <<"schema:CreativeWork">> -> Type;
        <<"schema:Article">> -> Type;
        <<"schema:NewsArticle">> -> Type;
        <<"schema:MediaObject">> -> Type;
        _ -> [ Type, <<"schema:CreativeWork">> ]
    end.

base_type(person) -> <<"schema:Person">>;
base_type(institution) -> <<"schema:Organization">>;
base_type(institute) -> <<"schema:Organization">>;
base_type(organization) -> <<"schema:Organization">>;
base_type(article) -> <<"schema:Article">>;
base_type(text) -> <<"schema:Article">>;
base_type(news) -> <<"schema:NewsArticle">>;
base_type(artifact) -> <<"schema:CreativeWork">>;
base_type(media) -> <<"schema:MediaObject">>;
base_type(event) -> <<"schema:Event">>;
base_type(location) -> <<"schema:Place">>;
base_type(_) -> undefined.

type_props(Types, Id, IsTransFallback, IsTopDoc, Context) when is_list(Types) ->
    lists:foldl(
        fun(T, Acc) ->
            Ps = type_props(T, Id, IsTransFallback, IsTopDoc, Context),
            maps:merge(Acc, Ps)
        end,
        #{},
        Types);
type_props(<<"schema:Article">>, Id, IsTransFallback, IsTopDoc, Context) ->
    % https://developers.google.com/search/docs/appearance/structured-data/article
    Doc = #{
        <<"schema:headline">> => trans(Id, <<"title">>, fun z_html:unescape/1, IsTransFallback, Context),
        <<"schema:image">> => images(Id, Context)
    },
    maps:merge(creative_work(Id, IsTransFallback, IsTopDoc, Context), Doc);
type_props(<<"schema:NewsArticle">>, Id, IsTransFallback, IsTopDoc, Context) ->
    % https://schema.org/NewsArticle
    Doc = type_props(<<"schema:Article">>, Id, IsTransFallback, IsTopDoc, Context),
    Doc;
type_props(<<"schema:Person">>, Id, _IsTransFallback, _IsTopDoc, Context) ->
    {Name, _} = z_template:render_to_iolist("_name.tpl", [ {id, Id} ], Context),
    #{
        <<"schema:name">> => unesc(iolist_to_binary(Name)),
        <<"schema:birthDate">> => m_rsc:p(Id, <<"date_start">>, Context),
        <<"schema:deathDate">> => m_rsc:p(Id, <<"date_end">>, Context),
        <<"schema:givenName">> => unesc(m_rsc:p(Id, <<"name_first">>, Context)),
        <<"schema:familyName">> => unesc(family_name(Id, Context)),
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:address">> => address(Id, Context),
        <<"schema:image">> => images(Id, Context)
    };
type_props(<<"schema:Organization">>, Id, _IsTransFallback, _IsTopDoc, Context) ->
    #{
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:address">> => address(Id, Context),
        <<"schema:image">> => images(Id, Context)
    };
type_props(<<"schema:Event">>, Id, IsTransFallback, _IsTopDoc, Context) ->
    Location = place(Id, IsTransFallback, Context),
    #{
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => m_rsc:p(Id, <<"phone">>, Context),
        <<"schema:location">> => Location,
        <<"schema:image">> => images(Id, Context),
        <<"schema:startDate">> => m_rsc:p(Id, <<"date_start">>, Context),
        <<"schema:endDate">> => m_rsc:p(Id, <<"date_end">>, Context)
    };
type_props(<<"schema:Place">>, Id, _IsTransFallback, _IsTopDoc, Context) ->
    #{
        <<"schema:address">> => address(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:image">> => images(Id, Context)
    };
type_props(<<"schema:MediaObject">>, Id, IsTransFallback, IsTopDoc, Context) ->
    Doc = case media_object(Id, Context) of
        undefined -> #{};
        Img -> Img
    end,
    maps:merge(creative_work(Id, IsTransFallback, IsTopDoc, Context), Doc);
type_props(_, Id, IsTransFallback, IsTopDoc, Context) ->
    Doc = #{
        <<"schema:image">> => images(Id, Context)
    },
    maps:merge(creative_work(Id, IsTransFallback, IsTopDoc, Context), Doc).

creative_work(Id, IsTransFallback, IsTopDoc, Context) ->
    % TODO: Add: o.keywords
    Doc = #{
        <<"schema:datePublished">> => case m_rsc:p(Id, <<"org_pubdate">>, Context) of
            undefined -> m_rsc:p_no_acl(Id, <<"publication_start">>, Context);
            OrgPubDate -> OrgPubDate
        end,
        <<"schema:dateCreated">> => m_rsc:p_no_acl(Id, <<"created">>, Context),
        <<"schema:dateModified">> => m_rsc:p_no_acl(Id, <<"modified">>, Context)
    },
    Doc1 = case IsTopDoc of
        true ->
            Doc#{
                <<"schema:author">> => authors(Id, IsTransFallback, Context)
            };
        false ->
            Doc
    end,
    case IsTransFallback of
        true ->
            Doc1#{
                <<"schema:inLanguage">> => z_context:language(Context)
            };
        false ->
            Doc1
    end.

content_language(Id, Context) ->
    Translations = case m_rsc:p_no_acl(Id, language, Context) of
        undefined -> [];
        Lngs -> Lngs
    end,
    z_trans:lookup_fallback_languages(Translations, Context).

family_name(Id, Context) ->
    case m_rsc:p(Id, <<"name_surname_prefix">>, Context) of
        undefined ->
            m_rsc:p(Id, <<"name_surname">>, Context);
        Prefix ->
            iolist_to_binary([
                Prefix,
                " ",
                z_convert:to_binary(m_rsc:p(Id, <<"name_surname">>, Context))
            ])
    end.


% TODO: add attribution.
% - creator; or
% - creditText; or
% - license; or
% - copyrightNotice
% https://developers.google.com/search/docs/appearance/structured-data/image-license-metadata#structured-data-type-definitions
% If these are added, then the image is accepted by Google for search results.
media_object(Id, Context) ->
    Depiction = m_media:depiction(Id, Context),
    case z_media_tag:attributes(
        Depiction,
        [{mediaclass, <<"schema-org-image">>}],
        Context)
    of
        {ok, Attrs} ->
            ImgSrcId = maps:get(<<"id">>, Depiction, undefined),
            ImgSrcUrl = z_context:abs_url(proplists:get_value(src, Attrs), Context),
            ImgTitle = title(ImgSrcId, Context),
            ImgCaption = case proplists:get_value(alt, Attrs) of
                undefined -> ImgTitle;
                <<>> -> ImgTitle;
                ImgC -> ImgC
            end,
            #{
                <<"schema:contentUrl">> => ImgSrcUrl,
                <<"schema:url">> => ImgSrcUrl,
                <<"schema:width">> => proplists:get_value(width, Attrs),
                <<"schema:height">> => proplists:get_value(height, Attrs),
                <<"schema:caption">> => ImgCaption,
                <<"schema:name">> => ImgTitle,
                <<"schema:description">> => ImgTitle,
                <<"schema:inLanguage">> => z_context:language(Context)
            };
        {error, _} ->
            undefined
    end.

%% Provide links to multiple image formats.
%% https://developers.google.com/search/docs/appearance/structured-data/article#article-types
images(Id, Context) ->
    case m_media:depiction(Id, Context) of
        undefined ->
            undefined;
        Depiction ->
            case lists:filtermap(
                fun(MediaClass) ->
                    Opts = [
                        {mediaclass, MediaClass}
                    ],
                    case z_media_tag:url(Depiction, Opts, Context) of
                        {ok, Url} ->
                            {true, z_context:abs_url(Url, Context)};
                        {error, _} ->
                            false
                    end
                end,
                [
                    <<"schema-org-1x1">>,
                    <<"schema-org-4x3">>,
                    <<"schema-org-16x9">>
                ])
            of
                [] -> undefined;
                Imgs -> Imgs
            end
    end.


title(Id, Context) ->
    z_trans:lookup_fallback(m_rsc:p(Id, <<"title">>, Context), Context).

authors(Id, IsTransFallback, Context) ->
    lists:filtermap(
        fun(AuthorId) ->
            case z_acl:rsc_visible(AuthorId, Context) of
                true ->
                    {true, sub_summary(AuthorId, IsTransFallback, Context)};
                false ->
                    false
            end
        end,
        m_edge:objects(Id, author, Context)).

%% @doc Email of a resource is preferred to be the public mail_email property, and then the mail property.
email(Id, Context) ->
    E = case m_rsc:p(Id, <<"mail_email">>, Context) of
        undefined -> m_rsc:p(Id, <<"mail">>, Context);
        Email -> Email
    end,
    unesc(E).


place(Id, IsTransFallback, Context) ->
    case m_edge:objects(Id, haslocation, Context) of
        [] ->
            #{
                <<"@type">> => <<"schema:Place">>,
                <<"schema:address">> => address(Id, Context),
                <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
                <<"schema:name">> => <<>>
            };
        Locs ->
            lists:filtermap(
                fun(LocId) ->
                    case z_acl:rsc_visible(LocId, Context) of
                        true ->
                            Doc = sub_summary(LocId, IsTransFallback, Context),
                            {true, Doc};
                        false ->
                            false
                    end
                end,
                Locs)
    end.

%% @doc Address of a resource is preferred to be the public mailing address, and then the normal address.
address(Id, Context) ->
    Doc = case m_rsc:p(Id, <<"mail_country">>, Context) of
        undefined ->
            case m_rsc:p(Id, <<"address_country">>, Context) of
                undefined ->
                    #{};
                Country ->
                    #{
                        <<"@type">> => <<"schema:PostalAddress">>,
                        <<"schema:addressCountry">> => Country,
                        <<"schema:postalCode">> => unesc(m_rsc:p(Id, <<"address_postcode">>, Context)),
                        <<"schema:streetAddress">> => unesc(m_rsc:p(Id, <<"address_street_1">>, Context)),
                        <<"schema:addressLocality">> => unesc(m_rsc:p(Id, <<"address_city">>, Context)),
                        <<"schema:addressRegion">> => unesc(m_rsc:p(Id, <<"address_state">>, Context)),
                        <<"schema:email">> => email(Id, Context)
                    }
            end;
        Country ->
            #{
                <<"@type">> => <<"schema:PostalAddress">>,
                <<"schema:addressCountry">> => Country,
                <<"schema:postalCode">> => unesc(m_rsc:p(Id, <<"mail_postcode">>, Context)),
                <<"schema:streetAddress">> => unesc(m_rsc:p(Id, <<"mail_street_1">>, Context)),
                <<"schema:addressLocality">> => unesc(m_rsc:p(Id, <<"mail_city">>, Context)),
                <<"schema:addressRegion">> => unesc(m_rsc:p(Id, <<"mail_state">>, Context)),
                <<"schema:email">> => email(Id, Context)
            }
    end,
    remove_undef(Doc).

remove_undef(M) when is_map(M) ->
    maps:fold(
        fun
            (_K, undefined, Acc) ->
                Acc;
            (_K, null, Acc) ->
                Acc;
            (K, V, Acc) ->
                Acc#{ K => V }
        end,
        #{},
        M).

unesc(#trans{} = V) -> z_html:unescape(V);
unesc(V) when is_binary(V) -> z_html:unescape(V);
unesc(V) -> V.


trans(Id, Prop, F, IsTransFallback, Context) ->
    trans_1(m_rsc:p(Id, Prop, Context), F, IsTransFallback, Context).


trans_1(undefined, _F, _IsTransFallback, _Context) ->
    undefined;
trans_1(V, F, _IsTransFallback, _Context) when is_binary(V) ->
    F(V);
trans_1(#trans{ tr = [{_,V}] }, F, _IsTransFallback, _Context) ->
    F(V);
trans_1(#trans{ tr = _ } = Trans, F, true, Context) ->
    V = z_trans:lookup_fallback(Trans, Context),
    F(V);
trans_1(#trans{ tr = Tr }, F, false, _Context) ->
            lists:map(
                fun({Code, V}) ->
                    #{
                        <<"@language">> => Code,
                        <<"@value">> => F(V)
                    }
                end,
                Tr);
trans_1(V, _F, _IsTransFallback, _Context) ->
    V.
