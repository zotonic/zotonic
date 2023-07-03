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
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).

-export([
    m_get/3,

    summary/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ <<"rsc-summary">>, Id | Rest ], _Msg, Context) ->
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
                true -> {ok, summary_1(RscId, Context)};
                false -> {error, eacces}
            end
    end.

summary_1(Id, Context) ->
    Type = base_type(Id, Context),
    Summary = filter_brlinebreaks:brlinebreaks(filter_summary:summary(Id, Context), Context),
    Doc = #{
        <<"@context">> => zotonic_rdf:namespaces(),
        <<"@id">> => m_rsc:uri(Id, Context),
        <<"@type">> => Type,
        <<"schema:name">> => trans(Id, <<"title">>, fun z_html:unescape/1, Context),
        <<"schema:description">> => trans_1(Summary, fun z_html:unescape/1)
    },
    Doc1 = remove_undef(maps:merge(Doc, type_props(Type, Id, Context))),
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
    if
        Type =:= undefined -> <<"schema:Thing">>;
        true -> Type
    end.

base_type(person) -> <<"schema:Person">>;
base_type(institution) -> <<"schema:Organization">>;
base_type(institute) -> <<"schema:Organization">>;
base_type(organization) -> <<"schema:Organization">>;
base_type(article) -> <<"schema:Article">>;
base_type(text) -> <<"schema:Article">>;
base_type(artifact) -> <<"schema:CreativeWork">>;
base_type(media) -> <<"schema:MediaObject">>;
base_type(event) -> <<"schema:Event">>;
base_type(location) -> <<"schema:PostalAddress">>;
base_type(_) -> undefined.

type_props(<<"schema:Person">>, Id, Context) ->
    #{
        <<"schema:birthDate">> => m_rsc:p(Id, <<"date_start">>, Context),
        <<"schema:deathDate">> => m_rsc:p(Id, <<"date_end">>, Context),
        <<"schema:givenName">> => unesc(m_rsc:p(Id, <<"name_first">>, Context)),
        <<"schema:familyName">> => unesc(family_name(Id, Context)),
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:address">> => location(Id, Context),
        <<"schema:image">> => image(Id, Context)
    };
type_props(<<"schema:Organization">>, Id, Context) ->
    #{
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:address">> => location(Id, Context),
        <<"schema:image">> => image(Id, Context)
    };
type_props(<<"schema:Event">>, Id, Context) ->
    #{
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => m_rsc:p(Id, <<"phone">>, Context),
        <<"schema:location">> => location(Id, Context),
        <<"schema:image">> => image(Id, Context),
        <<"schema:startDate">> => m_rsc:p(Id, <<"date_start">>, Context),
        <<"schema:endDate">> => m_rsc:p(Id, <<"date_end">>, Context)
    };
type_props(<<"schema:PostalAddress">>, Id, Context) ->
    Doc = location(Id, Context),
    Doc#{
        <<"schema:email">> => email(Id, Context),
        <<"schema:telephone">> => unesc(m_rsc:p(Id, <<"phone">>, Context)),
        <<"schema:image">> => image(Id, Context)
    };
type_props(<<"schema:MediaObject">>, Id, Context) ->
    Doc = case image(Id, Context) of
        undefined -> #{};
        Img -> Img
    end,
    maps:merge(creative_work(Id, Context), Doc);
type_props(_, Id, Context) ->
    Doc = #{
        <<"schema:image">> => image(Id, Context)
    },
    maps:merge(creative_work(Id, Context), Doc).

creative_work(Id, Context) ->
    % TODO: Add: o.author, o.keywords
    #{
        <<"schema:datePublished">> => case m_rsc:p(Id, <<"org_pubdate">>, Context) of
            undefined -> m_rsc:p_no_acl(Id, <<"publication_start">>, Context);
            OrgPubDate -> OrgPubDate
        end,
        <<"schema:dateCreated">> => m_rsc:p_no_acl(Id, <<"created">>, Context),
        <<"schema:dateModified">> => m_rsc:p_no_acl(Id, <<"modified">>, Context)
    }.

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

image(Id, Context) ->
    case m_media:depiction(Id, Context) of
        undefined ->
            undefined;
        Depict ->
            case z_media_tag:attributes(Depict, [ {mediaclass, <<"schema-org-image">>} ], Context) of
                {ok, Attrs} ->
                    #{
                        <<"@type">> => <<"schema:ImageObject">>,
                        <<"schema:contentUrl">> => z_context:abs_url(proplists:get_value(src, Attrs), Context),
                        <<"schema:width">> => proplists:get_value(width, Attrs),
                        <<"schema:height">> => proplists:get_value(height, Attrs),
                        <<"schema:caption">> => proplists:get_value(alt, Attrs)
                    };
                {error, _} ->
                    undefined
            end
    end.

%% @doc Email of a resource is preferred to be the public mail_email property, and then the mail property.
email(Id, Context) ->
    E = case m_rsc:p(Id, <<"mail_email">>, Context) of
        undefined -> m_rsc:p(Id, <<"mail">>, Context);
        Email -> Email
    end,
    unesc(E).

%% @doc Address of a resource is preferred to be the public mailing address, and then the normal address.
location(Id, Context) ->
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
                        <<"schema:addressRegion">> => unesc(m_rsc:p(Id, <<"address_state">>, Context))
                    }
            end;
        Country ->
            #{
                <<"@type">> => <<"schema:PostalAddress">>,
                <<"schema:addressCountry">> => Country,
                <<"schema:postalCode">> => unesc(m_rsc:p(Id, <<"mail_postcode">>, Context)),
                <<"schema:streetAddress">> => unesc(m_rsc:p(Id, <<"mail_street_1">>, Context)),
                <<"schema:addressLocality">> => unesc(m_rsc:p(Id, <<"mail_city">>, Context)),
                <<"schema:addressRegion">> => unesc(m_rsc:p(Id, <<"mail_state">>, Context))
            }
    end,
    remove_undef(Doc).

remove_undef(M) when is_map(M) ->
    maps:fold(
        fun
            (_K, undefined, Acc) ->
                Acc;
            (K, V, Acc) ->
                Acc#{ K => V }
        end,
        #{},
        M).

unesc(#trans{} = V) -> z_html:unescape(V);
unesc(V) when is_binary(V) -> z_html:unescape(V);
unesc(V) -> V.


trans(Id, Prop, F, Context) ->
    trans_1(m_rsc:p(Id, Prop, Context), F).


trans_1(undefined, _F) -> undefined;
trans_1(V, F) when is_binary(V) -> F(V);
trans_1(#trans{ tr = [{_,V}] }, F) -> F(V);
trans_1(#trans{ tr = Tr }, F) ->
            lists:map(
                fun({Code, V}) ->
                    #{
                        <<"@language">> => Code,
                        <<"@value">> => F(V)
                    }
                end,
                Tr);
trans_1(V, _F) -> V.
