%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014 Marc Worrell
%% @doc Import media from internet locations.

%% Copyright 2014 Marc Worrell
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

-module(z_media_import).
-author("Marc Worrell <marc@worrell.nl").

-export([
    insert/2,
    insert/3,
    update/3,
    url_import_props/2
    ]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_stdlib/include/z_url_metadata.hrl").


-define(EMPTY(A), ((A =:= undefined) orelse (A =:= "") orelse (A =:= <<>>))).

%% @doc Insert a selected #media_import_props{}
-spec insert( #media_import_props{}, z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert(MI, Context) ->
    insert(MI, #{}, Context).

%% @doc Insert a selected #media_import_props{}, add the forced properties to the resource.
-spec insert( #media_import_props{}, map(), z:context() ) -> {ok, m_rsc:resource_id()} | {error, term()}.
insert(#media_import_props{medium_props = MI} = MIPs, RscProps, Context) ->
    insert_1(maps:size(MI), MIPs, RscProps, Context).


insert_1(_, #media_import_props{ rsc_props = #{ <<"uri">> := Uri }, importer = rsc_import }, RscProps, Context) ->
    Options = [
        {props_forced, RscProps},
        is_import_deleted
    ],
    case m_rsc_import:import_uri(Uri, Options, Context) of
        {ok, {LocalId, _ObjectIds}} ->
            % TODO: schedule import of ObjectIds
            {ok, LocalId};
        {error, _} = Error ->
            Error
    end;
insert_1(0, #media_import_props{ category = Cat } = MI, RscProps, Context) ->
    RscProps1 = maps:merge(
                    default_rsc_props(MI, RscProps),
                    MI#media_import_props.rsc_props),
    case MI#media_import_props.preview_url of
        undefined ->
            m_rsc:insert(RscProps1, Context);
        PreviewUrl ->
            case m_media:insert_url(PreviewUrl, RscProps1, Context) of
                {ok, _} = OK ->
                    OK;
                {error, file_not_allowed} when Cat =:= website ->
                    m_rsc:insert(RscProps1, Context);
                {error, _} = Error ->
                    Error
            end
    end;
insert_1(_, #media_import_props{medium_url=MediumUrl, medium_props=MediumProps} = MI, RscProps, Context)
    when ?EMPTY(MediumUrl) ->
    RscProps1 = maps:merge(default_rsc_props(MI, RscProps), MI#media_import_props.rsc_props),
    Options = [
        {preview_url, MI#media_import_props.preview_url}
    ],
    m_media:insert_medium(MediumProps, RscProps1, Options, Context);
insert_1(_, #media_import_props{medium_url=MediumUrl} = MI, RscProps, Context) ->
    RscProps1 = maps:merge(default_rsc_props(MI, RscProps), MI#media_import_props.rsc_props),
    RscProps2 = RscProps1#{
        <<"original_filename">> => maps:get(<<"original_filename">>, MI#media_import_props.medium_props, undefined)
    },
    m_media:insert_url(MediumUrl, RscProps2, Context).

default_rsc_props(#media_import_props{category=Cat}, RscProps) ->
    maps:merge(
        #{
            <<"is_published">> => true,
            <<"category">> => Cat
        },
        RscProps).


%% @doc Update a resource with the selected #media_import_props()
update(RscId, #media_import_props{medium_props=MI} = MIPs, Context) ->
    update_1(maps:size(MI), RscId, MIPs, Context).


update_1(_, RscId, #media_import_props{ rsc_props = #{ <<"uri">> := Uri }, importer = rsc_import }, Context) ->
    Options = [
    ],
    m_rsc_import:update_medium_uri(RscId, Uri, Options, Context);
update_1(0, RscId, #media_import_props{preview_url=PreviewUrl, medium_url=MediumUrl}, _Context) 
    when ?EMPTY(PreviewUrl), ?EMPTY(MediumUrl) ->
    % Nothing to do
    {ok, RscId};
update_1(Sz, RscId, #media_import_props{medium_props=MP, medium_url=MediumUrl} = MI, Context) 
    when Sz > 0, ?EMPTY(MediumUrl) ->
    % Embedded, with optional preview_url
    RscProps = #{
        <<"original_filename">> => maps:get(<<"original_filename">>, MP, undefined)
    },
    Options = [
        {preview_url, MI#media_import_props.preview_url}
    ],
    m_media:replace_medium(MP, RscId, RscProps, Options, Context);
update_1(_, RscId, #media_import_props{medium_props=MP, medium_url=MediumUrl} = MI, Context) ->
    % Downloadable file, with optional preview_url
    RscProps = #{
        <<"original_filename">> => maps:get(<<"original_filename">>, MP, undefined)
    },
    Options = [
        {preview_url, MI#media_import_props.preview_url}
    ],
    m_media:replace_url(MediumUrl, RscId, RscProps, Options, Context).



%% @doc Find possible rsc/medium mappings for the url or fetched url metadata
-spec url_import_props(z_url_metadata:metadata() | string() | binary(), z:context()) ->
    {ok, list(#media_import_props{})} | {error, term()}.
url_import_props(Url, Context) when is_list(Url); is_binary(Url) ->
    case z_fetch:metadata(Url, [], Context) of
        {ok, MD} ->
            url_import_props(MD, Context);
        {error, _} = Error ->
            Error
    end;
url_import_props(#url_metadata{} = MD, Context) ->
    Url = z_url_metadata:p(url, MD),
    MI = #media_import{
        url = Url,
        host_rev = host_parts(Url),
        mime = z_url_metadata:p(mime, MD),
        metadata = MD
    },
    Ms = lists:flatten([
        import_as_resource(MD,Context),
        import_as_website(MD, Context),
        import_as_media(MD, Context)
        | z_notifier:map(MI, Context)
    ]),
    Ms1 = [ M || M <- Ms, M =/= undefined ],
    {ok, lists:sort(Ms1)}.

%% @doc Return the reversed list of parts of the hostname in an url.
host_parts(Url) ->
    case uri_string:parse(Url) of
        #{ host := Host } ->
            Ws = binary:split(z_convert:to_binary(Host), <<".">>, [ global ]),
            lists:reverse(Ws);
        _ ->
            []
    end.


%% @doc Import the remote as a resource export/import
import_as_resource(MD, Context) ->
    case importable_resource_uri(MD, Context) of
        {ok, {Uri, #{
            <<"rsc">> := Rsc,
            <<"depiction_url">> := DepUrl
        }}} ->
            DataDepUrl = case z_fetch:as_data_url(DepUrl, [], Context) of
                {ok, DU} -> DU;
                {error, _} -> undefined
            end,
            #media_import_props{
                prio = 1,
                category = maps:get(<<"category_id">>, Rsc, other),
                description = ?__("Resource import", Context),
                rsc_props = #{
                    <<"is_authoritative">> => false,
                    <<"uri">> => Uri,
                    <<"title">> => maps:get(<<"title">>, Rsc, undefined),
                    <<"summary">> => maps:get(<<"summary">>, Rsc, undefined)
                },
                medium_props = #{},
                preview_url = DataDepUrl,
                importer = rsc_import
            };
        false ->
            undefined
    end.

importable_resource_uri(MD, Context) ->
    case z_url_metadata:header(<<"x-resource-uri">>, MD) of
        <<>> ->
            false;
        Uri when is_binary(Uri) ->
            case m_rsc_import:fetch_preview(Uri, Context) of
                {ok, Rsc} ->
                    {ok, {Uri, Rsc}};
                {error, _} = Error ->
                    Error
            end;
        undefined ->
            false
    end.

%% @doc Import the url as a link to a website
import_as_website(MD, Context) ->
    #media_import_props{
        prio = 10,
        category = website,
        description = ?__("Website", Context),
        rsc_props = #{
            <<"title">> => z_url_metadata:p(title, MD),
            <<"summary">> => z_url_metadata:p(summary, MD),
            <<"website">> => z_url_metadata:p(url, MD)
        },
        medium_props = #{},
        preview_url = z_url_metadata:p(image, MD)
    }.

%% @doc The url refers to some (non-html) file, import it as-is.
import_as_media(MD, Context) ->
    Mime = z_url_metadata:p(mime, MD),
    case is_html(Mime) of
        false ->
            Category = m_media:mime_to_category(Mime),
            case m_rsc:exists(Category, Context) of
                true ->
                    #media_import_props{
                        prio = 3,
                        category = Category,
                        description = m_rsc:p_no_acl(Category, title, Context),
                        rsc_props = #{
                            <<"title">> => z_url_metadata:p(title, MD),
                            <<"summary">> => z_url_metadata:p(summary, MD),
                            <<"website">> => z_url_metadata:p(url, MD)
                        },
                        medium_props = #{
                            <<"mime">> => Mime,
                            <<"original_filename">> => z_url_metadata:p(filename, MD)
                        },
                        medium_url = z_url_metadata:p(url, MD)
                    };
                false ->
                    undefined
            end;
        true ->
            import_as_referred_image(MD, Context)
    end.

import_as_referred_image(MD, Context) ->
    Width = z_convert:to_integer(z_url_metadata:p(<<"og:image:width">>, MD)),
    Height = z_convert:to_integer(z_url_metadata:p(<<"og:image:height">>, MD)),
    case        is_integer(Width)
        andalso is_integer(Height)
        andalso Width > 32
        andalso Height > 32
    of
        true ->
            case z_url_metadata:p(<<"og:image">>, MD) of
                undefined -> undefined;
                <<>> -> undefined;
                ImageUrl ->
                    #media_import_props{
                        prio = 4,
                        category = image,
                        description = m_rsc:p_no_acl(image, title, Context),
                        rsc_props = #{
                            <<"title">> => z_url_metadata:p(title, MD),
                            <<"summary">> => z_url_metadata:p(summary, MD),
                            <<"website">> => z_url_metadata:p(url, MD)
                        },
                        medium_props = #{
                            <<"mime">> => <<"image/unknown">>,
                            <<"original_filename">> => z_url_metadata:p(filename, MD),
                            <<"width">> => Width,
                            <<"height">> => Height
                        },
                        medium_url = ImageUrl
                    }
            end;
        false ->
            undefined
    end.


is_html(<<"text/html">>) -> true;
is_html(<<"application/xhtml">>) -> true;
is_html(<<"application/xhtml+", _/binary>>) -> true;
is_html(_) -> false.
