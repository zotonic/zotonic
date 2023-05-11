%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2023 Arjan Scherpenisse
%% @doc Email image embedding
%% @end

%% Copyright 2010-2023 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(z_email_embed).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    embed_images/2,
    embed_images_html/2
]).

-include_lib("zotonic.hrl").
-include_lib("zotonic_file.hrl").

% Do not embed images > 1MB
-define(MAX_EMBED_SIZE, 1024*1024).

%% @doc Embed images mentioned in the HTML parts.
embed_images(Parts, Context) ->
    IsEmbed = not m_config:get_boolean(site, email_images_noembed, false, Context),
    [ embed_images_part(P, IsEmbed, Context) || P <- Parts ].


embed_images_part({<<"text">>, <<"html">>, Hs, Params, Html} = HtmlPart, IsEmbed, Context) when is_map(Params) ->
    case embed_images_html(Html, IsEmbed, Context) of
        {[], _Html} ->
            HtmlPart;
        {ImageParts, Html1} ->
            Html1,
            {   <<"multipart">>, <<"related">>,
                [],
                #{},
                [ {<<"text">>, <<"html">>, Hs, Params, z_convert:to_binary(Html1)} | ImageParts ]
            }
    end;
embed_images_part({<<"multipart">>, SubType, Hs, Params, Parts}, IsEmbed, Context) when is_map(Params) ->
    Parts1 = [ embed_images_part(P, IsEmbed, Context) || P <- Parts ],
    {<<"multipart">>, SubType, Hs, Params, Parts1};
embed_images_part(Part, _IsEmbed, _Context) ->
    Part.

embed_images_html(Html, Context) ->
    embed_images_html(Html, true, Context).

%% Given a HTML message, extract images (starting with /lib/ or /image/) and
% embed them as separate parts.
embed_images_html(Html, IsEmbed, Context) ->
    {Parts, Html1} = lists:foldl(
        fun(Img, Acc) ->
            embed_image(Img, IsEmbed, Acc, Context)
        end,
        {[], Html},
        find_images(Html, Context)),
    {Parts, Html1}.

find_images(Html, Context) ->
    BaseUrl = z_context:abs_url(<<>>, Context),
    Re = <<"src=([\"']((", BaseUrl/binary, "|/)?((lib|image)/[^\"']*\\.(jpg|gif|png)))[\"'])">>,
    case re:run(Html, Re, [{capture, all_but_first, binary}, global]) of
        nomatch ->
            [];
        {match, Matches} ->
            lists:usort(Matches)
    end.

% [
% <<"'https://example.com/lib/foo/bar.png'">>,
% <<"https://example.com/lib/foo/bar.png">>,
% <<"https://example.com/">>,
% <<"lib/foo/bar.png">>,
% <<"lib">>,
% <<"png">>
% ]
embed_image([QuotedSrc, _Src, _BaseUrl, Path0, _LibImg, _Ext], IsEmbed, {Parts, Html}, Context) ->
    ContextAnon = z_acl:anondo(Context),
    Path = <<"/", Path0/binary>>,
    case IsEmbed orelse not z_media_data:is_file_data_visible(Path, ContextAnon) of
        true ->
            case z_media_data:file_data(Path, Context) of
                {ok, {Mime, Data}} when is_binary(Data), size(Data) < ?MAX_EMBED_SIZE ->
                    {Part, Cid} = create_attachment(Data, Mime, filename:basename(Path)),
                    NewPath = iolist_to_binary([$", "cid:", Cid, $"]),
                    Html1 = binary:replace(Html, QuotedSrc, NewPath, [global]),
                    {[Part|Parts], Html1};
                {ok, _} ->
                    {Parts, Html};
                {error, _} ->
                    {Parts, Html}
            end;
        false ->
            {Parts, Html}
    end.

create_attachment(Data, ContentType, Name) ->
    ContentId = z_ids:id(30),
    Headers = [
        {<<"Content-ID">>, iolist_to_binary(["<", ContentId, ">"])},
        {<<"X-Attachment-Id">>, ContentId}
    ],
    {create_attachment_part(Data, ContentType, Name, Headers), ContentId}.

create_attachment_part(Data, ContentType, Name, Headers) ->
    [Type, Subtype] = binary:split(ContentType, <<"/">>),
    Params = #{
        transfer_encoding => <<"base64">>,
        disposition => <<"inline">>,
        disposition_params => [
                {<<"filename">>, z_convert:to_binary(unreserved_filename(Name))}
            ]
    },
    { Type, Subtype,
      Headers,
      Params,
      Data
    }.

%% @doc Remove all percent encoding from the path and remove any characters that might
%%      give a problem.
unreserved_filename(Name) ->
    Unquoted = z_url:url_decode(Name),
    lists:map(
        fun
            ($~) -> $-;
            (C) ->
                case z_url:url_unreserved_char(C) of
                    true -> C;
                    false -> $-
                end
        end,
        z_convert:to_list(Unquoted)).

