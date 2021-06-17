%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2014 Arjan Scherpenisse
%% @doc Email image embedding

%% Copyright 2010-2014 Arjan Scherpenisse <arjan@scherpenisse.net>
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

%% @doc Embed images mentioned in the HTML parts.
embed_images(Parts, Context) ->
    [ embed_images_part(P, Context) || P <- Parts ].


embed_images_part({<<"text">>, <<"html">>, Hs, Ps, Html} = HtmlPart, Context) ->
    case embed_images_html(Html, Context) of
        {[], _Html} ->
            HtmlPart;
        {ImageParts, Html1} ->
            Html1,
            {   <<"multipart">>, <<"related">>,
                [],
                [],
                [ {<<"text">>, <<"html">>, Hs, Ps, z_convert:to_binary(Html1)} | ImageParts ]
            }
    end;
embed_images_part({<<"multipart">>, SubType, Hs, Ps, Parts}, Context) ->
    Parts1 = [ embed_images_part(P, Context) || P <- Parts ],
    {<<"multipart">>, SubType, Hs, Ps, Parts1};
embed_images_part(Part, _Context) ->
    Part.


%% Given a HTML message, extract images (starting with /lib/ or /image/) and
% embed them as separate parts.
embed_images_html(Html, Context) ->
    {Parts, Html1, _C} = lists:foldl(fun embed_image/2, {[], Html, Context}, find_images(Html)),
    {Parts, Html1}.

find_images(Html) ->
    Re = <<"src=([\"'](/(lib|image)/[^\"']*\\.(jpg|gif|png))[\"'])">>,
    case re:run(Html, Re, [{capture, all_but_first, binary}, global]) of
        nomatch ->
            [];
        {match, Matches} ->
            sets:to_list(sets:from_list(Matches))
    end.

% [<<"'/lib/foo/bar.png'">>,<<"/lib/foo/bar.png">>,<<"lib">>, <<"png">>]
embed_image([QuotedPath, Path, _LibImg, _Ext], {Parts, Html, Context}) ->
    case z_media_data:file_data(Path, Context) of
        {ok, {Mime, Data}} ->
            {Part, Cid} = create_attachment(Data, Mime, filename:basename(Path)),
            NewPath = iolist_to_binary([$", "cid:", Cid, $"]),
            Html1 = re:replace(Html, QuotedPath, NewPath, [global, {return, binary}]),
            {[Part|Parts], Html1, Context};
        {error, _} ->
            {Parts, Html, Context}
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
    { Type, Subtype,
      Headers,
      [
        {<<"transfer-encoding">>, <<"base64">>},
        {<<"disposition">>, <<"inline">>},
        {<<"disposition-params">>, [{<<"filename">>, z_convert:to_binary(unreserved_filename(Name))}]}
      ],
      Data
    }.

%% @doc Remove all percent encoding from the path and remove any characters that might
%%      give a problem.
unreserved_filename(Name) ->
    Unquoted = z_url:url_decode( z_convert:to_list(Name) ),
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

