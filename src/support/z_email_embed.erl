%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @date 2010-08-05
%% @doc Email image embedding

%% Copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
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

%% interface functions
-export([embed_images/2]).

-include_lib("zotonic.hrl").

%% Given a HTML message, extract images (starting with /lib/ or /image/) and
embed_images(Html, Context) ->
    {P1, Html1} = embed_lib_images(Html, Context),
    {P2, Html2} = embed_generated_images(Html1, Context),
    {P1 ++ P2, Html2}.


embed_lib_images(Html, Context) ->
    Re = "src=[\"']/lib/(.*?)[\"']",
    case re:run(Html, Re, [{capture, all_but_first, list}, global, caseless]) of
        nomatch -> 
            {[], Html};
        {match, Matches} ->
            UniqueMatches = sets:to_list(sets:from_list(Matches)),
            {P, H, _C} = lists:foldl(fun embed_lib_image_match/2, {[], Html, Context}, UniqueMatches),
            {P, H}
    end.

embed_lib_image_match([Match], {Parts, Html, Context}) ->
    File = filename:join([z_path:site_dir(Context), "lib", Match]),
    case filelib:is_file(File) of
        true ->
            ContentType = z_media_identify:guess_mime(File),
            create_attachment,
            {Part, Cid} = {[], 1}, %esmtp_mime:create_attachment(File, ContentType, inline),
            Html1 = re:replace(Html, "/lib/" ++ Match, "cid:" ++ Cid, [global, {return, list}]),
            {[Part|Parts], Html1, Context};
        _ ->
            ?DEBUG("Not found attachment"),
            {Parts, Html, Context}
    end.


embed_generated_images(Html, Context) ->
    Re = "src=[\"']/image/(.*?)[\"']",
    case re:run(Html, Re, [{capture, all_but_first, list}, global, caseless]) of
        nomatch -> 
            {[], Html};
        {match, Matches} ->
            UniqueMatches = sets:to_list(sets:from_list(Matches)),
            {P, H, _C} = lists:foldl(fun embed_generated_image_match/2, {[], Html, Context}, UniqueMatches),
            {P, H}
    end.


embed_generated_image_match([Match], {Parts, Html, Context}) ->
    File = z_utils:url_decode(filename:join([z_path:site_dir(Context), "files", "preview", Match])),
    case filelib:is_file(File) of
        true -> nop;
        false ->
            %% Try to get over HTTP
            Url = lists:flatten(z_context:abs_url("/image/" ++ Match, Context)),
            ?DEBUG("Downloading image"), ?DEBUG(Url),
            {ok, {{_, 200, _}, _, _}} = http:request(Url)
    end,
    ContentType = z_media_identify:guess_mime(File),
    create_attachment,
    {Part, Cid} = {[], 1}, %esmtp_mime:create_attachment(File, ContentType, inline),
    Html1 = re:replace(Html, "/image/" ++ Match, "cid:" ++ Cid, [global, {return, list}]),
    {[Part|Parts], Html1, Context}.
