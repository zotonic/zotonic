%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2019 Marc Worrell <marc@worrell.nl>
%% @doc Enables embedding media from their URL.

%% Copyright 2014-2019 Marc Worrell <marc@worrell.nl>
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

-module(z_admin_media_discover).

-export([
    event/2,
    try_embed/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#submit{message={media_url_embed, Args}, form=Form}, Context) ->
    {discover_elt, DiscoverElt} = proplists:lookup(discover_elt, Args),
    UrlData = z_convert:to_binary(z_string:trim(z_context:get_q(url, Context))),
    Vars = case try_embed(UrlData, Context) of
                {ok, List} ->
                    ListAsProps = [ as_proplist(MI) || MI <- List ],
                    [
                        {media_imports, ListAsProps},
                        {id, proplists:get_value(id, Args)},
                        {form_id, Form},
                        {discover_id, DiscoverElt},
                        {args, Args}
                    ];
                {error, Error} ->
                    [
                        {media_imports, []},
                        {error, error_message(Error, Context)}
                    ]
           end,
    Context1 = z_render:update(DiscoverElt,
                    #render{
                        template="_admin_media_import_list.tpl",
                        vars=Vars
                    },
                    Context),
    z_render:wire({fade_in, [{target, DiscoverElt}]}, Context1);
event(#submit{message={media_url_import, Args}}, Context) ->
    {args, ArgsEmbed} = proplists:lookup(args, Args),
    {media, MediaImport} = proplists:lookup(media, Args),
    Id = m_rsc:rid( proplists:get_value(id, ArgsEmbed), Context),
    Result = case Id of
    undefined->
        RscProps = media_insert_rsc_props(ArgsEmbed, Context),
        z_media_import:insert(MediaImport, RscProps, Context);
    Id when is_integer(Id) ->
        z_media_import:update(Id, MediaImport, Context)
    end,
    handle_media_upload_args(Id, Result, ArgsEmbed, Context).

media_insert_rsc_props(ArgsEmbed, Context) ->
    SubjectId = m_rsc:rid(proplists:get_value(subject_id, ArgsEmbed), Context),
    CGId = m_rsc:rid(proplists:get_value(content_group_id, ArgsEmbed), Context),
    add_qprops(SubjectId, CGId, Context).

add_qprops(undefined, CGId, Context) ->
    [
        {content_group_id, CGId},
        {is_published, z_convert:to_bool( z_context:get_q(<<"is_published">>, Context, true) )}
    ]
    ++ maybe_title(Context);
add_qprops(SubjectId, undefined, Context) when is_integer(SubjectId) ->
    SubjCGId = m_rsc:p_no_acl(SubjectId, content_group_id, Context),
    CGId = get_q(<<"content_group_id">>, SubjCGId, Context),
    [
        {content_group_id, z_convert:to_integer(CGId)},
        {is_published, z_convert:to_bool( z_context:get_q(<<"is_published">>, Context, true) )},
        {is_dependent, z_convert:to_bool( z_context:get_q(<<"is_dependent">>, Context, false) )}
    ]
    ++ maybe_title(Context);
add_qprops(SubjectId, CGId, Context) when is_integer(SubjectId), is_integer(CGId) ->
    CGId1 = get_q(<<"content_group_id">>, CGId, Context),
    [
        {content_group_id, z_convert:to_integer(CGId1)},
        {is_published, z_convert:to_bool( z_context:get_q(<<"is_published">>, Context, true) )},
        {is_dependent, z_convert:to_bool( z_context:get_q(<<"is_dependent">>, Context, false) )}
    ]
    ++ maybe_title(Context).

maybe_title(Context) ->
    case z_context:get_q(<<"new_media_title">>, Context) of
        undefined -> [];
        <<>> -> [];
        Title -> [ {title, Title} ]
    end.

get_q(P, Default, Context) ->
    case z_context:get_q(P, Context) of
        undefined -> Default;
        <<>> -> Default;
        V -> V
    end.

%% Handling the media upload (Slightly adapted from action_admin_dialog_media_upload)
handle_media_upload_args(undefined, {ok, NewId}, EventProps, Context) ->
    %% Create a new media page
    action_admin_dialog_new_rsc:do_new_page_actions(NewId, EventProps, Context);
handle_media_upload_args(Id, {ok, _}, EventProps, Context) when is_integer(Id) ->
    %% Replace attached medium with the uploaded file (skip any edge requests)
    Actions = proplists:get_value(actions, EventProps, []),
    z_render:wire([
            {growl, [{text, ?__("Media item created.", Context)}]},
            {dialog_close, []}
            | Actions], Context);
handle_media_upload_args(Id, {error, R}, _EventProps, Context) when is_integer(Id) ->
    z_render:growl_error(error_message(R, Context), Context).

%% @doc Return a sane upload error message
error_message({failed_connect, _}, Context) ->
    ?__("Could not connect to the website.", Context);
error_message(timeout, Context) ->
    ?__("Timeout while trying to fetch data from the website.", Context);
error_message(eacces, Context) ->
    ?__("You don't have permission to change this media item.", Context);
error_message(file_not_allowed, Context) ->
    ?__("You don't have the proper permissions to upload this type of file.", Context);
error_message(download_failed, Context) ->
    ?__("Failed to download the file.", Context);
error_message(infected, Context) ->
    ?__("This file is infected with a virus.", Context);
error_message(av_external_links, Context) ->
    ?__("This file contains links to other files or locations.", Context);
error_message(sizelimit, Context) ->
    ?__("This file is too large.", Context);
error_message(R, Context) ->
    lager:warning("Unknown media discover error: ~p", [R]),
    ?__("Error checking the website.", Context).


as_proplist(#media_import_props{} = MI) ->
    Cat = case MI#media_import_props.category of
        audio -> audio;
        video -> video;
        image -> image;
        website -> website;
        document -> document;
        _ -> m_media:mime_to_category(proplists:get_value(mime, MI#media_import_props.medium_props))
    end,
    [
        {description, MI#media_import_props.description},
        {props, MI#media_import_props.rsc_props},
        {medium, MI#media_import_props.medium_props},
        {medium_url, MI#media_import_props.medium_url},
        {preview_url, MI#media_import_props.preview_url},
        {media_import, MI},
        {category, Cat}
    ].

try_embed(<<$<, _/binary>> = Html, Context) ->
    % Should still fetch the embed url from here, we can fetch metadata from youtube videos
    EmbedCode = z_sanitize:html(Html, Context),
    Url = case url_from_embed(EmbedCode) of
              {ok, SrcUrl} -> SrcUrl;
              {error, _} -> undefined
          end,
    {ok, [
        #media_import_props{
            prio=1,
            category=video,
            description=?__("Embedded Content", Context),
            rsc_props=[
                {website, Url}
            ],
            medium_props=[
                {mime, <<"text/html-video-embed">>},
                {video_embed_code, EmbedCode},
                {media_import, Html}
            ]
        }
    ]};
try_embed(Url, Context) ->
    try_url(url(Url), Context).

try_url({ok, <<"http:", _/binary>> = Url}, Context) ->
    try_url_http(Url, Context);
try_url({ok, <<"https:", _/binary>> = Url}, Context) ->
    try_url_http(Url, Context);
try_url({ok, <<"data:", _/binary>>}, _Context) ->
    % Use the z_url routines to decode, then save to tempfile and handle as file upload
    {error, todo};
try_url({ok, <<"ftp:", _/binary>>}, _Context) ->
    % Use anonymous ftp
    {error, todo};
try_url({ok, <<"email:", _/binary>>}, _Context) ->
    % Make an user for this email address?
    {error, todo};
try_url(_, _Context) ->
    {error, unknown}.

try_url_http(Url, Context) ->
    case z_url_metadata:fetch(Url) of
        {ok, MD} ->
            case z_media_import:url_import_props(MD, Context) of
                {ok, List} ->
                    {ok, List};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

url(<<"www.", _/binary>> = Url) -> {ok, <<"http://", Url/binary>>};
url(<<"//", _/binary>> = Url) -> {ok, <<"http:", Url/binary>>};
url(<<"http:", _/binary>> = Url) -> {ok, Url};
url(<<"https:", _/binary>> = Url) -> {ok, Url};
url(<<"data:", _/binary>> = Url) -> {ok, Url};
url(<<"ftp:", _/binary>> = Url) -> {ok, Url};
url(Url) ->
    case re:run(Url, "^[a-zA-Z0-9-]+\\.") of
        {match, _} -> {ok, <<"http://", Url/binary>>};
        nomatch -> {error, protocol}
    end.

url_from_embed(Embed) ->
    case re:run(Embed, "src=[\"']([^\"']+)[\"']", [{capture, all_but_first, binary}]) of
        {match, [Url|_]} ->
            url(Url);
        nomatch ->
            {error, notfound}
    end.

