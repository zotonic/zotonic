%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2014-2023 Marc Worrell <marc@worrell.nl>
%% @doc Enables embedding media from their URL.

%% Copyright 2014-2023 Marc Worrell <marc@worrell.nl>
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
    try_embed/2,

    try_url_http/2
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

event(#submit{message={media_url_embed, Args}, form=Form}, Context) ->
    {discover_elt, DiscoverElt} = proplists:lookup(discover_elt, Args),
    UrlData = z_string:trim(z_context:get_q(<<"url">>, Context)),
    Vars = case try_embed(UrlData, Context) of
                {ok, List} ->
                    ListAsProps = [ as_proplist(MI, Context) || MI <- List ],
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
    Intent = proplists:get_value(intent, ArgsEmbed),
    Result = case Intent of
        <<"update">> when is_integer(Id) ->
            RscProps = media_update_rsc_props(Context),
            m_rsc:update(Id, RscProps, Context),
            z_media_import:update(Id, MediaImport, Context);
        _ ->
            RscProps = media_insert_rsc_props(ArgsEmbed, Context),
            z_media_import:insert(MediaImport, RscProps, Context)
    end,
    handle_media_upload_args(Intent, Id, Result, ArgsEmbed, Context).

media_update_rsc_props(Context) ->
    case z_context:get_q(<<"medium_language">>, Context) of
        undefined ->
            #{};
        Language ->
            #{
                <<"medium_language">> => Language
            }
    end.

media_insert_rsc_props(ArgsEmbed, Context) ->
    SubjectId = m_rsc:rid(proplists:get_value(subject_id, ArgsEmbed), Context),
    CGId = m_rsc:rid(proplists:get_value(content_group_id, ArgsEmbed), Context),
    add_qprops(SubjectId, CGId, Context).


add_qprops(undefined, CGId, Context) ->
    Props = maps:remove(<<"is_dependent">>, qprops(Context)),
    Props1 = maybe_cgid(undefined, Props, CGId, Context),
    maybe_title(Props1, Context);
add_qprops(SubjectId, CGId, Context) when is_integer(SubjectId) ->
    Props = qprops(Context),
    Props1 = maybe_cgid(SubjectId, Props, CGId, Context),
    maybe_title(Props1, Context).

maybe_cgid(_SubjectId, #{ <<"content_group_id">> := CGId } = Props, _, _Context) when is_integer(CGId) ->
    Props;
maybe_cgid(_SubjectId, Props, CGId, _Context) when is_integer(CGId) ->
    Props#{
        <<"content_group_id">> => CGId
    };
maybe_cgid(SubjectId, Props, undefined, Context) ->
    SubjCGId = m_rsc:p_no_acl(SubjectId, content_group_id, Context),
    Props#{
        <<"content_group_id">> => SubjCGId
    }.


-define(is_id(X), X =:= <<"content_group_id">>; X =:= <<"category_id">>).

qprops(Context) ->
    Props = z_context:get_q_all_noz(Context),
    lists:foldl(
        fun
            ({IdArg, <<>>}, Acc) when ?is_id(IdArg)->
                Acc;
            ({IdArg, Id}, Acc) when ?is_id(IdArg) ->
                case m_rsc:rid(Id, Context) of
                    undefined -> Acc;
                    RId -> Acc#{ IdArg => RId }
                end;
            ({<<"is_", _/binary>> = K, V}, Acc) ->
                Acc#{ K => z_convert:to_bool(V) };
            ({K, V}, Acc) ->
                case binary:longest_common_suffix([<<"_id">>, K]) of
                    3 -> Acc#{ K => m_rsc:rid(V, Context) };
                    _ -> Acc#{ K => V }
                end
        end,
        #{},
        Props).

maybe_title(Props, Context) ->
    case z_context:get_q(<<"new_media_title">>, Context) of
        Title when is_binary(Title), Title =/= <<>> ->
            Props#{
                <<"title">> => Title
            };
        _ ->
            Props
    end.

%% Handling the media upload (Slightly adapted from action_admin_dialog_media_upload)
handle_media_upload_args(<<"update">>, Id, {ok, _}, EventProps, Context) when is_integer(Id) ->
    %% Replace attached medium with the uploaded file (skip any edge requests)
    Actions = proplists:get_value(actions, EventProps, []),
    z_render:wire([
            {growl, [{text, ?__("Media updated.", Context)}]},
            {dialog_close, []}
            | Actions], Context);
handle_media_upload_args(_Intent, _Id, {ok, NewId}, EventProps, Context) ->
    %% Create a new media page - connect and perform post actions
    action_admin_dialog_new_rsc:do_new_page_actions(NewId, EventProps, Context);
handle_media_upload_args(_Intent, _Id, {error, R}, _EventProps, Context) ->
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
error_message({Status, Url, Hs, _Size, _Body}, Context) when is_integer(Status), is_list(Hs) ->
    ?LOG_ERROR(#{
        text => <<"HTTP error fetching URL">>,
        in => zotonic_mod_admin,
        result => error,
        reason => http_status,
        http_status => Status,
        url => Url,
        headers => Hs
    }),
    z_fetch:error_msg(Status, Context);
error_message(R, Context) ->
    ?LOG_WARNING(#{
        text => <<"Unknown media discover error">>,
        in => zotonic_mod_admin,
        result => error,
        reason => R
    }),
    ?__("Error checking the website.", Context).


as_proplist(#media_import_props{} = MI, Context) ->
    Cat = case m_rsc:is_a(MI#media_import_props.category, category, Context) of
        true ->
            m_rsc:rid(MI#media_import_props.category, Context);
        false ->
            m_media:mime_to_category(maps:get(<<"mime">>, MI#media_import_props.medium_props, undefined))
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
            prio = 1,
            category = video,
            description = ?__("Embedded Content", Context),
            module = mod_admin,
            rsc_props = #{
                <<"website">> => Url
            },
            medium_props = #{
                <<"mime">> => <<"text/html-video-embed">>,
                <<"video_embed_code">> => EmbedCode,
                <<"media_import">> => Html
            }
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
    % Make a user for this email address?
    {error, todo};
try_url(_, _Context) ->
    {error, unknown}.

-spec try_url_http(Url, Context) -> {ok, MediaImports} | {error, Reason} when
    Url :: binary(),
    Context :: z:context(),
    MediaImports :: [ MediaImport ],
    MediaImport :: #media_import_props{},
    Reason :: term().
try_url_http(Url, Context) ->
    case z_media_import:url_import_props(z_sanitize:uri(Url), Context) of
        {ok, List} ->
            {ok, List};
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

