%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Open a dialog with some fields to upload a new media.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(action_admin_dialog_media_upload).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    Id = proplists:get_value(id, Args),
    Intent = case proplists:get_value(intent, Args) of
        undefined when Id =:= undefined -> <<"connect">>;
        undefined -> <<"update">>;
        Int -> Int
    end,
    SubjectId = proplists:get_value(subject_id, Args),
    Predicate = proplists:get_value(predicate, Args, depiction),
    Actions = proplists:get_all_values(action, Args),
    Stay = proplists:get_value(stay, Args, false),
    Center = proplists:get_value(center, Args, 1),
    Callback = proplists:get_value(callback, Args),
    Postback = {media_upload_dialog, Title, Intent, Id, SubjectId, Predicate, Stay, Center, Callback, Actions},
    {PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
    {PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={media_upload_dialog, Title, Intent, Id, SubjectId, Predicate, Stay, Center, Callback, Actions}}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {intent, Intent},
        {id, Id},
        {subject_id, SubjectId},
        {title, Title},
        {actions, Actions},
        {callback, Callback},
        {predicate, Predicate},
        {stay, Stay},
        {center, Center}
    ],
    DTitle = case Id of
        undefined -> ?__("Add a new media file", Context);
        _ -> ?__("Replace current medium", Context)
    end,
    z_render:dialog(DTitle, "_action_dialog_media_upload.tpl", Vars, Context);


event(#submit{message={media_upload, EventProps}}, Context) ->
    case z_context:get_q(<<"upload_file">>, Context) of
        #upload{ filename = OriginalFilename } = Upload ->
            Category = proplists:get_value(category, EventProps),
            Intent = proplists:get_value(intent, EventProps),
            Props = case Intent of
                <<"update">> ->
                    R = #{
                        <<"original_filename">> => OriginalFilename
                    },
                    case z_context:get_q(<<"medium_language">>, Context) of
                        undefined ->
                            R;
                        Language when is_binary(Language) ->
                            R#{
                                <<"medium_language">> => Language
                            }
                    end;
                _ ->
                    NewTitle = z_string:trim(z_convert:to_binary(z_context:get_q(<<"new_media_title">>, Context))),
                    IsDependent = z_convert:to_bool( z_context:get_q(<<"is_dependent">>, Context, false) ),
                    IsPublished = z_convert:to_bool( z_context:get_q(<<"is_published">>, Context, true) ),
                    Props0 = #{
                        <<"is_published">> => IsPublished,
                        <<"is_dependent">> => IsDependent,
                        <<"title">> =>  NewTitle,
                        <<"original_filename">> => OriginalFilename,
                        <<"medium_language">> => z_context:get_q(<<"medium_language">>, Context)
                    },
                    add_content_group(EventProps, Props0, Context)
            end,
            Opts = [
                {preferred_category, Category}
            ],
            handle_media_upload(Intent, EventProps, Context,
                                %% insert fun
                                fun(Ctx) -> m_media:insert_file(Upload, Props, Opts, Ctx) end,
                                %% replace fun
                                fun(Id, Ctx) -> m_media:replace_file(Upload, Id, Props, Opts, Ctx) end);
        _ ->
            z_render:growl(?__("Add a new media file", Context), Context)
    end;

event(#submit{message={media_url, EventProps}}, Context) ->
    Category = proplists:get_value(category, EventProps),
    Url = z_context:get_q(<<"url">>, Context),
    Intent = proplists:get_value(intent, EventProps),
    Props = case Intent of
        <<"update">> ->
            #{};
        _ ->
            IsDependent = z_convert:to_bool( z_context:get_q(<<"is_dependent">>, Context, false) ),
            IsPublished = z_convert:to_bool( z_context:get_q(<<"is_published">>, Context, true) ),
            Props0 = #{
                <<"is_published">> => IsPublished,
                <<"is_dependent">> => IsDependent,
                <<"title">> => z_context:get_q_validated(<<"new_media_title_url">>, Context)
            },
            add_content_group(EventProps, Props0, Context)
    end,
    Opts = [
        {preferred_category, Category}
    ],
    handle_media_upload(Intent, EventProps, Context,
                        %% insert fun
                        fun(Ctx) -> m_media:insert_url(Url, Props, Opts, Ctx) end,
                        %% replace fun
                        fun(Id, Ctx) -> m_media:replace_url(Url, Id, Props, Opts, Ctx) end).


add_content_group(EventProps, Props, Context) ->
    case content_group_id(
        proplists:get_value(content_group_id, EventProps),
        proplists:get_value(subject_id, EventProps),
        Context
    ) of
        undefined ->
            Props;
        ContentGroupId ->
            Props#{ <<"content_group_id">> => ContentGroupId }
    end.

content_group_id(undefined, SubjectId, Context) when is_integer(SubjectId) ->
    m_rsc:p_no_acl(SubjectId, content_group_id, Context);
content_group_id(ContentGroupId, _SubjectId, _Context) ->
    ContentGroupId.

%% Handling the media upload.
handle_media_upload(<<"update">>, EventProps, Context, _InsertFun, ReplaceFun) ->
    % Note: the 'stay' event argument is ignored here because we never redirect
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    case ReplaceFun(Id, Context) of
        {ok, _} ->
            z_render:wire([
                    {growl, [{text, ?__("Media item created.", Context)}]},
                    {dialog_close, []}
                    | Actions], Context);
        {error, R} ->
            z_render:growl_error(error_message(R, Context), Context)
    end;
handle_media_upload(_Intent, EventProps, Context, InsertFun, _ReplaceFun) ->
    case InsertFun(Context) of
        {ok, MediaId} ->
        % Note: the 'stay' event argument is "converted" to "redirect" here and
        % handled by 'action_admin_dialog_new_rsc:do_new_page_actions/3'
            Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
            NewEventProps =
                case proplists:is_defined(redirect, EventProps) of
                    true -> EventProps;
                    false -> [ {redirect, not Stay} | EventProps]
                end,
            action_admin_dialog_new_rsc:do_new_page_actions(MediaId, NewEventProps, Context);
        {error, R} ->
            z_render:growl_error(error_message(R, Context), Context)
    end.


%% @doc Return a sane upload error message
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
    ?LOG_WARNING(#{
        text => <<"Unknown file upload error">>,
        in => zotonic_mod_admin,
        result => error,
        reason => R
    }),
    ?__("Error uploading the file.", Context).
