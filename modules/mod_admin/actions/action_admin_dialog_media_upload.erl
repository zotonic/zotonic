%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-27
%% @doc Open a dialog with some fields to upload a new media.

%% Copyright 2009 Marc Worrell
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

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    Id = proplists:get_value(id, Args),
    SubjectId = proplists:get_value(subject_id, Args),
    Predicate = proplists:get_value(predicate, Args, depiction),
    Actions = proplists:get_all_values(action, Args),
    Stay = proplists:get_value(stay, Args, false),
    Callback = proplists:get_value(callback, Args),
    Postback = {media_upload_dialog, Title, Id, SubjectId, Predicate, Stay, Callback, Actions},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event(#postback{message={media_upload_dialog, Title, Id, SubjectId, Predicate, Stay, Callback, Actions}}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id},
        {subject_id, SubjectId},
        {title, Title},
        {actions, Actions},
        {callback, Callback},
        {predicate, Predicate},
        {stay, Stay}
    ],
    DTitle = case Id of undefined -> "Add a new media file"; _ -> "Replace current medium." end,
    z_render:dialog(DTitle, "_action_dialog_media_upload.tpl", Vars, Context);


event(#submit{message={media_upload, EventProps}}, Context) ->
    File = z_context:get_q_validated("upload_file", Context),
    ContextUpload = case File of
                        #upload{filename=OriginalFilename, tmpfile=TmpFile} ->
                            Props = case proplists:get_value(id, EventProps) of
                                        undefined ->
                                            Title = z_context:get_q("new_media_title", Context),
                                            NewTitle = case z_utils:is_empty(Title) of
                                                           true -> OriginalFilename;
                                                           false -> Title
                                                       end,
                                            [{title, NewTitle},
                                             {original_filename, OriginalFilename}];
                                        _ ->
                                            [{original_filename, OriginalFilename}]
                                    end,
                            handle_media_upload(EventProps, Context,
                                                %% insert fun
                                                fun(Ctx) -> m_media:insert_file(TmpFile, Props, Ctx) end,
                                                %% replace fun
                                                fun(Id, Ctx) -> m_media:replace_file(TmpFile, Id, Props, Ctx) end);
                        _ ->
                            z_render:growl("No file specified.", Context)
                    end,

    %% Close the dialog and optionally perform the post upload actions
    z_render:wire({dialog_close, []}, ContextUpload);


event(#submit{message={media_url, EventProps}}, Context) ->
    Url = z_context:get_q("url", Context),
    Props = case proplists:get_value(id, EventProps) of
                undefined ->
                    [{title, z_context:get_q_validated("new_media_title_url", Context)}];
                _ ->
                    []
            end,
    ContextUpload = handle_media_upload(EventProps, Context,
                                        %% insert fun
                                        fun(Ctx) -> m_media:insert_url(Url, Props, Ctx) end,
                                        %% replace fun
                                        fun(Id, Ctx) -> m_media:replace_url(Url, Id, Props, Ctx) end),

    %% Close the dialog and optionally perform the post upload actions
    z_render:wire({dialog_close, []}, ContextUpload).



%% Handling the media upload.
handle_media_upload(EventProps, Context, InsertFun, ReplaceFun) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = z_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    Callback = proplists:get_value(callback, EventProps),
    case Id of
        %% Create a new media page
        undefined ->
            SubjectId = proplists:get_value(subject_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),

            case InsertFun(Context) of
                {ok, MediaId} ->
                    {_, ContextCb} = mod_admin:do_link(z_convert:to_integer(SubjectId), Predicate, MediaId, Callback, Context),

                    ContextRedirect = 
                            case SubjectId of
                              undefined -> 
                                  case Stay of
                                      true -> ContextCb;
                                      false -> z_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, ContextCb)
                                  end;
                              _ -> 
                                  ContextCb
                            end,
                    Actions2 = [add_arg_to_action({id, MediaId}, A) || A <- Actions],
                    z_render:wire([
                            {growl, [{text, "Uploaded the file."}]},
                            {dialog_close, []} 
                            | Actions2], ContextRedirect);
                {error, _Error} ->
                    z_render:growl_error("Error uploading the file.", Context)
            end;
      
        %% Replace attached medium with the uploaded file (skip any edge requests)
        N when is_integer(N) -> 
            case ReplaceFun(Id, Context) of
                {ok, _} ->
                    Context1 = z_render:wire(Actions, Context),
                            z_render:growl("Uploaded the file.", Context1);
                {error, eacces} ->
                            z_render:growl_error("You don't have permission to change this page.", Context);
                {error, _} ->
                    z_render:growl_error("Error uploading the file.", Context)
            end
    end.


% Add an extra argument to a postback / submit action.
add_arg_to_action(Arg, {postback, [{postback, {Action, ArgList}} | Rest]}) ->
    {postback, [{postback, {Action, [Arg | ArgList]}} | Rest]};
add_arg_to_action(_A, B) ->
    B.


