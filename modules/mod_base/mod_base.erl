%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2012 Marc Worrell
%% Date: 2009-06-08
%% @doc The base module, implementing basic Zotonic scomps, actions, models and validators.

%% Copyright 2009-2012 Marc Worrell
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

-module(mod_base).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Zotonic Base").
-mod_description("Base supplies all basic scomps, actions and validators.").
-mod_prio(9999).
-mod_depends([]).
-mod_provides([base]).

-include_lib("zotonic.hrl").

%% interface functions
-export([
    observe_edge_insert/2,
    observe_edge_delete/2,
    observe_media_stillimage/2,
    observe_scomp_script_render/2,
    observe_dispatch/2,
    observe_hierarchy_updated/2
]).


%% @doc If an edge is inserted, then force a repivot of the subject
observe_edge_insert(#edge_insert{ subject_id=SubjectId }, Context) ->
    z_pivot_rsc:insert_queue(SubjectId, Context),
    ok.

%% @doc If an edge is deleted, then force a repivot of the subject
observe_edge_delete(#edge_delete{ subject_id=SubjectId }, Context) ->
    z_pivot_rsc:insert_queue(SubjectId, Context),
    ok.


%% @doc Return the filename of a still image to be used for image tags.
%% @spec observe_media_stillimage(Notification, _Context) -> undefined | {ok, Filename} | {ok, {filepath, Filename, Path}}
observe_media_stillimage(#media_stillimage{props=Props}, Context) ->
    case proplists:get_value(preview_filename, Props) of
        None when None =:= <<>>; None =:= undefined ->
            case proplists:get_value(mime, Props) of
                undefined -> undefined;
                [] -> undefined;
                Mime ->
                    case z_media_preview:can_generate_preview(Mime) of
                        true ->
                            %% Let media preview handle this.
                            undefined;
                        false ->
                            %% Serve an icon representing the mime type.
                            [A,B] = string:tokens(z_convert:to_list(Mime), "/"),
                            Files = [
                                "images/mimeicons/"++A++[$-|B]++".png",
                                "images/mimeicons/"++A++".png",
                                "images/mimeicons/application-octet-stream.png"
                            ],

                            lists:foldl(
                                fun(F, undefined) ->
                                        case z_module_indexer:find(lib, F, Context) of
                                            {ok, #module_index{filepath=_File}} -> {ok, "lib/"++F};
                                            {error, enoent} -> undefined
                                        end;
                                   (_F, Result) ->
                                       Result
                                end,
                                undefined,
                                Files)
                    end
            end;
        PreviewFilename ->
            {ok, z_convert:to_list(PreviewFilename)}
    end.


%% @doc Part of the {% script %} rendering in templates
observe_scomp_script_render(#scomp_script_render{is_nostartup=false}, Context) ->
    DefaultFormPostback = z_render:make_postback_info("", "submit", undefined, undefined, undefined, Context),
    [<<"z_init_postback_forms();\nz_default_form_postback = \"">>, DefaultFormPostback, $", $; ];
observe_scomp_script_render(#scomp_script_render{is_nostartup=true}, _Context) ->
    [].

%% @doc Check if there is a controller or template matching the path.
observe_dispatch(#dispatch{path=Path}, Context) ->
    case m_rsc:page_path_to_id(z_utils:url_path_encode(Path), Context) of
        {ok, Id} ->
            {ok, Id};
        {redirect, Id} ->
            {ok, #dispatch_match{
                    mod=controller_redirect,
                    mod_opts=[{id, Id}, {is_permanent, true}]}};
        {error, _} ->
            SlashPath = case Path of
                "" -> "/";
                "/" ++ _ -> Path;
                _ -> [ $/ | Path ]
            end,
            Last = last(SlashPath),
            Template= case Last of
                         $/ -> "static"++SlashPath++"index.tpl";
                         _ -> "static"++SlashPath++".tpl"
                      end,
            case z_module_indexer:find(template, Template, Context) of
                {ok, _} ->
                    {ok, #dispatch_match{
                        mod=controller_template,
                        mod_opts=[{template, Template}, {ssl, any}],
                        bindings=[{path, SlashPath}, {is_static, true}]
                    }};
                {error, _} ->
                    % Check again, assuming the path is a directory (without trailing $/)
                    case Last of
                        $/ ->
                            undefined;
                        $. ->
                            undefined;
                        _ ->
                            Template1 = "static"++SlashPath++"/index.tpl",
                            case z_module_indexer:find(template, Template1, Context) of
                                {ok, _} ->
                                    {ok, #dispatch_match{
                                        mod=controller_template,
                                        mod_opts=[{template, Template1}, {ssl, any}],
                                        bindings=[{path, SlashPath}, {is_static, true}]
                                    }};
                                {error, _} ->
                                    undefined
                            end
                    end
            end
    end.


observe_hierarchy_updated(#hierarchy_updated{ root_id = <<"$category">> }, Context) ->
    % Something changed to the category hierarchy - let m_categoruy resync the pivot
    m_category:renumber(Context);
observe_hierarchy_updated(#hierarchy_updated{ root_id = _ }, _Context) ->
    ok.

%%====================================================================
%% support functions
%%====================================================================

last([]) -> $/;
last(Path) -> lists:last(Path).
