%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% Date: 2010-01-19
%% @doc Creates an editable overview of all categories.

%% Copyright 2010 Marc Worrell
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

-module(controller_admin_comments).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4,
    event/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_comment)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Html = z_template:render("admin_comments.tpl", [{page_admin_comments, true}], Context),
    z_context:output(Html, Context).

event(#postback{message={comment_delete, Args}}, Context) ->
    CommentId = proplists:get_value(id, Args),
    case m_comment:delete(CommentId, Context) of
        ok ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            Context1 = z_render:wire(OnSuccess, Context),
            z_render:growl(?__("The comment has been deleted.", Context1), Context1);
        {error, _Reason} ->
            %% Assume permission problem.
            z_render:growl_error(?__("You are not allowed to delete the comment.", Context), Context)
    end;

event(#postback{message={comment_toggle, Args}}, Context) ->
    CommentId = proplists:get_value(id, Args),
    BtnPublish = proplists:get_value(btnpublish, Args),
    BtnUnpublish = proplists:get_value(btnunpublish, Args),
    case m_comment:toggle(CommentId, Context) of
        {ok, IsVisible} ->
            OnSuccess = proplists:get_all_values(on_success, Args),
            Context1 = z_render:wire(OnSuccess, Context),
            case proplists:get_value(element, Args) of
                undefined ->
                    Context1;
                ElementId ->
                    case IsVisible of
                        true ->
                            z_render:wire([
					   {remove_class, [{target, ElementId},{class,"unpublished"}]},
					   {add_class, [{target, BtnPublish},{class,"disabled"}]},
					   {remove_class, [{target, BtnUnpublish},{class,"disabled"}]}
					  ], Context1);
                        false ->
                            z_render:wire([
					   {add_class, [{target, ElementId},{class,"unpublished"}]},
					   {add_class, [{target, BtnUnpublish},{class,"disabled"}]},
					   {remove_class, [{target, BtnPublish},{class,"disabled"}]}
					  ], Context1)
                    end
            end;
        {error, _Reason} ->
            %% Assume permission problem.
            z_render:growl_error(?__("You are not allowed to toggle the comment.", Context), Context)
    end;

event(Else, _Context) ->
    error_logger:info_msg("Other Event: ~p", [Else]).




