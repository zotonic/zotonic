%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Admin webmachine_resource.

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

-module(resource_admin_edit).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(true, visible, "id", ReqData, Context).


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Id = z_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = z_context:set(id, IdN, Context2),
        ?WM_REPLY(m_rsc:exists(IdN, Context3), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Vars = [
        {id, z_context:get(id, Context)}
    ],
    Html = z_template:render("admin_edit.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, rscform, _FormId, _TargetId}, Context) ->
    Post = z_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = ?TR(proplists:get_value("title", Props), Context),
    Id = z_convert:to_integer(proplists:get_value("id", Props)),
    Props1 = proplists:delete("id", Props),
    CatBefore = m_rsc:p(Id, category_id, Context),
    case m_rsc:update(Id, Props1, Context) of
        {ok, _} -> 
            case proplists:is_defined("save_view", Post) of
                true ->
                    PageUrl = m_rsc:p(Id, page_url, Context),
                    z_render:wire({redirect, [{location, PageUrl}]}, Context);
                false ->
                    case m_rsc:p(Id, category_id, Context) of
                        CatBefore ->
                            Context1 = z_render:set_value("field-name", m_rsc:p(Id, name, Context), Context),
                            Context2 = z_render:set_value("field-uri",  m_rsc:p(Id, uri, Context1), Context1),
                            Context3 = z_render:set_value("field-page-path",  m_rsc:p(Id, page_path, Context1), Context2),
                            Context4 = z_render:set_value("slug",  m_rsc:p(Id, slug, Context3), Context3),
                            Context5 = case z_convert:to_bool(m_rsc:p(Id, is_protected, Context4)) of
                                true ->  z_render:wire("delete-button", {disable, []}, Context4);
                                false -> z_render:wire("delete-button", {enable, []}, Context4)
                            end,
                            Context6 = z_render:growl(["Saved ",z_html:strip(Title),"."], Context5),
                            case proplists:is_defined("save_duplicate", Post) of
                                true ->
                                    z_render:wire({dialog_duplicate_rsc, [{id, Id}]}, Context6);
                                false ->
                                    Context6
                            end;
                        _CatOther ->
                            z_render:wire({reload, []}, Context)
                    end
            end;
        {error, duplicate_uri} ->
            z_render:growl_error("Error, duplicate uri. Please change the uri.", Context);
        {error, duplicate_name} ->
            z_render:growl_error("Error, duplicate name. Please change the name.", Context);
        {error, eacces} ->
            z_render:growl_error("You don't have permission to edit this page.", Context);
        {error, _Reason} ->
            z_render:growl_error("Something went wrong. Sorry.", Context)
    end;

event({postback, {reload_media, Opts}, _TriggerId, _TargetId}, Context) ->
    RscId = proplists:get_value(rsc_id, Opts),
    DivId = proplists:get_value(div_id, Opts),
    {Html, Context1} = z_template:render_to_iolist("_edit_media.tpl", [{id,RscId},{div_id,DivId}], Context),
    z_render:update(DivId, Html, Context1);

event({sort, Sorted, {dragdrop, {object_sorter, Props}, _, _}}, Context) ->
    RscId     = proplists:get_value(id, Props),
    Predicate = proplists:get_value(predicate, Props),
    EdgeIds   = [ EdgeId || {dragdrop, EdgeId, _, _ElementId} <- Sorted ],
    m_edge:update_sequence_edge_ids(RscId, Predicate, EdgeIds, Context),
    Context.
    

%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
        "triggervalue",
        "postback",
        "z_trigger_id",
        "z_pageid",
        "trigger_value",
        "save_view",
        "save_duplicate",
        "save_stay"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].


