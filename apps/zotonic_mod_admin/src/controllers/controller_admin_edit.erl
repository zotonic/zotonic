%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell, Arjan Scherpenisse
%% @doc Admin webmachine_controller.

%% Copyright 2009-2010 Marc Worrell, Arjan Scherpenisse
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

-module(controller_admin_edit).
-author("Marc Worrell <marc@worrell.nl>").

-export([resource_exists/1,
         previously_existed/1,
         moved_temporarily/1,
         is_authorized/1,
         event/2,
         filter_props/1,
         ensure_id/1
        ]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(Context) ->
    Context1 = z_context:set_resp_header(<<"x-frame-options">>, <<"SAMEORIGIN">>, Context),
    Context2 = z_admin_controller_helper:init_session(Context1),
    {Context3, Id} = ensure_id(Context2),
    z_acl:wm_is_authorized([{use, mod_admin}, {view, Id}], admin_logon, Context3).


resource_exists(Context) ->
    {Context2, Id} = ensure_id(Context),
    case Id of
        undefined -> {false, Context2};
        _N -> {m_rsc:exists(Id, Context2), Context2}
    end.

previously_existed(Context) ->
    {Context1, Id} = ensure_id(Context),
    IsGone = m_rsc_gone:is_gone(Id, Context1),
    {IsGone, Context1}.

moved_temporarily(Context) ->
    {Context1, Id} = ensure_id(Context),
    redirect(m_rsc_gone:get_new_location(Id, Context1), Context1).

redirect(undefined, Context) ->
    {false, Context};
redirect(Location, Context) ->
    {{true, Location}, Context}.


html(Context) ->
    Id = z_context:get(id, Context),
    Blocks = z_notifier:foldr(#admin_edit_blocks{id=Id}, [], Context),
    Vars = [
            {id, Id},
            {blocks, lists:sort(Blocks)}
            | z_context:get_all(Context)
           ],
    Html = z_template:render(z_context:get(template, Context, {cat, "admin_edit.tpl"}), Vars, Context),
    z_context:output(Html, Context).


%% @doc Fetch the (numerical) page id from the request
ensure_id(Context) ->
    case z_context:get(id, Context) of
        N when is_integer(N) ->
            {Context, N};
        undefined ->
            MaybeId = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
            {z_context:set(id, MaybeId, Context), MaybeId}
    end.


%% @doc Handle the submit of the resource edit form
event(#submit{message=rscform} = Msg, Context) ->
    event(Msg#submit{message={rscform, []}}, Context);
event(#submit{message={rscform, Args}}, Context) ->
    Post = z_context:get_q_all_noz(Context),
    Props = filter_props(Post),
    Id = z_convert:to_integer(proplists:get_value(<<"id">>, Props)),
    Props1 = proplists:delete(<<"id">>, Props),
    CatBefore = m_rsc:p(Id, category_id, Context),
    Props2 = z_notifier:foldl(#admin_rscform{id=Id, is_a=m_rsc:is_a(Id, Context)}, Props1, Context),
    case m_rsc:update(Id, Props2, Context) of
        {ok, _} ->
            case proplists:is_defined(<<"save_view">>, Post) of
                true ->
                    case proplists:get_value(view_location, Args) of
                        undefined ->
                            PageUrl = m_rsc:p(Id, page_url, Context),
                            z_render:wire({redirect, [{location, PageUrl}]}, Context);
                        Location ->
                            z_render:wire({redirect, [{location, Location}]}, Context)
                    end;
                false ->
                    case m_rsc:p(Id, category_id, Context) of
                        CatBefore ->
                            Context1 = z_render:set_value("field-name", m_rsc:p(Id, name, Context), Context),
                            Context2 = z_render:set_value("field-uri",  m_rsc:p(Id, uri, Context), Context1),
                            Context3 = z_render:set_value("field-page-path",  m_rsc:p(Id, page_path, Context), Context2),
                            Context4 = z_render:set_value("website",  m_rsc:p(Id, website, Context), Context3),
                            Context4a = z_render:set_value("slug",  m_rsc:p(Id, slug, Context), Context4),
                            Context5 = case z_convert:to_bool(m_rsc:p(Id, is_protected, Context)) of
                                           true ->  z_render:wire("delete-button", {disable, []}, Context4a);
                                           false -> z_render:wire("delete-button", {enable, []}, Context4a)
                                       end,
                            Title = z_trans:lookup_fallback(m_rsc:p(Id, title, Context5), Context5),
                            Context6 = z_render:growl([<<"Saved \"">>, Title, <<"\".">>], Context5),
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
        {error, duplicate_page_path} ->
            z_render:growl_error("Error, duplicate page path. Please change the uri.", Context);
        {error, duplicate_name} ->
            z_render:growl_error("Error, duplicate name. Please change the name.", Context);
        {error, eacces} ->
            z_render:growl_error("You don't have permission to edit this page.", Context);
        {error, invalid_query} ->
            z_render:growl_error("Your search query is invalid. Please correct it before saving.", Context);
        {error, Message} when is_list(Message); is_binary(Message) ->
            z_render:growl_error(Message, Context)
    end;

%% Opts: rsc_id, div_id, edge_template
event(#postback{message={reload_media, Opts}}, Context) ->
    DivId = proplists:get_value(div_id, Opts),
    {Html, Context1} = z_template:render_to_iolist({cat, "_edit_media.tpl"}, Opts, Context),
    z_render:update(DivId, Html, Context1);

event(#sort{items=Sorted, drop={dragdrop, {object_sorter, Props}, _, _}}, Context) ->
    RscId     = proplists:get_value(id, Props),
    Predicate = proplists:get_value(predicate, Props),
    EdgeIds   = [ EdgeId || {dragdrop, EdgeId, _, _ElementId} <- Sorted ],
    m_edge:update_sequence_edge_ids(RscId, Predicate, EdgeIds, Context),
    Context;

%% Previewing the results of a query in the admin edit
event(#postback{message={query_preview, Opts}}, Context) ->
    DivId = proplists:get_value(div_id, Opts),
    try
        Q = search_query:parse_query_text(z_context:get_q("triggervalue", Context)),
        S = z_search:search({'query', Q}, Context),
        {Html, Context1} = z_template:render_to_iolist("_admin_query_preview.tpl", [{result,S}], Context),
        z_render:update(DivId, Html, Context1)
    catch
        _: {error, {Kind, Arg}} ->
            z_render:growl_error(["There is an error in your query: ", Kind, " - ", Arg], Context)
    end.


%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
              <<"triggervalue">>,
              <<"postback">>,
              <<"z_trigger_id">>,
              <<"z_pageid">>,
              <<"z_submitter">>,
              <<"trigger_value">>,
              <<"save_view">>,
              <<"save_duplicate">>,
              <<"save_stay">>
             ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
