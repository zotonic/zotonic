%% @author Marc Worrell, Arjan Scherpenisse
%% @copyright 2009-2025 Marc Worrell, Arjan Scherpenisse
%% @doc Admin webmachine_controller.
%% @end

%% Copyright 2009-2025 Marc Worrell, Arjan Scherpenisse
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

-export([
    service_available/1,
    resource_exists/1,
    previously_existed/1,
    moved_temporarily/1,
    is_authorized/1,
    event/2,
    filter_props/1,
    ensure_id/1,
    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    Context1 = z_context:set_resp_header(<<"x-frame-options">>, <<"SAMEORIGIN">>, Context),
    {Context2, Id} = ensure_id(Context1),
    z_controller_helper:is_authorized(Id, [ {use, mod_admin}, {view, Id} ], Context2).

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


process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Id = z_context:get(id, Context),
    Context1 = z_context:set_resource_headers(Id, Context),
    Blocks = z_notifier:foldr(#admin_edit_blocks{id=Id}, [], Context1),
    Vars = [
            {id, Id},
            {blocks, lists:sort(Blocks)}
            | z_context:get_all(Context1)
           ],
    Html = z_template:render(z_context:get(template, Context, {cat, "admin_edit.tpl"}), Vars, Context1),
    z_context:output(Html, Context1).


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
    CatBefore = m_rsc:p(Id, <<"category_id">>, Context),
    Props2 = z_notifier:foldl(
        #admin_rscform{
            id = Id,
            is_a = m_rsc:is_a(Id, Context)
        },
        Props1,
        Context),
    UpdateOptions = case proplists:get_value(default_tz, Args) of
        undefined -> [];
        Tz -> [ {default_tz, Tz} ]
    end,
    case m_rsc:update(Id, Props2, UpdateOptions, Context) of
        {ok, _} ->
            case z_context:get_q(<<"z_submitter">>, Context) of
                SaveView when SaveView =:= <<"save_view">>;
                              SaveView =:= <<"save_view_float">> ->
                    case proplists:get_value(view_location, Args) of
                        undefined ->
                            PageUrl = m_rsc:p(Id, <<"page_url">>, Context),
                            z_render:wire({redirect, [{location, PageUrl}]}, Context);
                        Location ->
                            z_render:wire({redirect, [{location, Location}]}, Context)
                    end;
                Submitter ->
                    case m_rsc:p(Id, <<"category_id">>, Context) of
                        CatBefore ->
                            Context1 = update_rsc_form(Id, Context),
                            Title = z_convert:to_binary(
                                z_trans:lookup_fallback(m_rsc:p(Id, <<"title">>, Context1), Context1)),
                            Context2 = z_render:growl([<<"Saved \"">>, Title, <<"\".">>], Context1),
                            case Submitter of
                                <<"save_duplicate">> ->
                                    z_render:wire({dialog_duplicate_rsc, [{id, Id}]}, Context2);
                                _SaveStay ->
                                    z_render:wire(proplists:get_all_values(on_success, Args), Context2)
                            end;
                        _CatOther ->
                            z_render:wire({reload, []}, Context)
                    end
            end;
        {error, duplicate_uri} ->
            z_render:growl_error(?__("Error, duplicate uri. Please change the uri.", Context), Context);
        {error, duplicate_page_path} ->
            z_render:growl_error(?__("Error, duplicate page path. Please change the uri.", Context), Context);
        {error, duplicate_name} ->
            z_render:growl_error(?__("Error, duplicate name. Please change the name.", Context), Context);
        {error, eacces} ->
            z_render:growl_error(?__("You don't have permission to edit this page.", Context), Context);
        {error, invalid_query} ->
            z_render:growl_error(?__("Your search query is invalid. Please correct it before saving.", Context), Context);
        {error, Message} when is_list(Message); is_binary(Message) ->
            z_render:growl_error(Message, Context)
    end;

%% Opts: rsc_id, div_id, edge_template
event(#postback{message={reload_media, Opts}}, Context) ->
    DivId = proplists:get_value(div_id, Opts),
    {Html, Context1} = z_template:render_to_iolist({cat, "_edit_media.tpl"}, Opts, Context),
    z_render:update(DivId, Html, Context1);

event(#postback{message={delete_media, Opts}}, Context) ->
    {id, Id} = proplists:lookup(id, Opts),
    case m_media:delete(Id, Context) of
        ok ->
            Context;
        {error, eacces} ->
            z_render:growl_error("Sorry, you have no permission to edit this page.", Context)
    end;

event(#sort{items=Sorted, drop={dragdrop, {object_sorter, Props}, _, _}}, Context) ->
    RscId     = proplists:get_value(id, Props),
    Predicate = proplists:get_value(predicate, Props),
    EdgeIds   = [ EdgeId || {dragdrop, EdgeId, _, _ElementId} <- Sorted ],
    m_edge:update_sequence_edge_ids(RscId, Predicate, EdgeIds, Context),
    Context;

%% Previewing the results of a query in the admin edit
event(#postback{message={query_preview, Opts}}, Context) ->
    DivId = proplists:get_value(div_id, Opts),
    RscId = proplists:get_value(rsc_id, Opts),
    try
        Q = z_search_props:from_text(z_context:get_q(<<"triggervalue">>, Context)),
        S = z_search:search(<<"query">>, Q, 1, 20, Context),
        Vars = [
            {id, RscId},
            {result, S}
        ],
        {Html, Context1} = z_template:render_to_iolist("_admin_query_preview.tpl", Vars, Context),
        z_render:update(DivId, Html, Context1)
    catch
        _: {error, {Kind, Arg}} ->
            z_render:growl_error([?__("There is an error in your query:", Context), " ", Kind, " - ", Arg], Context)
    end.

set_value_slug(undefined, Context) ->
    set_value_slug(<<>>, Context);
set_value_slug(#trans{ tr = Tr }, Context) ->
    lists:foldl(
        fun({Lang, V}, Ctx) ->
            z_render:set_value(
                "title_slug--" ++ atom_to_list(Lang),
                V,
                Ctx)
        end,
        Context,
        Tr);
set_value_slug(Slug, Context) ->
    z_render:set_value("title_slug", Slug, Context).

%% @doc Remove the submit buttons from the resource properties.
filter_props(Fs) ->
    Remove = [
        <<"save_view">>,
        <<"save_view_float">>,
        <<"save_duplicate">>,
        <<"save_stay">>,
        <<"save_stay_float">>
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).

%% @doc Patch the rsc edit form with the new values.
update_rsc_form(Id, Context) ->
    Context1 = z_render:set_value("field-name", m_rsc:p(Id, <<"name">>, Context), Context),
    Context2 = z_render:set_value("field-uri",  m_rsc:p(Id, <<"uri_raw">>, Context), Context1),
    Context3 = update_rsc_page_path(m_rsc:p(Id, <<"page_path">>, Context), Context2),
    Context4 = z_render:set_value("website",  m_rsc:p(Id, <<"website">>, Context), Context3),
    Context5 = set_value_slug(m_rsc:p(Id, <<"title_slug">>, Context), Context4),
    case z_convert:to_bool(m_rsc:p(Id, <<"is_protected">>, Context))
        andalso z_acl:rsc_deletable(Id, Context)
    of
        true ->  z_render:wire("delete-button", {disable, []}, Context5);
        false -> z_render:wire("delete-button", {enable, []}, Context5)
    end.

update_rsc_page_path(undefined, Context) ->
    z_render:set_value("field-page-path", <<>>, Context);
update_rsc_page_path(Path, Context) when is_binary(Path) ->
    Tr = #trans{ tr = [ {z_language:default_language(Context), Path} ]},
    update_rsc_page_path(Tr, Context);
update_rsc_page_path(#trans{} = TransPath, Context) ->
    lists:foldl(
        fun(Lang, CAcc) ->
            Path = z_trans:lookup_fallback(TransPath, [ Lang ], CAcc),
            Path1 = filter_urldecode:urldecode(z_convert:to_binary(Path), Context),
            EltId = <<"field-page-path--", (atom_to_binary(Lang))/binary>>,
            z_render:set_value(EltId, Path1, CAcc)
        end,
        Context,
        z_language:editable_language_codes(Context)).
