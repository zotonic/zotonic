%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010 Marc Worrell
%% @date 2010-08-09
%% @doc Slideshow on the basis of collections.

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

-module(mod_slideshow).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Slideshow").
-mod_description("Add slideshows as media items.").
-mod_prio(600).

%% interface functions
-export([
    init/1,
    observe_media_viewer/2,
    observe_media_stillimage/2,
    observe_postback_notify/2
]).

-include_lib("zotonic.hrl").


%% Add 'collection' as a valid depiction for articles etc.
init(Context) ->
    case {m_category:name_to_id(collection, Context), m_predicate:name_to_id(depiction, Context)} of
        {{ok, CollId}, {ok, DepictId}} ->
            Objects = m_predicate:objects(DepictId, Context),
            case lists:member(CollId, Objects) of
                true -> nop;
                false -> 
                    Subjects = m_predicate:subjects(DepictId, Context),
                    Objects1 = [CollId|Objects],
                    m_predicate:update_noflush(DepictId, Subjects, Objects1, Context),
                    m_predicate:flush(Context)
            end;
        _ ->
            ok
    end.


%% @doc Return the media viewer for the embedded video (that is, when it is an embedded media).
%% @spec media_viewer(Notification, Context) -> undefined | {ok, Html}
observe_media_viewer({media_viewer, Id, _Props, _Filename, Options}, Context) ->
    case m_rsc:is_a(Id, collection, Context) of
        true ->
            % todo: handle possible extra js in the contexts
            Html = z_template:render("_slideshow_media.tpl", [{id,Id}, {parts, get_parts(Id, Context)}|Options], Context),
            {ok, Html};
        false ->
            undefined
    end.


%% @doc Return the filename of a still image to be used for image tags.
%% @spec media_stillimage(Notification, _Context) -> undefined | {ok, Filename}
observe_media_stillimage({media_stillimage, Id, _Props}, Context) ->
    case m_rsc:is_a(Id, collection, Context) of
        true ->
            % todo: create an image that represents this slideshow
            case find_depiction_1(Id, Context) of
                {ok, Filename} -> {ok, Filename};
                undefined -> find_depiction(get_parts(Id, Context), Context)
            end;
        false ->
            undefined
    end.
    
%% @doc Handle a request for a slideshow popup
observe_postback_notify({postback_notify, "slideshow"}, Context) ->
    Ids = [ list_to_integer(Id) || Id <- z_context:get_q_all("id", Context) ],
    Ids1 = [ Id || Id <- Ids, z_acl:rsc_visible(Id, Context) ],
    StartId = case z_context:get_q("start_id", Context) of
                [] -> undefined;
                QStartId -> list_to_integer(QStartId)
              end,
    z_render:dialog("", 
                    "_slideshow_dialog.tpl", 
                    [
                        {class, "slideshow-dialog"},
                        {parts, reorder_ids(StartId, Ids1)},
                        {width, 800}, 
                        {height, 600}
                    ],
                    Context);
observe_postback_notify(_, _Context) ->
    undefined.

    reorder_ids(undefined, Ids) ->
        Ids;
    reorder_ids(StartId, Ids) ->
        {Before,After} = lists:splitwith(fun(N) -> N /= StartId end, Ids),
        After ++ Before.


find_depiction([], _Context) ->
    undefined;
find_depiction([Id|Rest], Context) ->
    case find_depiction_1(Id, Context) of
        {ok, Filename} -> {ok, Filename};
        undefined -> find_depiction(Rest, Context)
    end.

find_depiction_1(Id, Context) ->
    case m_media:depiction(Id, Context) of
        L when is_list(L) ->
            case z_convert:to_list(proplists:get_value(preview_filename, L)) of
                [] ->
                    case z_media_preview:can_generate_preview(proplists:get_value(mime, L)) of
                        true -> {ok, z_convert:to_list(proplists:get_value(filename, L))};
                        false -> undefined
                    end;
                PreviewFilename -> {ok, PreviewFilename}
            end;
        undefined ->
            undefined
    end.


get_parts(Id, Context) ->
    case m_edge:objects(Id, haspart, Context) of
        [] -> m_edge:objects(Id, depiction, Context);
        Parts -> Parts
    end.

