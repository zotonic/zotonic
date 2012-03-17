%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-06-09
%% @doc Administrative interface.  Aka backend.

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

-module(mod_admin).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Admin module").
-mod_description("Provides administrative interface for editing pages, media, users etc.").
-mod_depends([base, authentication]).
-mod_provides([admin]).
-mod_prio(1000).

-export([
         observe_sanitize_element/3,
         observe_admin_menu/3
]).

-include_lib("zotonic.hrl").


%% @doc Fix tinymce images that are the result of copying
%% <img class="z-tinymce-media z-tinymce-media-align-block z-tinymce-media-size-small z-tinymce-media-crop- z-tinymce-media-link- " 
%%      src="/admin/media/preview/41113" 
%%      alt="" />
observe_sanitize_element(sanitize_element, {<<"img">>, Attrs, _Enclosed} = Element, Context) ->
    case proplists:get_value(<<"src">>, Attrs) of
        <<"/admin/media/preview/", Number/binary>> ->
            NumberList = binary_to_list(Number),
            case m_rsc:rid(NumberList, Context) of
                undefined ->
                    {nop, []};
                ImgId ->
                    CommentText = [
                        <<" z-media ">>,
                        integer_to_list(ImgId),
                        32,
                        class_to_opts(proplists:get_value(<<"class">>, Attrs))
                    ],
                    ?DEBUG({comment, iolist_to_binary(CommentText)})
            end;
        _OtherSrc ->
            Element
    end;
observe_sanitize_element(sanitize_element, Element, _Context) ->
    Element.


class_to_opts(undefined) ->
    [];
class_to_opts(Class) ->
    case re:run(Class, "z-tinymce-media-([a-z]+)-([a-z]*)", [{capture, all_but_first, binary}, global]) of
        nomatch ->
            [];
        {match, Ms} ->
            [
                mochijson:encode({struct, [{A,V} || [A,V] <- Ms]}),
                32
            ]
    end.

    
observe_admin_menu(admin_menu, Acc, Context) ->
    [
     {admin_dashboard, {undefined, ?__("Dashboard", Context), {url, admin} }},

     %% CONTENT %%
     {admin_content,   {undefined, ?__("Content", Context), undefined}},

     {overview,        {admin_content,   ?__("Pages", Context), {url, admin_overview_rsc}}},
     {admin_media,     {admin_content,   ?__("Media", Context), {url, admin_media}}},
     
     {admin_structure,  {undefined, ?__("Structure", Context), undefined}},
     {admin_categories, {admin_content,   ?__("Categories", Context),
                         {url, admin_categories_manager}}},

     
     {admin_modules,   {undefined, ?__("Modules", Context), undefined}},
     
     {admin_auth,      {undefined, ?__("Auth", Context), undefined}},

     {admin_system,    {undefined, ?__("System", Context), undefined}},
     {admin_status,     {admin_system,   ?__("Status", Context), {url, admin_status}}}

     |Acc].

