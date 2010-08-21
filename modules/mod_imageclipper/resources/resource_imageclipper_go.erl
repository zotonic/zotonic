%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc Post URLs which get transformed into images.

%% Copyright 2010 Arjan Scherpenisse <arjan@scherpenisse.net>
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

-module(resource_imageclipper_go).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
         is_authorized/2,
         allowed_methods/2,
         process_post/2,
         event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, _Context) ->
    Context = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_all(Context),
    case z_context:get_q("referer", Context1) of
        undefined -> nop;
        _ ->
            z_context:set_persistent(clipper_referer, z_context:get_q("referer", Context1), Context1),
            z_context:set_persistent(clipper_urls, z_context:get_q_all("url", Context1), Context1),
            z_context:set_persistent(clipper_aspects, z_context:get_q_all("aspect", Context1), Context1)
    end,
    z_acl:wm_is_authorized(use, mod_imageclipper, ReqData, Context1).


%% POST just redirect to the GET variant ( the post variables have been stored in the session )
process_post(ReqData, Context) ->
    Redirect = "/clipper/go",
    ReqData1 = wrq:set_resp_header("Location", Redirect, ReqData),
    {{halt, 301}, ReqData1, Context}.


html(Context) ->
    Referer = z_context:get_persistent(clipper_referer, Context),
    Urls = z_context:get_persistent(clipper_urls, Context),
    Vars = [{postback, z_render:make_postback_info({go, Urls, Referer, []}, go, undefined, undefined, ?MODULE, Context)},
            {aspects, [z_convert:to_float(A) || A <- z_context:get_persistent(clipper_aspects, Context)]}
           ],
    Html = z_template:render({cat, "clipper_go.tpl"}, Vars, Context),
	z_context:output(Html, Context).


allowed_methods(ReqData, Context) ->
    {['POST', 'GET'], ReqData, Context}.


event({postback, {go, [], Referer, Ids}, _TriggerId, _TargetId}, Context) ->
    %% All urls processed; do administration and redirect.
    z_context:set_persistent(clipper_referer, undefined, Context),
    z_context:set_persistent(clipper_urls, undefined, Context),
    z_context:set_persistent(clipper_aspects, undefined, Context),
    z_session:set(new_imageclipper_items, Ids, Context),
    z_context:add_script_page(["document.location.href=\"", Referer, "\";"], Context);

event({postback, {go, [Url|Rest], Referer, Ids}, TriggerId, _TargetId}, Context) ->
    %% Get the file and process it
    Id = upload(Url, Context),
    %% Feedback to the user
    z_context:add_script_page(["$('#img-",z_convert:to_list(length(Ids)),"').append($('<img>').attr('height', '100').attr('src', '", Url, "').hide().fadeIn());"], Context),
    %% Process the next
    Next = z_render:make_postback_info({go, Rest, Referer, [Id|Ids]}, go, undefined, undefined, ?MODULE, Context),
    z_context:add_script_page(["z_queue_postback('",TriggerId,"', '", Next,"', [], true);"], Context),
    Context.


upload(Url, Context) ->
    {ok, Id} = m_media:insert_url(Url, Context),
    {ok, Id} = m_rsc:update(Id, [{category, clipping}, {uploaded_with, mod_imageclipper}], Context),
    {ok, _} = m_edge:insert(Id, author, z_acl:user(Context), Context),
    Id.
