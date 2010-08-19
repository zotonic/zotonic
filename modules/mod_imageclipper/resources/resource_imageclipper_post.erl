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

-module(resource_imageclipper_post).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    init/1, 
    is_authorized/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
    ]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init([]) -> {ok, []}.

malformed_request(ReqData, _Context) ->
    Context1 = z_context:new(ReqData, ?MODULE),
    Context2 = z_context:ensure_qs(Context1),
    case z_context:get_q_all("url", Context2) of
        [] ->
            ?WM_REPLY(true, Context2);
        _ ->
            ?WM_REPLY(false, Context2)
    end.

is_authorized(ReqData, Context) ->
    z_acl:wm_is_authorized(use, mod_imageclipper, ReqData, Context).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Urls = z_context:get_q_all("url", Context1),
    ?DEBUG(Urls),
    Ids = [upload(Url, Context1) || Url <- Urls],
    z_session:set(new_imageclipper_items, Ids, Context1),
    ?DEBUG(Ids),

    Redirect = "/",
    ReqData1 = wrq:set_resp_header("Location", Redirect, ReqData),
    {{halt, 301}, ReqData1, Context}.


upload(Url, Context) ->
    {ok, Id} = m_media:insert_url(Url, Context),
    {ok, Id} = m_rsc:update(Id, [{category, clipping}, {uploaded_with, mod_imageclipper}], Context),
    {ok, _} = m_edge:insert(Id, author, z_acl:user(Context), Context),
    Id.
