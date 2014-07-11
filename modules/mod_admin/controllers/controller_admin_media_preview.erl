%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @doc Redirect to a preview for media items; for use in the tinyMCE media plugin.

%% Copyright 2009 Arjan Scherpenisse
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

-module(controller_admin_media_preview).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([init/1,
         service_available/2,
         resource_exists/2,
         content_types_provided/2,
         to_image/2
        ]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").

init(_Args) -> {ok, []}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_admin_controller_helper:init_session(Context1),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_admin_controller_helper:init_session(Context1),
    case z_context:get_q("id", Context2) of
        undefined ->
            ?WM_REPLY(false, Context2);
        [] ->
            ?WM_REPLY(false, Context2);
        Id ->
            case m_rsc:rid(Id, Context2) of
                undefined -> 
                    ?WM_REPLY(false, Context2);
                RscId -> 
                    case m_rsc:exists(RscId, Context2) andalso m_rsc:is_visible(RscId, Context2) of 
                        true ->
                            Context3 = z_context:set(id, RscId, Context2),
                            ?WM_REPLY(true, Context3);
                        false ->
                            ?WM_REPLY(true, Context2)
                    end
            end
    end.

content_types_provided(ReqData, Context) ->
    {[{"image/jpeg", to_image}], ReqData, Context}.

to_image(ReqData, Context) ->
    Opts = [{mediaclass, "admin-editor"}],
    {ok, Url} = z_media_tag:url(z_context:get(id, Context), Opts, Context),
    ReqData1 = wrq:set_resp_header("Location", z_context:abs_url(Url, Context), ReqData),
    {{halt, 303}, ReqData1, Context}.
