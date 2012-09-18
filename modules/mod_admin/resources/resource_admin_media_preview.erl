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

-module(resource_admin_media_preview).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([init/1,
         resource_exists/2,
         is_authorized/2,
         content_types_provided/2,
         to_image/2
        ]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.

is_authorized(ReqData, _Args) ->
    Context = z_context:new(ReqData, ?MODULE),
    z_acl:wm_is_authorized(use, mod_admin, admin_logon, ReqData, Context).

resource_exists(ReqData, Context) ->
    case z_context:get_q("id", Context) of
        undefined ->
            {false, ReqData, Context};
        [] ->
            {false, ReqData, Context};
        Id ->
            case m_rsc:exists(Id, Context) of 
                true ->
                    case m_rsc:is_visible(Id, Context) of
                        true ->
                            Context2 = z_context:set("id", Id, Context),
                            {true, ReqData, Context2};
                        false ->
                            {false, ReqData, Context}
                    end;
                false ->
                    {false, ReqData, Context}
            end
    end.

content_types_provided(ReqData, Context) ->
    {[{"image/jpeg", to_image}], ReqData, Context}.

to_image(ReqData, Context) ->
    Opts = [{mediaclass, "admin-editor"}],
    Id = m_rsc:rid(z_context:get("id", Context), Context),
    {ok, Url} = z_media_tag:url(Id, Opts, Context),
    ReqData1 = wrq:set_resp_header("Location", binary_to_list(Url), ReqData),
    {{halt, 303}, ReqData1, Context}.
