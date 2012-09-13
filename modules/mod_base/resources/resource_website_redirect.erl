%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2009-11-15
%% @doc Redirect to the URL of a resource of type 'website'.

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

-module(resource_website_redirect).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    init/1,
	service_available/2,
    resource_exists/2,
    previously_existed/2,
    moved_temporarily/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(
                    z_context:continue_session(Context1)),
    Id = m_rsc:rid(z_context:get_q("id", ContextQs), ContextQs),
    Exists = m_rsc:exists(Id, ContextQs)
             andalso z_acl:rsc_visible(Id, ContextQs),
    ?WM_REPLY(Exists, ContextQs).

moved_temporarily(ReqData, Context) ->
    Id = m_rsc:rid(z_context:get_q("id", Context), Context),
    case m_rsc:p(Id, website, Context) of
        undefined ->
            {false, ReqData, Context};
        <<>> ->
            {false, ReqData, Context};
        Url ->
            AbsUrl = iolist_to_binary(z_context:abs_url(z_html:unescape(Url), Context)),
            {{true, AbsUrl}, ReqData, Context}
    end.

