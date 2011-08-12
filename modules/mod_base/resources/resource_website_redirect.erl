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
    resource_exists/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    Id = z_context:get_q("id", ContextQs),
    case m_rsc:exists(Id, ContextQs) of
        false ->
            ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs);
        true ->
            case m_category:is_a(m_rsc:p(Id, category_id, ContextQs), "website", ContextQs) of
                true ->
                    case m_rsc:p(Id, website, ContextQs) of
                        undefined ->
                            ?WM_REPLY(false, ContextQs);
                        <<>> ->
                            ?WM_REPLY(false, ContextQs);
                        Url ->
                            %% Redirect to the website.
                            AbsUrl = binary_to_list(filter_unescape:unescape(z_context:abs_url(Url, ContextQs), Context)),
                            Context3 = z_context:set_resp_header("Location", AbsUrl, ContextQs),
                            ?WM_REPLY({halt, 301}, Context3)
                    end;
                _ ->
                    ?WM_REPLY(false, ContextQs)
            end
    end.
