%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% Date: 2009-04-28
%%
%% @doc Redirects to a resource when the path matches the page_path.
%% @todo Consult the dispatch lists and simulate the controller that would have been selected
%% for the default url. When doing so we prevent the uri from being changed, giving a nicer
%% user experience.

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


-module(resource_page_path).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    resource_exists/2,
    content_types_provided/2,
    to_html/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.

resource_exists(ReqData, _Context) ->
    Context = z_context:new(ReqData, ?MODULE),
    case m_rsc:page_path_to_id(wrq:disp_path(ReqData), Context) of
        {ok, Id} ->
            Context2 = z_context:set(id, Id, Context),
            ?WM_REPLY(true, Context2);
        {error, _} ->
            ?WM_REPLY(false, Context)
    end.

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html}], ReqData, Context}.
 
to_html(ReqData, Context) ->
    Context1 = z_context:set_reqdata(ReqData, Context),
    Id       = z_context:get(id, Context1),
    Location = m_rsc:p_no_acl(Id, default_page_url, Context1),
    Url      = z_context:abs_url(Location, Context1),
    ReqData1 = wrq:set_resp_header("Location", Url, ReqData),
    ReqData2 = wrq:set_response_code(302, ReqData1),
    {{halt, 302}, ReqData2, Context}.
