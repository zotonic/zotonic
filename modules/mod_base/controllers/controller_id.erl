%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% Date: 2009-04-28
%% @doc Redirect to resources depening on the content type requested.

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

-module(controller_id).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    resource_exists/2,
    content_types_provided/2,
    see_other/2
]).

-include_lib("webmachine_controller.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    {Id, ContextQs} = get_id(z_context:ensure_qs(z_context:continue_session(Context1))),
    ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs).

content_types_provided(ReqData, Context) ->
    {CT,Context1} = get_content_types(Context),
    CT1 = [{Mime, see_other} || {Mime, _Dispatch} <- CT],
    {CT1, ReqData, Context1}.

see_other(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Mime = z_context:get_resp_header("Content-Type", Context1),
    {CT,Context2} = get_content_types(Context1),
    {Id, Context3} = get_id(Context2),
    {Location,Context4} = case proplists:get_value(Mime, CT) of
                            page_url ->
                                ContextSession = z_context:continue_session(Context3),
                                {m_rsc:p_no_acl(Id, page_url, ContextSession), ContextSession};
                            Dispatch -> 
                                {z_dispatcher:url_for(Dispatch, [{id,Id}], Context3), Context3}
                          end,
    AbsUrl = z_context:abs_url(Location, Context4),
    Context5 = z_context:set_resp_header("Location", AbsUrl, Context4),
    ?WM_REPLY({halt, 303}, Context5).

%% @doc Fetch the list of content types provided, together with their dispatch rule name.
%% text/html is moved to the front of the list as that is the default mime type to be returned.
get_content_types(Context) ->
    case z_context:get(content_types_dispatch, Context) of
        undefined ->
            {Id, Context1} = get_id(Context),
            CT = z_notifier:foldr(#content_types_dispatch{id=Id}, [], Context1),
            CT1 = case proplists:get_value("text/html", CT) of
                    undefined -> [{"text/html", page_url}|CT];
                    Prov -> [Prov|CT]
                  end,
            Context2 = z_context:set(content_types_dispatch, CT1, Context1),
            {CT1, Context2};
        CT -> 
            {CT, Context}
    end.
    
get_id(Context) ->
    case z_context:get(id, Context) of
        undefined ->
            case z_context:get_q("id", Context) of
                undefined ->
                    {undefined, Context};
                [] ->
                    {undefined, Context};
                Id ->
                    RscId = m_rsc:rid(Id, Context),
                    {RscId, z_context:set(id, {ok, RscId}, Context)}
            end;
        {ok, Id} ->
            {Id, Context}
    end.
