%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell
%% @doc User Agent selection

%% Copyright 2012 Marc Worrell
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

-module(resource_user_agent_probe).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    malformed_request/2,
    content_types_provided/2,
    to_js/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    ContextArgs = z_context:set(DispatchArgs, Context),
    ContextSession = z_context:continue_session(ContextArgs),
    ContextQs = z_context:ensure_qs(ContextSession),
    ?WM_REPLY(true, ContextQs).

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

malformed_request(ReqData, Context) ->
    try
        C1 = ?WM_REQ(ReqData, Context),
        C2 = z_context:set(ua_args, [
                            {width, list_to_integer(z_context:get_q("w", C1))},
                            {height, list_to_integer(z_context:get_q("h", C1))},
                            {is_touch, z_convert:to_bool(z_context:get_q("t", C1))}
                           ],
                           C1),
        ?WM_REPLY(false, C2)
    catch
        _:_ -> {true, ReqData, Context}
    end.

content_types_provided(ReqData, Context) -> 
    {[
        {"application/x-javascript", to_js}
     ], ReqData, Context }.

to_js(ReqData, Context) ->
    CRD = z_context:set_nocache_headers(?WM_REQ(ReqData, Context)),
    case z_user_agent:ua_probe(z_context:get(ua_args, CRD), CRD) of
        {reload, CR} ->
            ?WM_REPLY(<<"window.location.href = window.location.protocol+'//'+window.location.host+page.val();">>, CR);
        {ok, CR} ->
            ?WM_REPLY(<<>>, CR);
        ok ->
            ?WM_REPLY(<<>>, CRD)
    end.

