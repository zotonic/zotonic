%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012 Marc Worrell <marc@worrell.nl>
%% @doc Change the selected user agent, by letting him choose from a form/links etc.

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

-module(controller_user_agent_select).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    service_available/2,
    resource_exists/2,
    previously_existed/2,
    moved_temporarily/2
]).

-include_lib("webmachine_controller.hrl").
-include_lib("zotonic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    ContextArgs = z_context:set(DispatchArgs, Context),
    ContextSession = z_context:continue_session(ContextArgs),
    ContextQs = z_context:ensure_qs(ContextSession),
    ?WM_REPLY(true, ContextQs).


resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.


previously_existed(ReqData, Context) ->
    C1 = ?WM_REQ(ReqData, Context),
    case z_context:get_q("ua_class", C1) of
        "automatic" ->
            C2 = z_user_agent:ua_select(automatic, C1),
            ?WM_REPLY(true, C2);
        UAClassQ ->
            case z_user_agent:to_ua_class(UAClassQ) of
                undefined ->
                    ?WM_REPLY(false, C1);
                UAClass ->
                    C2 = z_user_agent:ua_select(UAClass, C1),
                    ?WM_REPLY(true, C2)
            end
    end.


moved_temporarily(ReqData, Context) ->
    C1 = z_context:set_nocache_headers(?WM_REQ(ReqData, Context)),
    Loc = case z_context:get_q("p", C1, []) of
            [] ->
                case z_context:get_req_header("referer", C1) of
                    undefined -> "/";
                    Referrer ->  z_html:noscript(Referrer)
                end;
            [$/|_] = Page ->
                z_html:noscript(Page);
            Page ->
                z_html:noscript([$/|Page])
         end,
    ?WM_REPLY({true, z_context:abs_url(Loc, C1)}, C1).



