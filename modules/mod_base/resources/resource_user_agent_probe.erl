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
-author("Marc Worrell <marc@worrell>").
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    malformed_request/2,
    post_is_create/2,
    process_post/2
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
    {['POST'], ReqData, Context}.

%% Check all parameters
%%
malformed_request(ReqData, Context) ->
    C1 = ?WM_REQ(ReqData, Context),
    try
        C2 = z_context:set(ua_args, [
                            {width, list_to_integer(z_context:get_q("w", C1))},
                            {height, list_to_integer(z_context:get_q("h", C1))},
                            {is_touch, z_convert:to_bool(z_context:get_q("t", C1))}
                           ],
                           C1),
        ?WM_REPLY(false, C2)
    catch
        _:_ -> {true, ReqData, C1}
    end.

%% The probe does not create a new resource.
%%
post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

%% Process the probe. Send back a 204 when the user agent class does not 
%% change. Force a reload of the page with a 303 See Other when a reload 
%% is requested.
%%
process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    UaArgs = z_context:get(ua_args, Context1),
    ReplyContext = case z_user_agent:ua_probe(UaArgs, Context1) of
        {reload, ReloadContext} -> 
            to_referer(ReloadContext);
        {ok, CookieContext} -> 
            no_reply(CookieContext);
        ok -> 
            no_reply(Context1)
    end,
    ?WM_REPLY(true, ReplyContext).

to_referer(Context) ->
    RD = z_context:get_reqdata(Context),
    Referer = wrq:get_req_header_lc("referer", RD),
    RD1 = wrq:do_redirect(true, 
        wrq:set_resp_header("location", Referer, RD)),
    z_context:set_reqdata(RD1, Context).

no_reply(Context) ->
    RD = z_context:get_reqdata(Context),
    RD1 = wrq:set_resp_body(<<>>, RD),
    z_context:set_reqdata(RD1, Context).

