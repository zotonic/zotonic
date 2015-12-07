%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Handles comet long polls from the user agent

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

-module(controller_comet).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1, 
    service_available/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
    ]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").


init(_Args) -> {ok, []}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:continue_session(z_context:set(DispatchArgs, Context)),
    z_context:lager_md(Context1),
    ?WM_REPLY(true, Context1).

%% @doc Expect an UBF encoded #z_msg_v1 record in the POST
malformed_request(ReqData, Context0) ->
    try
        {Data, RD1} = wrq:req_body(ReqData),
        {ok, #z_msg_v1{} = ZMsg, _Rest} = z_transport:data_decode(Data),
        Context = ?WM_REQ(RD1, Context0),
        z_context:lager_md(Context),
        Context1 = z_context:set(z_msg, ZMsg, Context),
        ?WM_REPLY(ZMsg#z_msg_v1.delegate =/= '$comet', Context1)
    catch _:_ ->
        {true, ReqData, Context0}
    end.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.
    
content_types_provided(ReqData, Context) ->
    {[{"text/x-ubf", undefined}], ReqData, Context}.

%% @doc Collect all data to be pushed back to the user agent
process_post(ReqData, Context) ->
    case wrq:get_req_header_lc("content-type", ReqData) of
        "text/x-ubf" ++ _ ->
            process_post_ubf(ReqData, Context);
        "text/plain" ++ _ ->
            process_post_ubf(ReqData, Context);
        _ ->
            {{halt, 415}, ReqData, Context}
    end. 

process_post_ubf(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Term = z_context:get(z_msg, Context1),
    {ok, Rs, Context2} = z_transport:incoming(Term, Context1),
    {ok, ReplyData} = z_transport:data_encode(Rs),
    {x, RD, Context3} = ?WM_REPLY(x, Context2),
    RD1 = wrq:append_to_resp_body(ReplyData, RD),
    {true, RD1, Context3}.

