%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell
%% @doc Handles comet long polls from the user agent

%% Copyright 2009-2017 Marc Worrell
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
    service_available/1,
    malformed_request/1,
    allowed_methods/1,
    content_types_provided/1,
    process_post/1
    ]).

-include_lib("zotonic.hrl").


service_available(Context) ->
    Context1 = z_context:continue_session(Context),
    z_context:lager_md(Context1),
    {true, Context1}.

%% @doc Expect an UBF encoded #z_msg_v1 record in the POST
malformed_request(Context0) ->
    try
        {Data, Context} = cowmachine_req:req_body(Context0),
        {ok, #z_msg_v1{} = ZMsg, _Rest} = z_transport:data_decode(Data),
        Context1 = z_context:set(z_msg, ZMsg, Context),
        {ZMsg#z_msg_v1.delegate =/= '$comet', Context1}
    catch _:_ ->
        {true, Context0}
    end.

allowed_methods(Context) ->
    {[<<"POST">>], Context}.

content_types_provided(Context) ->
    {[
        {<<"text/x-ubf">>, process_post},
        {<<"text/plain">>, process_post}
    ], Context}.

%% @doc Collect all data to be pushed back to the user agent
process_post(Context) ->
    case z_context:get_req_header(<<"content-type">>, Context) of
        <<"text/x-ubf", _/binary>> ->
            process_post_ubf(Context);
        <<"text/plain", _/binary>> ->
            process_post_ubf(Context);
        _ ->
            {{halt, 415}, Context}
    end.

process_post_ubf(Context) ->
    Term = z_context:get(z_msg, Context),
    {ok, Rs, Context2} = z_transport:incoming(Term, Context),
    {ok, ReplyData} = z_transport:data_encode(Rs),
    Context3 = cowmachine_req:set_resp_body(ReplyData, Context2),
    {true, Context3}.

