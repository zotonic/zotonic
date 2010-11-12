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

-module(resource_comet).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1, 
    forbidden/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
    ]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").


%% Timeout for comet flush when there is no data, webmachine 0.x had a timeout of 60 seconds, so leave after 55
-define(COMET_FLUSH_EMPTY, 55000).

%% Timeout for comet flush when there is data, allow for 50 msec more to gather extra data before flushing
-define(COMET_FLUSH_DATA,  50).


init(_Args) -> {ok, []}.

malformed_request(ReqData, _Context) ->
    Context = z_context:new(ReqData),
    ?WM_REPLY(false, Context).

forbidden(ReqData, Context) ->
    %% TODO: prevent that we make a new ua session or a new page session, fail when a new session is needed
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(false, Context2).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.


%% @doc Collect all scripts to be pushed back to the user agent
process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    erlang:monitor(process, Context1#context.page_pid),
    z_session_page:comet_attach(self(), Context1#context.page_pid),
    {ok, TRef} = start_timer(?COMET_FLUSH_EMPTY),
    process_post_loop(Context1, TRef, false).


%% @doc Wait for all scripts to be pushed to the user agent.
process_post_loop(Context, TRef, HasData) ->
    receive
        flush ->
            timer:cancel(TRef),
            z_session_page:comet_detach(Context#context.page_pid),
            ?WM_REPLY(true, Context);

        script_queued ->
            Scripts = z_session_page:get_scripts(Context#context.page_pid),
            RD  = z_context:get_reqdata(Context),
            RD1 = wrq:append_to_response_body(Scripts, RD),
            Context1 = z_context:set_reqdata(RD1, Context),
            TRef1 = case HasData of
                        true  -> TRef;
                        false -> reset_timer(?COMET_FLUSH_DATA, TRef)
                    end,
            process_post_loop(Context1, TRef1, true);

        {'DOWN', _MonitorRef, process, Pid, _Info} when Pid == Context#context.page_pid ->
            self() ! flush,
            process_post_loop(Context, TRef, HasData)
    end.


start_timer(Delta) ->
    timer:send_after(Delta, flush).

reset_timer(Delta, TRef) ->
    timer:cancel(TRef),
    start_timer(Delta).
