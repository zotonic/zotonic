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
    forbidden/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
    ]).

-include_lib("controller_webmachine_helper.hrl").
-include_lib("zotonic.hrl").


%% Timeout for comet flush when there is no data, webmachine 0.x had a timeout of 60 seconds, so leave after 55
-define(COMET_FLUSH_EMPTY, 55000).

%% Timeout for comet flush when there is data, allow for 100 msec more to gather extra data before flushing
-define(COMET_FLUSH_DATA,  100).


init(_Args) -> {ok, []}.

malformed_request(ReqData, _Context) ->
    Context = z_context:new(ReqData, ?MODULE),
    z_context:lager_md(Context),
    ?WM_REPLY(false, Context).

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    %% Ensure all, but don't start a new session
    Context2 = z_context:set(no_session, true, Context1),
    Context3 = z_context:ensure_all(Context2),
    z_context:lager_md(Context3),
    ?WM_REPLY(not z_context:has_session(Context3), Context3).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.


%% @doc Collect all scripts to be pushed back to the user agent
process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    MRef = erlang:monitor(process, Context1#context.page_pid),
    z_session_page:comet_attach(self(), Context1#context.page_pid),
    TRefFinal = erlang:send_after(?COMET_FLUSH_EMPTY, self(), flush_empty),
    process_post_loop(Context1, TRefFinal, undefined, MRef).


%% @doc Wait for all scripts to be pushed to the user agent.
process_post_loop(Context, TRefFinal, TRefData, MPageRef) ->
    receive
        flush_empty ->
            Data = z_session_page:get_transport_data(Context#context.page_pid),
            flush(true, Data, TRefFinal, TRefData, MPageRef, Context);

        flush_data ->
            Data = z_session_page:get_transport_data(Context#context.page_pid),
            flush(false, Data, TRefFinal, TRefData, MPageRef, Context);

        transport ->
            TRef1 = case TRefData of
                        undefined  -> erlang:send_after(?COMET_FLUSH_DATA, self(), flush_data);
                        _ -> TRefData
                    end,
            process_post_loop(Context, TRefFinal, TRef1, MPageRef);

        {'DOWN', _MonitorRef, process, Pid, _Info} when Pid == Context#context.page_pid ->
            Context1 = Context#context{page_pid=undefined},
            cancel_timer(TRefFinal),
            cancel_timer(TRefData),
            ?WM_REPLY(true, Context1);

        Msg ->
            lager:warning("Unknown comet loop message ~p", [Msg]),
            process_post_loop(Context, TRefFinal, TRefData, MPageRef)
    end.



flush(false, <<>>, TRefFinal, TRefData, MPageRef, Context) ->
    process_post_loop(Context, TRefFinal, TRefData, MPageRef);
flush(_IsFinal, Data, TRefFinal, TRefData, MPageRef, Context) ->
    erlang:demonitor(MPageRef, [flush]),
    cancel_timer(TRefFinal),
    cancel_timer(TRefData),
    z_session_page:comet_detach(Context#context.page_pid),
    RD  = z_context:get_reqdata(Context),
    RD1 = wrq:append_to_response_body(Data, RD),
    Context1 = z_context:set_reqdata(RD1, Context),
    ?WM_REPLY(true, Context1).

cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).
