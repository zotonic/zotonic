%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Handles comet long polls from the user agent

%% Copyright 2015 Marc Worrell
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

-module(z_transport_comet).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    comet_delegate/1,
    process_post_loop/4
    ]).

-include_lib("zotonic.hrl").

%% Timeout for comet flush when there is no data, webmachine 0.x had a timeout of 60 seconds, so leave after 55
-define(COMET_FLUSH_EMPTY, 55000).

%% Timeout for comet flush when there is data, allow for 100 msec more to gather extra data before flushing
%% This must be higher than SIDEJOB_TIMEOUT in controlelr_postback.erl
-define(COMET_FLUSH_DATA,  100).


%% @doc Collect all scripts to be pushed back to the user agent
comet_delegate(Context) ->
    MRef = erlang:monitor(process, Context#context.page_pid),
    case z_session_page:comet_attach(self(), Context#context.page_pid) of
        ok ->
            TRefFinal = erlang:send_after(?COMET_FLUSH_EMPTY, self(), flush_empty),
            process_post_loop(Context, TRefFinal, undefined, MRef);
        {error, _} = Error ->
            lager:info("[~p] Comet attach failed due to ~p", 
                       [z_context:site(Context), Error]),
            {ok, [], Context}
    end.

%% @doc Wait for all scripts to be pushed to the user agent.
process_post_loop(Context, TRefFinal, TRefData, MPageRef) ->
    receive
        flush_empty ->
            Msgs = z_session_page:get_transport_msgs(Context#context.page_pid),
            flush(true, Msgs, TRefFinal, TRefData, MPageRef, Context);

        flush_data ->
            Msgs = z_session_page:get_transport_msgs(Context#context.page_pid),
            flush(false, Msgs, TRefFinal, TRefData, MPageRef, Context);

        transport ->
            TRef1 = case TRefData of
                        undefined  ->
                            erlang:send_after(?COMET_FLUSH_DATA, self(), flush_data);
                        _ ->
                            TRefData
                    end,
            ?MODULE:process_post_loop(Context, TRefFinal, TRef1, MPageRef);

        close ->
            flush(true, [], TRefFinal, TRefData, MPageRef, Context);

        {final, Msgs} ->
            flush(true, Msgs, TRefFinal, TRefData, MPageRef, Context);

        {'DOWN', _MonitorRef, process, Pid, _Info} when Pid =:= Context#context.page_pid ->
            Context1 = Context#context{page_pid=undefined},
            maybe_cancel_timer(TRefFinal),
            maybe_cancel_timer(TRefData),
            {ok, [], Context1};

        Msg ->
            lager:warning("Unknown comet loop message ~p", [Msg]),
            ?MODULE:process_post_loop(Context, TRefFinal, TRefData, MPageRef)
    end.

flush(false, [], TRefFinal, TRefData, MPageRef, Context) ->
    maybe_cancel_timer(TRefData),
    ?MODULE:process_post_loop(Context, TRefFinal, undefined, MPageRef);
flush(_IsFinal, Msgs, TRefFinal, TRefData, MPageRef, Context) ->
    erlang:demonitor(MPageRef, [flush]),
    maybe_cancel_timer(TRefFinal),
    maybe_cancel_timer(TRefData),
    z_session_page:comet_detach(self(), Context#context.page_pid),
    {ok, Msgs, Context}.

maybe_cancel_timer(undefined) ->
    ok;
maybe_cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).
