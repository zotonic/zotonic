%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-28

%% @doc Show a "fail whale" (503 service not available) when the site is too busy.

%% Copyright 2011 Arjan Scherpenisse
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

-module(mod_failwhale).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Fail whale").
-mod_description("Show a \"fail whale\" (503 service not available) when the site is too busy.").
-mod_prio(900).

-include_lib("include/zotonic.hrl").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
         observe_dispatch_rewrite/3
]).

-record(state, {context, requesttime, threshold, lastn}).

-define(LAST_N, 30). %% in seconds. Can be overridden in mod_failwhale.last_n config setting.
-define(THRESHOLD, 100). %% in milliseconds. Can be overridden in mod_failwhale.threshold config setting.

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    Host = proplists:get_value(host, Args),
    Name = name(?MODULE, Host),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).


%% @doc Rewrite the dispatch rules
observe_dispatch_rewrite(#dispatch_rewrite{}, Dispatch, Context) ->
    case check_request(Context) of
        true ->
           Dispatch;
        false ->
            {["failwhale"], []}
    end.

%% Get the request time from my gen_server
check_request(Context) ->
    gen_server:call(name(Context), check_request).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initiates the server.
init(Args) ->
    Context = proplists:get_value(context, Args),
    timer:send_interval(1000, probe),
    Threshold = z_convert:to_integer(m_config:get_value(?MODULE, threshold, ?THRESHOLD, Context)),
    LastN = z_convert:to_integer(m_config:get_value(?MODULE, lastn, ?LAST_N, Context)),
    {ok, #state{context=Context, threshold=Threshold, lastn=LastN}}.

%% @doc Check the request for the request time threshold. If above threshold, serve 503.
handle_call(check_request, _From, State=#state{requesttime=undefined}) ->
    {reply, true, State}; %% startup
handle_call(check_request, _From, State=#state{requesttime=RequestTime, threshold=Threshold}) ->
    {reply, RequestTime < Threshold, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @doc Update the load stats
%% Take the average request durations of the last 30 seconds as an
%% indication of average request time.
handle_info(probe, State=#state{requesttime=OldT, threshold=Threshold, context=Context}) ->
    {ok, Summary} = statz:summary({zotonic, webzmachine, duration}),
    LastN = State#state.lastn,
    Avgs = lists:sublist(lists:map(fun({_, 0}) -> 0;
                                      ({C, N}) -> C/N end, 
                                   proplists:get_value(second, Summary, [])),
                         LastN),
    T = case length(Avgs) of
            0 -> 'NaN';
            N -> lists:sum(Avgs)/N
        end,

    case OldT < Threshold andalso T >= Threshold of
        true ->
            ?zInfo("Exceeded request time. Serving 503 page.~n", Context);
        _ -> nop
    end,

    case OldT >= Threshold andalso T < Threshold of
        true ->
            ?zInfo("Request time back to normal.~n", Context);
        _ -> nop
    end,

    {noreply, State#state{requesttime=T}};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


name(Context) ->
    name(?MODULE, Context#context.host).
name(Module, Host) ->
    z_utils:name_for_host(Module, Host).
