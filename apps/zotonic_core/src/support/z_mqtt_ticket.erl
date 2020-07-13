%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Tickets for MQTT out of band publish via HTTP

%% Copyright 2020 Marc Worrell
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

-module(z_mqtt_ticket).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-export([
    new_ticket/1,
    exchange_ticket/2
    ]).

% Unused tickets timeout in 30 seconds
-define(TICKET_TIMEOUT, 30000).

-record(state, {
    site :: atom(),
    tickets :: #{ binary() := z:context()}
    }).

%%====================================================================
%% API
%%====================================================================

%% @doc Return a new ticket id for out of band MQTT calls.
-spec new_ticket( z:context() ) -> {ok, binary()} | {error, term()}.
new_ticket(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    Context1 = z_context:prune_for_scomp(Context),
    gen_server:call(Name, {new_ticket, Context1}).


%% @doc Exchange a ticket for a stored context record.
-spec exchange_ticket( binary(), z:context() ) -> {ok, z:context()} | {error, term()}.
exchange_ticket(Ticket, Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:call(Name, {exchange_ticket, Ticket}).


%% @doc Starts the MQTT ticket server server
start_link(Site) ->
    Name = z_utils:name_for_site(?MODULE, Site),
    gen_server:start_link({local, Name}, ?MODULE, [ Site ], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ Site ]) ->
    State = #state{
        site = Site,
        tickets = #{}
    },
    {ok, State}.


handle_call({new_ticket, Context}, _From, #state{ tickets = Tickets } = State) ->
    Ticket = z_ids:id(32),
    Tickets1 = Tickets#{ Ticket => Context },
    timer:send_after(?TICKET_TIMEOUT, {ticket_timeout, Ticket}),
    State1 = State#state{ tickets = Tickets1 },
    {reply, {ok, Ticket}, State1};

handle_call({exchange_ticket, Ticket}, _From, #state{ tickets = Tickets } = State) ->
    case maps:find(Ticket, Tickets) of
        {ok, Context} ->
            Tickets1 = maps:remove(Ticket, Tickets),
            {reply, {ok, Context}, State#state{ tickets = Tickets1 }};
        error ->
            {reply, {error, enoent}, State}
    end;

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info({ticket_timeout, Ticket}, #state{ tickets = Tickets } = State) ->
    Tickets1 = maps:remove(Ticket, Tickets),
    {noreply, State#state{ tickets = Tickets1 }};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

