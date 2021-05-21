%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Handle tickets for out of band MQTT actions via controller_mqtt_transport.

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

-module(m_mqtt_ticket).

-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,
    m_delete/3,

    new_ticket/1,
    exchange_ticket/2
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Create a new ticket for the current context, must be called via a MQTT connection.
-spec m_post( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_post([ <<"new">> ], _Msg, Context) ->
    new_ticket(Context);
m_post(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Delete ticket.
-spec m_delete( list( binary() ), zotonic_model:opt_msg(), z:context() ) -> {ok, term()} | ok | {error, term()}.
m_delete([ Ticket ], _Msg, Context) when is_binary(Ticket) ->
    delete_ticket(Ticket, Context);
m_delete(_Path, _Msg, _Context) ->
    {error, unknown_path}.


%% @doc Create a new ticket. This store the context and returns an unique ticket-id. This ticket can later
%%      be exchanged for the stored context. The stored context MUST contain the MQTT client-id.
-spec new_ticket( z:context() ) -> {ok, binary()} | {error, term()}.
new_ticket(Context) ->
    case z_context:client_id(Context) of
        {error, _} = Error ->
            Error;
        {ok, _ClientId} ->
            z_mqtt_ticket:new_ticket(Context)
    end.

%% @doc Delete a ticket.
-spec delete_ticket( binary(), z:context() ) -> ok | {error, term()}.
delete_ticket(Ticket, Context) ->
    z_mqtt_ticket:delete_ticket(Ticket, Context).

%% @doc Exchange a ticket for a previously saved MQTT connection context.
-spec exchange_ticket( binary(), z:context() ) -> {ok, z:context()} | {error, term()}.
exchange_ticket(Ticket, Context) ->
    z_mqtt_ticket:exchange_ticket(Ticket, Context).
