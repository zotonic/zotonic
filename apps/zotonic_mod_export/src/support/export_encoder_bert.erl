%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for vevent format

%% Copyright 2016 Marc Worrell
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

-module(export_encoder_bert).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    rows = [] :: list( m_rsc:resource_id() )
}).

-export([
    extension/0,
    mime/0,
    init/2,
    header/3,
    row/3,
    footer/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

extension() ->
    [ <<"bert">> ].

mime() ->
    [ {<<"application">>, <<"x-bert">>, []} ].

init(_Options, _Context) ->
    {ok, #state{}}.

header(_Header, #state{} = State, _Context) ->
    {ok, <<>>, State}.

row(Row, #state{} = State, _Context) when is_integer(Row) ->
    {ok, <<>>, State#state{ rows = [ Row | State#state.rows ]}}.

footer(_Data, #state{} = State, Context) ->
    Data = lists:map(
        fun(Id) ->
            m_rsc:get(Id, Context)
        end,
        State#state.rows),
    {ok, erlang:term_to_binary(Data)}.
