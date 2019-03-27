%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for ubf format

%% Copyright 2019 Marc Worrell
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

-module(export_encoder_ubf).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    id
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
    [ <<"ubf">> ].

mime() ->
    [ {<<"text">>, <<"x-ubf">>, []} ].

init(Options, _Context) ->
    {ok, #state{
        id = proplists:get_value(id, Options)
    }}.

header(_Header, #state{ id = _Id } = State, _Context) ->
    {ok, <<"#">>, State}.

row(Row, #state{} = State, Context) when is_integer(Row) ->
    Rsc = m_rsc:get(Row, Context),
    {ok, Enc} = z_ubf:encode(Rsc),
    Data = [
        ${, integer_to_binary(Row), $,, Enc, $}, $&
    ],
    {ok, Data, State};
row(_Row, #state{} = State, _Context) ->
    {ok, <<>>, State}.

footer(_Data, #state{}, _Context) ->
    {ok, <<"$">>}.
