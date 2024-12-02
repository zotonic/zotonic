%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2024 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for JSON format
%% @end

%% Copyright 2016-2024 Marc Worrell
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

-module(export_encoder_json).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    id :: integer() | undefined,
    is_first_row = true
}).

-export([
    extension/0,
    mime/0,
    init/2,
    header/3,
    row/3,
    footer/3
]).

extension() ->
    [ <<"json">> ].

mime() ->
    [ {<<"application">>, <<"json">>, []} ].

init(Options, _Context) ->
    {ok, #state{
        id = proplists:get_value(id, Options),
        is_first_row = true
    }}.

header(_Header, #state{ id = _Id } = State, _Context) ->
    {ok, <<"[">>, State}.

row(Row, #state{ is_first_row = IsFirstRow } = State, Context) when is_integer(Row) ->
    case m_rsc_export:full(Row, Context) of
        {ok, JSON} ->
            Data = [
                case IsFirstRow of
                    true -> <<>>;
                    false -> $,
                end,
                jsxrecord:encode(JSON)
            ],
            {ok, Data, State#state{ is_first_row = false }};
        {error, _} ->
            {ok, <<>>, State}
    end;
row(_Row, #state{} = State, _Context) ->
    {ok, <<>>, State}.

footer(_Data, #state{}, _Context) ->
    {ok, <<"]">>}.
