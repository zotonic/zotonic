%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for CSV format

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

-module(export_encoder_csv).
-author("Marc Worrell <marc@worrell.nl>").

-record(state, {
    props :: list(atom()) | undefined,
    is_raw = false
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
    [ <<"csv">> ].

mime() ->
    [ <<"text/csv">> ].

init(Options, Context) ->
    IsRaw = proplists:get_value(is_raw, Options, false),
    case z_context:get(rsc_props, Context) of
        L when is_list(L) ->
            {ok, #state{ props=L, is_raw=IsRaw }};
        undefined ->
            {ok, #state{ props=undefined, is_raw=IsRaw }}
    end.

header(undefined, #state{props=undefined} = State, Context) ->
    Ps = mod_export:rsc_props(Context) -- [blocks],
    Data = flatten_row(Ps, false, Context),
    {ok, Data, State#state{props=Ps}};
header(undefined, #state{props=Ps} = State, Context) ->
    Hs = [ export_encoder:lookup_header(P, Context) || P <- Ps ],
    Data = flatten_row(Hs, false, Context),
    {ok, Data, State#state{props=Ps}};
header(Row, State, Context) when is_list(Row) ->
    Hs = [ export_encoder:lookup_header(P, Context) || P <- Row ],
    Data = flatten_row(Hs, false, Context),
    State1 = case State#state.props of
                undefined -> State#state{props=Row};
                _ -> State
             end,
    {ok, Data, State1};
header(Row, State, Context) ->
    Data = flatten_row(Row, false, Context),
    {ok, Data, State}.

row(Id, #state{props=Ps, is_raw=IsRaw} = State, Context) when is_integer(Id), is_list(Ps) ->
    Data = [ export_encoder:lookup_value([Id, P], Context) || P <- Ps ],
    Data1 = flatten_row(Data, IsRaw, Context),
    {ok, Data1, State};
row(Row, #state{is_raw=IsRaw} = State, Context) ->
    Data = flatten_row(Row, IsRaw, Context),
    {ok, Data, State}.

footer(Row, #state{is_raw=IsRaw}, Context) ->
    Data = flatten_row(Row, IsRaw, Context),
    {ok, Data}.


flatten_row(undefined, _IsRaw, _Context) ->
    <<>>;
flatten_row(Row, _IsRaw, _Context) when is_binary(Row) ->
    Row;
flatten_row(Row, IsRaw, Context) when is_list(Row) ->
    export_encode_csv:encode(Row, IsRaw, Context);
flatten_row(Value, IsRaw, Context) ->
    export_encode_csv:encode([Value], IsRaw, Context).
