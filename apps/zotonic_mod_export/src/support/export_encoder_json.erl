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
    props :: list() | undefined,
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

init(Options, Context) ->
    Id = proplists:get_value(id, Options),
    Props = case proplists:get_value(rsc_props, Options, z_context:get(rsc_props, Context)) of
        L when is_list(L) -> L;
        undefined -> export_value:maybe_rsc_props(Id, Context)
    end,
    PropExprs = export_value:prepare_rsc_props(Props, Context),
    {ok, #state{
        id = Id,
        props = PropExprs,
        is_first_row = true
    }}.

header(_Header, #state{} = State, _Context) ->
    {ok, <<"[">>, State}.

row(Row, #state{ props = Props } = State, Context) when is_integer(Row) ->
    case row_value(Row, Props, Context) of
        {ok, JSON} ->
            row(JSON, State, Context);
        {error, _} ->
            {ok, <<>>, State}
    end;
row(Row, #state{ is_first_row = IsFirstRow } = State, _Context) when is_map(Row) ->
    Data = [
        case IsFirstRow of
            true -> <<>>;
            false -> $,
        end,
        jsxrecord:encode(Row)
    ],
    {ok, Data, State#state{ is_first_row = false }};
row(_Row, #state{} = State, _Context) ->
    {ok, <<>>, State}.

footer(_Data, #state{}, _Context) ->
    {ok, <<"]">>}.

row_value(Id, undefined, Context) ->
    m_rsc_export:full(Id, Context);
row_value(Id, Props, Context) ->
    {ok, lists:foldl(
        fun(P, Acc) ->
            Label = export_value:header(P),
            Value = export_value:value(Id, P, Context),
            Acc#{
                Label => Value
            }
        end,
        #{},
        Props)}.
