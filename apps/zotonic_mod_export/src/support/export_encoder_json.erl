%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2026 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for JSON format
%% @end

%% Copyright 2016-2026 Marc Worrell
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
    headers :: [binary()] | undefined,
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

-include_lib("zotonic_core/include/zotonic.hrl").

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
    PropExprs = case Props of
        undefined -> undefined;
        _ -> export_value:prepare_rsc_props(Props, Context)
    end,
    {ok, #state{
        id = Id,
        props = PropExprs,
        headers = undefined,
        is_first_row = true
    }}.

header(Header, #state{} = State, Context) when is_list(Header) ->
    Headers = unique_headers(Header, Context),
    {ok, <<"[">>, State#state{headers = Headers}};
header(_Header, #state{} = State, _Context) ->
    {ok, <<"[">>, State}.

row(Row, #state{ props = Props } = State, Context) when is_integer(Row) ->
    case row_value(Row, Props, Context) of
        {ok, JSON} ->
            row(JSON, State, Context);
        {error, _} ->
            {ok, <<>>, State}
    end;
row(Row, #state{ headers = Headers } = State, Context)
        when is_list(Row), is_list(Headers) ->
    row(row_map(Headers, Row, Context), State, Context);
row(Row, #state{ is_first_row = IsFirstRow } = State, _Context) when is_map(Row) ->
    Data = [
        case IsFirstRow of
            true -> <<>>;
            false -> $,
        end,
        z_json:encode(maps:map(
            fun(_Key, Value) -> export_encoder:cell_value(Value) end,
            Row))
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

unique_headers(Headers, Context) ->
    {Unique, _Used} = lists:mapfoldl(
        fun(Header, Used) ->
            Base = header_name(Header, map_size(Used) + 1, Context),
            unique_header(Base, Used, 1)
        end,
        #{},
        Headers),
    Unique.

header_name(Header, N, Context) ->
    case z_string:trim(z_convert:to_binary(export_encoder:lookup_header(Header, Context))) of
        <<>> -> <<"column_", (integer_to_binary(N))/binary>>;
        Name -> Name
    end.

unique_header(Base, Used, N) ->
    Candidate = case N of
        1 -> Base;
        _ -> <<Base/binary, "_", (integer_to_binary(N))/binary>>
    end,
    case maps:is_key(Candidate, Used) of
        true -> unique_header(Base, Used, N + 1);
        false -> {Candidate, Used#{ Candidate => true }}
    end.

row_map(Headers, Values, Context) ->
    Used = maps:from_list([ {Header, true} || Header <- Headers ]),
    row_map(Headers, Values, length(Headers) + 1, Used, #{}, Context).

row_map([Header | Headers], [Value | Values], N, Used, Acc, Context) ->
    row_map(Headers, Values, N, Used, Acc#{ Header => json_value(Value, Context) }, Context);
row_map([Header | Headers], [], N, Used, Acc, Context) ->
    row_map(Headers, [], N, Used, Acc#{ Header => undefined }, Context);
row_map([], [Value | Values], N, Used, Acc, Context) ->
    Base = <<"column_", (integer_to_binary(N))/binary>>,
    {Header, Used1} = unique_header(Base, Used, 1),
    row_map([], Values, N + 1, Used1, Acc#{ Header => json_value(Value, Context) }, Context);
row_map([], [], _N, _Used, Acc, _Context) ->
    Acc.

json_value(Value, Context) ->
    json_value_1(export_encoder:cell_value(Value), Context).

json_value_1(#trans{} = Trans, Context) ->
    z_trans:lookup_fallback(Trans, Context);
json_value_1(?ST_JUTTEMIS, _Context) ->
    undefined;
json_value_1({{9999, _M, _D}, {_H, _I, _S}}, _Context) ->
    undefined;
json_value_1({Y, M, D} = Date, Context)
        when is_integer(Y), is_integer(M), is_integer(D) ->
    z_datetime:format_utc({Date, {0, 0, 0}}, "Y-m-d", Context);
json_value_1({{Y, M, D}, {H, I, S}} = Date, Context)
        when is_integer(Y), is_integer(M), is_integer(D),
             is_integer(H), is_integer(I), is_integer(S) ->
    z_datetime:format_utc(Date, "c", Context);
json_value_1(Value, _Context) ->
    Value.
