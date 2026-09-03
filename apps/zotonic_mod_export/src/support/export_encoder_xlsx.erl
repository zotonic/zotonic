%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2026 Marc Worrell <marc@worrell.nl>
%% @doc Format exports for Microsoft xlsx format
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

-module(export_encoder_xlsx).
-author("Marc Worrell <marc@worrell.nl>").

-define(MAX_CELL_STYLES, 64).

-record(state, {
    query_id :: m_rsc:resource_id() | undefined,
    props :: list(binary() | atom()) | undefined,
    rows = [] :: list(),
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

% For testing
-export([
    number_to_letter/1,
    zip/4,
    xlsx_styles/1,
    encode_cell/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

extension() ->
    [ <<"xlsx">> ].

mime() ->
    [ {<<"application">>, <<"vnd.openxmlformats-officedocument.spreadsheetml.sheet">>, []} ].

init(Options, Context) ->
    IsRaw = proplists:get_value(is_raw, Options, false),
    QueryId = proplists:get_value(id, Options, z_context:get(id, Context)),
    Props = case proplists:get_value(rsc_props, Options, z_context:get(rsc_props, Context)) of
        L when is_list(L) -> L;
        undefined -> export_value:rsc_props(QueryId, Context)
    end,
    PropExprs = export_value:prepare_rsc_props(Props, Context),
    {ok, #state{
        query_id = QueryId,
        props = PropExprs,
        is_raw=IsRaw
    }}.

header(undefined, #state{ props = Ps } = State, _Context) ->
    {ok, <<>>, State#state{ props = Ps }};
header(Row, State, _Context) ->
    {ok, <<>>, State#state{ props = Row }}.

row(Row, #state{ rows = Rows } = State, _Context) ->
    {ok, <<>>, State#state{ rows = [ Row | Rows ]}}.

footer(_Row, State, Context) ->
    zip(State#state.props, lists:reverse(State#state.rows), State#state.is_raw, Context).

zip(Keys, Rows, IsRaw, Context) ->
    Styles = xlsx_styles(Rows),
    StyleLookup = maps:get(lookup, Styles),
    Vars = [
        {encode_cell, fun(V, Ctx) -> encode_cell(V, IsRaw, StyleLookup, Ctx) end},
        {lookup_header, fun export_encoder:lookup_header/2},
        {lookup_value, fun export_encoder:lookup_value/2},
        {sheet_name, <<"Sheet1">>},
        {keys, Keys},
        {rows, Rows},
        {fill_count, maps:get(fill_count, Styles)},
        {fill_colors, maps:get(fill_colors, Styles)},
        {cell_xf_count, maps:get(cell_xf_count, Styles)},
        {cell_styles, maps:get(cell_styles, Styles)}
    ],
    Fs = lists:map(fun({Fn, Template}) ->
                     {Fn, file_render(Template, Vars, Context)}
                   end,
                   file_templates()),
    {ok, {_ZipFilename, ZipBin}} = zip:zip("", Fs, [memory]),
    {ok, ZipBin}.

file_render(Template, Vars, Context) ->
    {Bin, _Context} = z_template:render_to_iolist(Template, Vars, Context),
    iolist_to_binary(Bin).

file_templates() ->
    [
        {"_rels/.rels", <<"xlsx/_rels.xml.tpl">>},
        {"xl/_rels/workbook.xml.rels", <<"xlsx/workbook.xml.rels.tpl">>},
        {"xl/worksheets/sheet1.xml", <<"xlsx/sheet1.xml.tpl">>},
        {"xl/styles.xml", <<"xlsx/styles.xml.tpl">>},
        {"xl/workbook.xml", <<"xlsx/workbook.xml.tpl">>},
        {"[Content_Types].xml", <<"xlsx/content_types.xml.tpl">>}
    ].

%% @doc Build a bounded, deterministic style table for the styled cells.
xlsx_styles(Rows) ->
    StyleKeys = collect_style_keys(Rows),
    FillColors = unique_colors(StyleKeys),
    FillIds = maps:from_list([
        {Color, FillId}
        || {FillId, Color} <- numbered(FillColors, 2)
    ]),
    NumberedStyles = numbered(StyleKeys, 3),
    CellStyles = [
        cell_style_vars(StyleId, BaseStyle, maps:get(Color, FillIds))
        || {StyleId, {BaseStyle, Color}} <- NumberedStyles
    ],
    #{
        lookup => maps:from_list([
            {StyleKey, StyleId}
            || {StyleId, StyleKey} <- NumberedStyles
        ]),
        fill_count => 2 + length(FillColors),
        fill_colors => FillColors,
        cell_xf_count => 3 + length(CellStyles),
        cell_styles => CellStyles
    }.

collect_style_keys(Rows) ->
    {StyleKeys, _Seen} = lists:foldl(
        fun collect_row_style_keys/2,
        {[], #{}},
        Rows),
    lists:reverse(StyleKeys).

collect_row_style_keys(Row, Acc) when is_list(Row) ->
    lists:foldl(fun collect_cell_style_key/2, Acc, Row);
collect_row_style_keys(_Row, Acc) ->
    Acc.

collect_cell_style_key(Cell, {StyleKeys, Seen} = Acc) ->
    case cell_style_key(Cell) of
        undefined ->
            Acc;
        _StyleKey when map_size(Seen) >= ?MAX_CELL_STYLES ->
            {StyleKeys, Seen};
        StyleKey ->
            case maps:is_key(StyleKey, Seen) of
                true -> Acc;
                false -> {[StyleKey | StyleKeys], Seen#{ StyleKey => true }}
            end
    end.

cell_style_key(Cell) ->
    Options = export_encoder:cell_options(Cell),
    case normalize_background_color(maps:get(background_color, Options, undefined)) of
        undefined -> undefined;
        Color -> {base_style(export_encoder:cell_value(Cell)), Color}
    end.

normalize_background_color(<<$#, Hex:6/binary>>) ->
    normalize_background_color(Hex);
normalize_background_color(Hex) when is_binary(Hex), byte_size(Hex) =:= 6 ->
    case is_hex(Hex) of
        true -> <<"FF", (z_string:to_upper(Hex))/binary>>;
        false -> undefined
    end;
normalize_background_color(_Color) ->
    undefined.

is_hex(<<>>) ->
    true;
is_hex(<<C, Rest/binary>>) when
        C >= $0, C =< $9;
        C >= $a, C =< $f;
        C >= $A, C =< $F ->
    is_hex(Rest);
is_hex(_Hex) ->
    false.

base_style(?ST_JUTTEMIS) ->
    0;
base_style({{Y, M, D}, {H, I, S}}) when
        is_integer(Y), is_integer(M), is_integer(D),
        is_integer(H), is_integer(I), is_integer(S) ->
    2;
base_style({Y, M, D}) when is_integer(Y), is_integer(M), is_integer(D) ->
    2;
base_style(_Value) ->
    0.

unique_colors(StyleKeys) ->
    {Colors, _Seen} = lists:foldl(
        fun({_BaseStyle, Color}, {Acc, Seen}) ->
            case maps:is_key(Color, Seen) of
                true -> {Acc, Seen};
                false -> {[Color | Acc], Seen#{ Color => true }}
            end
        end,
        {[], #{}},
        StyleKeys),
    lists:reverse(Colors).

numbered(Values, First) ->
    {Numbered, _Next} = lists:mapfoldl(
        fun(Value, N) -> {{N, Value}, N + 1} end,
        First,
        Values),
    Numbered.

cell_style_vars(StyleId, 2, FillId) ->
    #{
        style_id => StyleId,
        fill_id => FillId,
        font_id => 1,
        num_fmt_id => 164,
        apply_number_format => true
    };
cell_style_vars(StyleId, _BaseStyle, FillId) ->
    #{
        style_id => StyleId,
        fill_id => FillId,
        font_id => 0,
        num_fmt_id => 0,
        apply_number_format => false
    }.

encode_cell([Row, Col, Cell], IsRaw, StyleLookup, Context) ->
    Value = export_encoder:cell_value(Cell),
    StyleId = case cell_style_key(Cell) of
        undefined -> base_style(Value);
        StyleKey -> maps:get(StyleKey, StyleLookup, base_style(Value))
    end,
    encode_cell_value(Row, Col, Value, IsRaw, StyleId, Context).

encode_cell_value(Row, Col, V, _IsRaw, StyleId, _Context) when is_integer(V); is_float(V) ->
    iolist_to_binary([
        <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), style_attribute(StyleId), <<">">>,
            <<"<v>">>, z_convert:to_binary(V), <<"</v></c>">>
        ]);
encode_cell_value(Row, Col, ?ST_JUTTEMIS, IsRaw, StyleId, Context) ->
    encode_cell_value(Row, Col, <<>>, IsRaw, StyleId, Context);
encode_cell_value(Row, Col, {{Y,M,D},{H,I,S}} = Date, _IsRaw, StyleId, _Context) when
        is_integer(Y), is_integer(M), is_integer(D),
        is_integer(H), is_integer(I), is_integer(S) ->
    try
        Secs = z_datetime:datetime_to_timestamp(Date) + 2209161600,
        iolist_to_binary([
            <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), style_attribute(StyleId), <<">">>,
                <<"<v>">>, z_convert:to_binary(Secs / 86400), <<"</v></c>">>
            ])
    catch
        error:_if_clause ->
            ?LOG_WARNING(#{
                text => <<"Illegal date in xlsx export">>,
                in => zotonic_mod_export,
                result => error,
                reason => illegal_date,
                date => Date
            }),
            encode_inlinestr(Row, Col, <<>>, StyleId)
    end;
encode_cell_value(Row, Col, {Y,M,D} = Date, _IsRaw, StyleId, _Context) when
        is_integer(Y), is_integer(M), is_integer(D) ->
    try
        Secs = z_datetime:datetime_to_timestamp({Date, {0,0,0}}) + 2209161600,
        iolist_to_binary([
            <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row), style_attribute(StyleId), <<">">>,
                <<"<v>">>, z_convert:to_binary(Secs / 86400), <<"</v></c>">>
            ])
    catch
        error:_if_clause ->
            ?LOG_WARNING(#{
                text => <<"Illegal date in xlsx export">>,
                in => zotonic_mod_export,
                result => error,
                reason => illegal_date,
                date => Date
            }),
            encode_inlinestr(Row, Col, <<>>, StyleId)
    end;
encode_cell_value(Row, Col, V, false, StyleId, _Context) ->
    B = z_xml:escape(
            z_html:unescape(
                z_html:strip(z_convert:to_binary(V)))),
    encode_inlinestr(Row, Col, B, StyleId);
encode_cell_value(Row, Col, V, true, StyleId, _Context) ->
    B = z_xml:escape(z_convert:to_binary(V)),
    encode_inlinestr(Row, Col, B, StyleId).

encode_inlinestr(Row, Col, B, StyleId) ->
    iolist_to_binary([
        <<"<c r=\"">>, number_to_letter(Col), integer_to_list(Row),
            style_attribute(StyleId), <<" t=\"inlineStr\">">>,
            <<"<is><t>">>, B, <<"</t></is></c>">>
        ]).

style_attribute(0) ->
    <<"\"">>;
style_attribute(StyleId) ->
    [<<"\" s=\"">>, integer_to_binary(StyleId), <<"\"">>].


number_to_letter(0) ->
    <<>>;
number_to_letter(N) when N > 0 ->
    number_to_letter(N, <<>>).

number_to_letter(0, Acc) ->
    Acc;
number_to_letter(N, Acc) ->
    C = ((N-1) rem 26) + $A,
    number_to_letter((N-1) div 26, <<C,Acc/binary>>).
