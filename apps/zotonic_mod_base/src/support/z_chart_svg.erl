%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Render bounded chart data as inline SVG and an optional data table.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_chart_svg).
-moduledoc(<<
    "Render a small, single-series chart as inline SVG.\n\n",
    "The renderer does not fetch data, load external resources, emit scripts, or\n",
    "accept arbitrary SVG/CSS. All text is escaped and all dimensions, colors, and\n",
    "data sizes are bounded before rendering.\n"
>>).

-export([render/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_POINTS, 1000).
-define(DEFAULT_MAX_PIE_VALUES, 50).
-define(MAX_LABEL_LENGTH, 512).
-define(MAX_CLASS_LENGTH, 256).
-define(MAX_PALETTE_LENGTH, 32).
-define(MAX_PALETTE_TEXT_LENGTH, 1024).
-define(MAX_DIMENSION, 4096).
-define(MIN_DIMENSION, 64).
-define(MAX_VALUE, 1000000000000000).

-define(DEFAULT_WIDTH, 400).
-define(DEFAULT_HEIGHT, 240).

-define(PIE_LABEL_FONT_SIZE, 11).
-define(PIE_LABEL_SHORT_FONT_SIZE, 14).
-define(PIE_LABEL_SHORT_MAX_LENGTH, 8).
-define(PIE_LABEL_SHORT_MAX_COUNT, 6).
-define(LINE_TICK_COUNT, 6).
-define(LINE_LABEL_CHARACTER_WIDTH, 6).
-define(LINE_LABEL_GAP, 12).
-define(MAX_DARKEN_AMOUNT, 0.35).

-define(PALETTE, [
    <<"#4477aa">>,
    <<"#ee6677">>,
    <<"#228833">>,
    <<"#ccbb44">>,
    <<"#aa3377">>,
    <<"#66ccee">>,
    <<"#ee8866">>,
    <<"#009988">>,
    <<"#332288">>,
    <<"#cc79a7">>,
    <<"#999933">>,
    <<"#117733">>,
    <<"#ddcc77">>,
    <<"#882255">>,
    <<"#44aa99">>,
    <<"#888888">>
]).

-type chart_type() :: pie | donut | horizontal_bar | vertical_bar | line.
-type data_point() :: {binary(), number(), binary() | undefined}.
-type colored_data_point() :: {binary(), number(), binary()}.

-spec render(Params, Context) -> {ok, binary()} | {error, badarg}
    when
        Params :: proplists:proplist(),
        Context :: z:context().
render(Params, Context) when is_list(Params) ->
    case normalize_type(proplists:get_value(type, Params, pie)) of
        {ok, Type} ->
            Data0 = normalize_params_data(Params, Context),
            LabelMinPercent = normalize_percentage(proplists:get_value(label_min_percent, Params)),
            MaxPieValues = normalize_max_pie_values(proplists:get_value(max_pie_values, Params)),
            {Data1, OtherValue} = compact_pie_data(Type, Data0, LabelMinPercent, MaxPieValues),
            Sort = proplists:get_value(sort, Params, false),
            Data2 = sort_data(Data1, Sort),
            Data = place_other(Data2, OtherValue, Sort, Context),
            Colors = chart_colors(Params, Context),
            ColoredData = colorize_data(Type, Data, Colors),
            Width = normalize_dimension(proplists:get_value(width, Params), ?DEFAULT_WIDTH),
            Height0 = normalize_dimension(proplists:get_value(height, Params), ?DEFAULT_HEIGHT),
            Height = chart_height(Type, Height0, length(Data)),
            Id = normalize_id(proplists:get_value(id, Params)),
            Title = normalize_title(proplists:get_value(title, Params), Context),
            Class = normalize_class(proplists:get_value(class, Params)),
            AriaDescribedBy = normalize_reference(proplists:get_value(aria_describedby, Params)),
            HideTable = z_convert:to_bool(proplists:get_value(hide_table, Params, false)),
            ShowLabels = z_convert:to_bool(proplists:get_value(show_labels, Params, default_show_labels(Type))),
            Svg = render_svg(
                Type,
                ColoredData,
                Width,
                Height,
                Id,
                Title,
                AriaDescribedBy,
                ShowLabels,
                Context),
            Table = case HideTable of
                true ->
                    <<>>;
                false ->
                    render_table(Type, ColoredData, Title, Params, Context)
            end,
            Legend = case should_render_legend(Type, HideTable, proplists:get_value(legend, Params, auto)) of
                true -> render_legend(ColoredData, Context);
                false -> <<>>
            end,
            FigureClass = case Class of
                <<>> -> <<"z-chart">>;
                _ -> <<"z-chart ", Class/binary>>
            end,
            Figure = z_tags:render_tag(
                <<"figure">>,
                [
                    {<<"id">>, Id},
                    {<<"class">>, FigureClass},
                    {<<"data-chart-type">>, atom_to_binary(Type, utf8)}
                ],
                [Svg, Legend, Table]),
            {ok, iolist_to_binary(Figure)};
        error ->
            {error, badarg}
    end;
render(_Params, _Context) ->
    {error, badarg}.

-spec normalize_type(term()) -> {ok, chart_type()} | error.
normalize_type(pie) -> {ok, pie};
normalize_type(<<"pie">>) -> {ok, pie};
normalize_type("pie") -> {ok, pie};
normalize_type(pie3d) -> {ok, pie};
normalize_type(<<"pie3d">>) -> {ok, pie};
normalize_type("pie3d") -> {ok, pie};
normalize_type(donut) -> {ok, donut};
normalize_type(<<"donut">>) -> {ok, donut};
normalize_type("donut") -> {ok, donut};
normalize_type(horizontal_bar) -> {ok, horizontal_bar};
normalize_type(<<"horizontal_bar">>) -> {ok, horizontal_bar};
normalize_type("horizontal_bar") -> {ok, horizontal_bar};
normalize_type(bar) -> {ok, horizontal_bar};
normalize_type(<<"bar">>) -> {ok, horizontal_bar};
normalize_type("bar") -> {ok, horizontal_bar};
normalize_type(vertical_bar) -> {ok, vertical_bar};
normalize_type(<<"vertical_bar">>) -> {ok, vertical_bar};
normalize_type("vertical_bar") -> {ok, vertical_bar};
normalize_type(line) -> {ok, line};
normalize_type(<<"line">>) -> {ok, line};
normalize_type("line") -> {ok, line};
normalize_type(_) -> error.

-spec normalize_params_data(proplists:proplist(), z:context()) -> [data_point()].
normalize_params_data(Params, Context) ->
    case proplists:get_value(data, Params) of
        undefined ->
            normalize_label_values(
                proplists:get_value(labels, Params, []),
                proplists:get_value(values, Params, []),
                Context,
                0,
                []);
        Data ->
            normalize_data(Data, Context)
    end.

-spec normalize_data(term(), z:context()) -> [data_point()].
normalize_data(Data, Context) when is_list(Data) ->
    normalize_data(Data, Context, 0, []);
normalize_data(Data, Context) when is_map(Data) ->
    normalize_map_data(maps:iterator(Data), Context, 0, []);
normalize_data(_Data, _Context) ->
    [].

normalize_data(_Data, _Context, ?MAX_POINTS, Acc) ->
    lists:reverse(Acc);
normalize_data([Row | Rest], Context, Count, Acc) ->
    case normalize_row(Row, Context) of
        {ok, Point} ->
            normalize_data(Rest, Context, Count + 1, [Point | Acc]);
        error ->
            normalize_data(Rest, Context, Count + 1, Acc)
    end;
normalize_data(_ImproperOrEmpty, _Context, _Count, Acc) ->
    lists:reverse(Acc).

normalize_map_data(_Iterator, _Context, ?MAX_POINTS, Acc) ->
    lists:reverse(Acc);
normalize_map_data(Iterator, Context, Count, Acc) ->
    case maps:next(Iterator) of
        {Label, Value, NextIterator} ->
            case normalize_row_1(Label, Value, Context) of
                {ok, Point} ->
                    normalize_map_data(NextIterator, Context, Count + 1, [Point | Acc]);
                error ->
                    normalize_map_data(NextIterator, Context, Count + 1, Acc)
            end;
        none ->
            lists:reverse(Acc)
    end.

normalize_label_values(_Labels, _Values, _Context, ?MAX_POINTS, Acc) ->
    lists:reverse(Acc);
normalize_label_values([Label | Labels], [Value | Values], Context, Count, Acc) ->
    case normalize_row_1(Label, Value, Context) of
        {ok, Point} ->
            normalize_label_values(Labels, Values, Context, Count + 1, [Point | Acc]);
        error ->
            normalize_label_values(Labels, Values, Context, Count + 1, Acc)
    end;
normalize_label_values(_Labels, _Values, _Context, _Count, Acc) ->
    lists:reverse(Acc).

normalize_row({Label, Value}, Context) ->
    normalize_row_1(Label, Value, Context);
normalize_row([Label, Value], Context) ->
    normalize_row_1(Label, Value, Context);
normalize_row(#{label := Label, value := Value} = Row, Context) ->
    normalize_row_1(Label, Value, row_color(Row), Context);
normalize_row(#{<<"label">> := Label, <<"value">> := Value} = Row, Context) ->
    normalize_row_1(Label, Value, row_color(Row), Context);
normalize_row(_Row, _Context) ->
    error.

normalize_row_1(Label, Value, Context) ->
    normalize_row_1(Label, Value, undefined, Context).

normalize_row_1(Label, Value, Color, Context) ->
    case normalize_number(Value) of
        {ok, Number} ->
            {ok, {
                normalize_text(Label, ?MAX_LABEL_LENGTH, Context),
                Number,
                normalize_row_color(Color)
            }};
        error ->
            error
    end.

row_color(Row) ->
    case maps:find(<<"color">>, Row) of
        {ok, Color} -> Color;
        error -> maps:get(color, Row, undefined)
    end.

normalize_row_color(undefined) ->
    undefined;
normalize_row_color(Color) ->
    case normalize_color(Color) of
        {ok, Valid} -> Valid;
        error -> undefined
    end.

normalize_number(Value) when is_integer(Value),
        Value >= -?MAX_VALUE,
        Value =< ?MAX_VALUE ->
    {ok, Value};
normalize_number(Value) when is_float(Value),
        Value =:= Value,
        Value >= -?MAX_VALUE,
        Value =< ?MAX_VALUE ->
    {ok, Value};
normalize_number(Value) when is_binary(Value), byte_size(Value) =< 64 ->
    try
        normalize_number(binary_to_integer(Value))
    catch
        error:badarg ->
            try
                normalize_number(binary_to_float(Value))
            catch
                error:badarg -> error
            end
    end;
normalize_number(_Value) ->
    error.

normalize_percentage(undefined) ->
    0;
normalize_percentage(Value) ->
    case normalize_number(Value) of
        {ok, Number} -> erlang:max(0, erlang:min(100, Number));
        error -> 0
    end.

normalize_max_pie_values(undefined) ->
    ?DEFAULT_MAX_PIE_VALUES;
normalize_max_pie_values(Value) when is_integer(Value) ->
    clamp(Value, 1, ?MAX_POINTS);
normalize_max_pie_values(Value) when is_binary(Value); is_list(Value) ->
    case z_convert:to_integer(Value) of
        undefined -> ?DEFAULT_MAX_PIE_VALUES;
        Integer -> clamp(Integer, 1, ?MAX_POINTS)
    end;
normalize_max_pie_values(_Value) ->
    ?DEFAULT_MAX_PIE_VALUES.

default_show_labels(pie) -> true;
default_show_labels(donut) -> true;
default_show_labels(_Type) -> false.

compact_pie_data(pie, Data, MinPercent, MaxValues) ->
    compact_pie_data_1(Data, MinPercent, MaxValues);
compact_pie_data(donut, Data, MinPercent, MaxValues) ->
    compact_pie_data_1(Data, MinPercent, MaxValues);
compact_pie_data(_Type, Data, _MinPercent, _MaxValues) ->
    {Data, undefined}.

compact_pie_data_1(Data, MinPercent, MaxValues) ->
    PositiveData = [Point || {_Label, Value, _Color} = Point <- Data, Value > 0],
    Total = lists:sum([Value || {_Label, Value, _Color} <- PositiveData]),
    {AboveThreshold, BelowThreshold} = lists:partition(
        fun({_Label, Value, _Color}) ->
            Value * 100 / Total >= MinPercent
        end,
        PositiveData),
    {Kept, BeyondLimit} = case length(AboveThreshold) > MaxValues of
        true ->
            lists:split(MaxValues, sort_data(AboveThreshold, value, descending));
        false ->
            {AboveThreshold, []}
    end,
    {Kept, other_value(BelowThreshold ++ BeyondLimit)}.

other_value([]) ->
    undefined;
other_value(OtherData) ->
    lists:sum([Value || {_Label, Value, _Color} <- OtherData]).

place_other(Data, undefined, _Sort, _Context) ->
    Data;
place_other(Data, OtherValue, Sort, Context) ->
    Other = {?__("Other", Context), OtherValue, undefined},
    case normalize_sort(Sort) of
        {value, ascending} -> [Other | Data];
        _ -> Data ++ [Other]
    end.

sort_data(Data, Sort) ->
    case normalize_sort(Sort) of
        undefined -> Data;
        {Field, Direction} -> sort_data(Data, Field, Direction)
    end.

normalize_sort(true) -> {value, descending};
normalize_sort(value) -> {value, ascending};
normalize_sort(label) -> {label, ascending};
normalize_sort('-value') -> {value, descending};
normalize_sort('+value') -> {value, ascending};
normalize_sort('-label') -> {label, descending};
normalize_sort('+label') -> {label, ascending};
normalize_sort(<<"-value">>) -> {value, descending};
normalize_sort(<<"+value">>) -> {value, ascending};
normalize_sort(<<"value">>) -> {value, ascending};
normalize_sort(<<"-label">>) -> {label, descending};
normalize_sort(<<"+label">>) -> {label, ascending};
normalize_sort(<<"label">>) -> {label, ascending};
normalize_sort("-value") -> {value, descending};
normalize_sort("+value") -> {value, ascending};
normalize_sort("value") -> {value, ascending};
normalize_sort("-label") -> {label, descending};
normalize_sort("+label") -> {label, ascending};
normalize_sort("label") -> {label, ascending};
normalize_sort(_Sort) -> undefined.

sort_data(Data, Field, Direction) ->
    Indexed = lists:zip(lists:seq(1, length(Data)), Data),
    Sorted = lists:sort(
        fun({IndexA, PointA}, {IndexB, PointB}) ->
            KeyA = sort_key(Field, PointA),
            KeyB = sort_key(Field, PointB),
            case KeyA == KeyB of
                true -> IndexA < IndexB;
                false -> sort_before(Direction, KeyA, KeyB)
            end
        end,
        Indexed),
    [Point || {_Index, Point} <- Sorted].

sort_key(value, {_Label, Value, _Color}) -> Value;
sort_key(label, {Label, _Value, _Color}) -> Label.

sort_before(ascending, A, B) -> A < B;
sort_before(descending, A, B) -> A > B.

normalize_dimension(undefined, Default) ->
    Default;
normalize_dimension(Value, _Default) when is_integer(Value) ->
    clamp(Value, ?MIN_DIMENSION, ?MAX_DIMENSION);
normalize_dimension(Value, Default) when is_binary(Value), byte_size(Value) =< 16 ->
    case z_convert:to_integer(Value) of
        undefined -> Default;
        Integer -> clamp(Integer, ?MIN_DIMENSION, ?MAX_DIMENSION)
    end;
normalize_dimension(_Value, Default) ->
    Default.

clamp(Value, Minimum, _Maximum) when Value < Minimum -> Minimum;
clamp(Value, _Minimum, Maximum) when Value > Maximum -> Maximum;
clamp(Value, _Minimum, _Maximum) -> Value.

chart_height(horizontal_bar, Height, Count) ->
    erlang:min(?MAX_DIMENSION, erlang:max(Height, Count * 24 + 20));
chart_height(_Type, Height, _Count) ->
    Height.

normalize_id(undefined) ->
    <<"chart-", (z_ids:identifier(10))/binary>>;
normalize_id(Value) ->
    Id = normalize_text(Value, 80, undefined),
    case re:run(Id, <<"^[A-Za-z][A-Za-z0-9_-]{0,79}$">>, [{capture, none}]) of
        match -> Id;
        nomatch -> <<"chart-", (z_ids:identifier(10))/binary>>
    end.

normalize_reference(undefined) ->
    undefined;
normalize_reference(Value) ->
    Id = normalize_text(Value, 80, undefined),
    case re:run(Id, <<"^[A-Za-z][A-Za-z0-9_-]{0,79}$">>, [{capture, none}]) of
        match -> Id;
        nomatch -> undefined
    end.

normalize_title(undefined, Context) ->
    ?__("Chart", Context);
normalize_title(<<>>, Context) ->
    ?__("Chart", Context);
normalize_title(Title, Context) ->
    normalize_text(Title, ?MAX_LABEL_LENGTH, Context).

normalize_class(undefined) ->
    <<>>;
normalize_class(Value) ->
    Class0 = normalize_text(Value, ?MAX_CLASS_LENGTH, undefined),
    re:replace(Class0, <<"[^A-Za-z0-9 _-]">>, <<>>, [global, {return, binary}]).

chart_colors(Params, Context) ->
    case normalize_palette(proplists:get_value(color, Params)) of
        [Color | _] ->
            [Color];
        [] ->
            first_palette([
                proplists:get_value(palette, Params),
                proplists:get_value(colors, Params)
            ], Context)
    end.

first_palette([], Context) ->
    case normalize_palette(configured_palette(Context)) of
        [] -> ?PALETTE;
        Colors -> Colors
    end;
first_palette([Palette | Rest], Context) ->
    case normalize_palette(Palette) of
        [] -> first_palette(Rest, Context);
        Colors -> Colors
    end.

configured_palette(Context) ->
    try m_config:get_value(site, chart_palette, Context)
    catch
        exit:{noproc, _} -> undefined
    end.

normalize_palette(Palette) when is_binary(Palette) ->
    Text = z_string:truncatechars(Palette, ?MAX_PALETTE_TEXT_LENGTH),
    normalize_colors(re:split(Text, <<"[\\s,;]+">>, [{return, binary}, trim]), 0, []);
normalize_palette([C | _] = Palette) when is_integer(C) ->
    Prefix = take_list(Palette, ?MAX_PALETTE_TEXT_LENGTH, []),
    try unicode:characters_to_binary(Prefix) of
        Text when is_binary(Text) -> normalize_palette(Text);
        _ -> []
    catch
        _:_ -> []
    end;
normalize_palette(Palette) when is_list(Palette) ->
    normalize_colors(Palette, 0, []);
normalize_palette(_Palette) ->
    [].

normalize_colors(_Colors, ?MAX_PALETTE_LENGTH, Acc) ->
    lists:reverse(Acc);
normalize_colors([Color | Rest], Count, Acc) ->
    case normalize_color(Color) of
        {ok, Valid} -> normalize_colors(Rest, Count + 1, [Valid | Acc]);
        error -> normalize_colors(Rest, Count + 1, Acc)
    end;
normalize_colors(_ImproperOrEmpty, _Count, Acc) ->
    lists:reverse(Acc).

-spec colorize_data(chart_type(), [data_point()], [binary()]) -> [colored_data_point()].
colorize_data(line, [], _Colors) ->
    [];
colorize_data(line, [{_Label, _Value, CustomColor} | _] = Data, Colors) ->
    LineColor = case CustomColor of
        undefined -> color(0, 1, Colors);
        _ -> CustomColor
    end,
    [{Label, Value, LineColor} || {Label, Value, _Color} <- Data];
colorize_data(Type, Data, Colors) ->
    colorize_data(Type, Data, Colors, length(Data), 0, []).

colorize_data(_Type, [], _Colors, _ColorCount, _Index, Acc) ->
    lists:reverse(Acc);
colorize_data(Type, [{Label, Value, CustomColor} | Rest], Colors, ColorCount, Index, Acc) ->
    PointColor = case CustomColor of
        undefined -> color(Index, ColorCount, Colors);
        _ -> CustomColor
    end,
    colorize_data(Type, Rest, Colors, ColorCount, Index + 1, [{Label, Value, PointColor} | Acc]).

normalize_color(Color) when is_binary(Color), byte_size(Color) =< 7 ->
    Color1 = case Color of
        <<"#", Hex/binary>> -> Hex;
        Hex -> Hex
    end,
    case re:run(Color1, <<"^[0-9A-Fa-f]{6}$">>, [{capture, none}]) of
        match -> {ok, <<"#", Color1/binary>>};
        nomatch -> error
    end;
normalize_color(_Color) ->
    error.

normalize_text(#trans{} = Trans, MaxLength, Context) ->
    truncate_text(z_trans:lookup_fallback(Trans, Context), MaxLength);
normalize_text(Value, MaxLength, _Context) when is_binary(Value) ->
    truncate_text(Value, MaxLength);
normalize_text(Value, MaxLength, _Context) when is_list(Value) ->
    Prefix = take_list(Value, MaxLength * 4, []),
    Text = try unicode:characters_to_binary(Prefix, utf8) of
        Binary when is_binary(Binary) -> Binary;
        _ -> <<>>
    catch
        _:_ -> <<>>
    end,
    truncate_text(Text, MaxLength);
normalize_text(Value, MaxLength, _Context) when is_atom(Value) ->
    truncate_text(atom_to_binary(Value, utf8), MaxLength);
normalize_text(Value, MaxLength, _Context) when is_integer(Value) ->
    truncate_text(integer_to_binary(Value), MaxLength);
normalize_text(Value, MaxLength, _Context) when is_float(Value) ->
    truncate_text(format_number(Value), MaxLength);
normalize_text(_Value, _MaxLength, _Context) ->
    <<>>.

take_list(_List, 0, Acc) ->
    lists:reverse(Acc);
take_list([C | Rest], Count, Acc) when is_integer(C) ->
    take_list(Rest, Count - 1, [C | Acc]);
take_list(_ImproperOrEmpty, _Count, Acc) ->
    lists:reverse(Acc).

truncate_text(Text, MaxLength) ->
    z_string:truncatechars(Text, MaxLength).

render_svg(
        Type,
        Data,
        Width,
        Height,
        Id,
        Title,
        AriaDescribedBy,
        ShowLabels,
        Context) ->
    TitleId = <<Id/binary, "-title">>,
    DescId = <<Id/binary, "-description">>,
    Description = iolist_to_binary([
        ?__("Data points", Context), <<": ">>, integer_to_binary(length(Data))
    ]),
    DescribedBy = case AriaDescribedBy of
        undefined -> DescId;
        Reference -> Reference
    end,
    Content = [
        z_tags:render_tag(<<"title">>, [{<<"id">>, TitleId}], escape(Title)),
        z_tags:render_tag(<<"desc">>, [{<<"id">>, DescId}], escape(Description)),
        render_plot(Type, Data, Width, Height, ShowLabels, Context)
    ],
    z_tags:render_tag(
        <<"svg">>,
        [
            {<<"class">>, <<"z-chart-svg">>},
            {<<"xmlns">>, <<"http://www.w3.org/2000/svg">>},
            {<<"viewBox">>, iolist_to_binary([<<"0 0 ">>, integer_to_binary(Width), $\s, integer_to_binary(Height)])},
            {<<"width">>, <<"100%">>},
            {<<"height">>, Height},
            {<<"preserveAspectRatio">>, <<"xMidYMid meet">>},
            {<<"role">>, <<"img">>},
            {<<"aria-labelledby">>, TitleId},
            {<<"aria-describedby">>, DescribedBy},
            {<<"font-family">>, <<"sans-serif">>},
            {<<"font-size">>, 12}
        ],
        Content).

render_plot(pie, Data, Width, Height, ShowLabels, Context) ->
    render_pie(Data, Width, Height, false, ShowLabels, Context);
render_plot(donut, Data, Width, Height, ShowLabels, Context) ->
    render_pie(Data, Width, Height, true, ShowLabels, Context);
render_plot(horizontal_bar, Data, Width, Height, _ShowLabels, _Context) ->
    render_horizontal_bar(Data, Width, Height);
render_plot(vertical_bar, Data, Width, Height, _ShowLabels, _Context) ->
    render_vertical_bar(Data, Width, Height);
render_plot(line, Data, Width, Height, _ShowLabels, _Context) ->
    render_line(Data, Width, Height).

render_pie(Data, Width, Height, IsDonut, ShowLabels, Context) ->
    case lists:sum([Value || {_Label, Value, _Color} <- Data]) of
        Total when Total > 0 ->
            CX = Width / 2,
            CY = Height / 2,
            {Radius, LabelSpace} = pie_geometry(ShowLabels, Width, Height),
            Slices = pie_slices(Data, Total),
            [
                render_pie_slices(Slices, Total, CX, CY, Radius, IsDonut),
                render_pie_labels(
                    ShowLabels,
                    Slices,
                    Total,
                    CX,
                    CY,
                    Radius,
                    LabelSpace,
                    Width,
                    Height)
            ];
        _ ->
            render_no_data(Width, Height, Context)
    end.

pie_geometry(false, Width, Height) ->
    {erlang:max(1, erlang:min(Width, Height) / 2 - 8), 0};
pie_geometry(true, Width, Height) ->
    LabelSpace = erlang:min(140.0, erlang:max(20.0, Width * 0.24)),
    Radius = erlang:max(1, erlang:min((Width - 2 * LabelSpace) / 2 - 6, Height / 2 - 12)),
    {Radius, LabelSpace}.

pie_slices([{Label, Value, Color}], _Total) ->
    [#{
        label => Label,
        value => Value,
        color => Color,
        start => -math:pi() / 2,
        finish => 3 * math:pi() / 2,
        angle => 0,
        index => 1
    }];
pie_slices(Data, Total) ->
    {Slices, _Angle} = lists:mapfoldl(
        fun({{Label, Value, Color}, Index}, StartAngle) ->
            FinishAngle = StartAngle + 2 * math:pi() * Value / Total,
            Slice = #{
                label => Label,
                value => Value,
                color => Color,
                start => StartAngle,
                finish => FinishAngle,
                angle => (StartAngle + FinishAngle) / 2,
                index => Index
            },
            {Slice, FinishAngle}
        end,
        -math:pi() / 2,
        lists:zip(Data, lists:seq(1, length(Data)))),
    Slices.

render_pie_slices([#{label := Label, value := Value, color := Color}], _Total, CX, CY, Radius, IsDonut) ->
    render_full_pie(Label, Value, CX, CY, Radius, Color, IsDonut);
render_pie_slices(Slices, Total, CX, CY, Radius, IsDonut) ->
    [
        render_pie_segment(
            Label,
            Value,
            Total,
            Start,
            Finish,
            CX,
            CY,
            Radius,
            Color,
            IsDonut)
        || #{
            label := Label,
            value := Value,
            color := Color,
            start := Start,
            finish := Finish
        } <- Slices
    ].

render_pie_labels(false, _Slices, _Total, _CX, _CY, _Radius, _LabelSpace, _Width, _Height) ->
    <<>>;
render_pie_labels(true, Slices, _Total, CX, CY, Radius, LabelSpace, Width, Height) ->
    FontSize = pie_label_font_size(Slices),
    LineHeight = FontSize + 2,
    LabelGap = FontSize + 3,
    MinY = LineHeight,
    MaxY = Height - LineHeight,
    Candidates = [
        pie_label_candidate(Slice, CX, CY, Radius, MinY, MaxY)
        || Slice <- Slices
    ],
    Left = [Label || #{side := left} = Label <- Candidates],
    Right = [Label || #{side := right} = Label <- Candidates],
    MaxLabels = erlang:max(1, trunc((MaxY - MinY) / LabelGap) + 1),
    Positioned =
        position_pie_labels(limit_pie_labels(Left, MaxLabels), MinY, MaxY, LabelGap)
        ++ position_pie_labels(
            limit_pie_labels(Right, MaxLabels),
            MinY,
            MaxY,
            LabelGap),
    case Positioned of
        [] ->
            <<>>;
        _ ->
            z_tags:render_tag(
                <<"g">>,
                [{<<"class">>, <<"z-chart-segment-labels">>}],
                [
                    render_pie_label(Label, CX, CY, Radius, LabelSpace, Width, FontSize)
                    || Label <- Positioned
                ])
    end.

pie_label_font_size(Slices) when length(Slices) =< ?PIE_LABEL_SHORT_MAX_COUNT ->
    case lists:all(
        fun(#{label := Label}) ->
            z_string:len(Label) =< ?PIE_LABEL_SHORT_MAX_LENGTH
        end,
        Slices)
    of
        true -> ?PIE_LABEL_SHORT_FONT_SIZE;
        false -> ?PIE_LABEL_FONT_SIZE
    end;
pie_label_font_size(_Slices) ->
    ?PIE_LABEL_FONT_SIZE.

pie_label_candidate(#{angle := Angle} = Slice, CX, CY, Radius, MinY, MaxY) ->
    {_X, NaturalY} = polar(CX, CY, Radius + 8, Angle),
    Side = case math:cos(Angle) >= 0 of
        true -> right;
        false -> left
    end,
    Slice#{
        side => Side,
        natural_y => erlang:max(MinY, erlang:min(MaxY, NaturalY))
    }.

limit_pie_labels(Labels, MaxLabels) when length(Labels) =< MaxLabels ->
    Labels;
limit_pie_labels(Labels, MaxLabels) ->
    ByValue = lists:sort(
        fun(#{value := ValueA, index := IndexA}, #{value := ValueB, index := IndexB}) ->
            case ValueA == ValueB of
                true -> IndexA < IndexB;
                false -> ValueA > ValueB
            end
        end,
        Labels),
    lists:sublist(ByValue, MaxLabels).

position_pie_labels([], _MinY, _MaxY, _Gap) ->
    [];
position_pie_labels(Labels, MinY, MaxY, Gap) ->
    ByPosition = lists:sort(
        fun(#{natural_y := YA, index := IndexA}, #{natural_y := YB, index := IndexB}) ->
            {YA, IndexA} < {YB, IndexB}
        end,
        Labels),
    {PositionedDown, _LastY} = lists:mapfoldl(
        fun(#{natural_y := NaturalY} = Label, PreviousY) ->
            Y = erlang:max(NaturalY, PreviousY + Gap),
            {Label#{y => Y}, Y}
        end,
        MinY - Gap,
        ByPosition),
    position_pie_labels_up(lists:reverse(PositionedDown), MaxY + Gap, Gap, []).

position_pie_labels_up([], _NextY, _Gap, Acc) ->
    Acc;
position_pie_labels_up([#{y := Y} = Label | Rest], NextY, Gap, Acc) ->
    BoundedY = erlang:min(Y, NextY - Gap),
    position_pie_labels_up(Rest, BoundedY, Gap, [Label#{y => BoundedY} | Acc]).

render_pie_label(
        #{label := Label, angle := Angle, side := Side, y := Y},
        CX,
        CY,
        Radius,
        LabelSpace,
        Width,
        FontSize) ->
    {StartX, StartY} = polar(CX, CY, Radius, Angle),
    {OuterX, OuterY} = polar(CX, CY, Radius + 8, Angle),
    {TextX, LineX, TextAnchor} = case Side of
        left -> {LabelSpace - 8, LabelSpace - 4, <<"end">>};
        right -> {Width - LabelSpace + 8, Width - LabelSpace + 4, <<"start">>}
    end,
    Points = iolist_to_binary(lists:join($\s, [
        [coordinate(StartX), $,, coordinate(StartY)],
        [coordinate(OuterX), $,, coordinate(OuterY)],
        [coordinate(LineX), $,, coordinate(Y)]
    ])),
    LabelLength = erlang:max(4, erlang:min(24, trunc((LabelSpace - 12) / 6))),
    [
        z_tags:render_tag(
            <<"polyline">>,
            [
                {<<"class">>, <<"z-chart-segment-label-line">>},
                {<<"points">>, Points},
                {<<"fill">>, <<"none">>},
                {<<"stroke">>, <<"currentColor">>},
                {<<"stroke-width">>, 1},
                {<<"aria-hidden">>, <<"true">>}
            ],
            <<>>),
        z_tags:render_tag(
            <<"text">>,
            [
                {<<"class">>, <<"z-chart-segment-label">>},
                {<<"x">>, coordinate(TextX)},
                {<<"y">>, coordinate(Y)},
                {<<"text-anchor">>, TextAnchor},
                {<<"dominant-baseline">>, <<"middle">>},
                {<<"fill">>, <<"currentColor">>},
                {<<"font-size">>, FontSize}
            ],
            escape(short_label(Label, LabelLength)))
    ].

render_full_pie(Label, Value, CX, CY, Radius, Color, false) ->
    z_tags:render_tag(
        <<"circle">>,
        [
            {<<"cx">>, coordinate(CX)},
            {<<"cy">>, coordinate(CY)},
            {<<"r">>, coordinate(Radius)},
            {<<"fill">>, Color}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, Value))));
render_full_pie(Label, Value, CX, CY, Radius, Color, true) ->
    RingRadius = Radius * 0.72,
    z_tags:render_tag(
        <<"circle">>,
        [
            {<<"cx">>, coordinate(CX)},
            {<<"cy">>, coordinate(CY)},
            {<<"r">>, coordinate(RingRadius)},
            {<<"fill">>, <<"none">>},
            {<<"stroke">>, Color},
            {<<"stroke-width">>, coordinate(Radius * 0.5)}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, Value)))).

render_pie_segment(Label, Value, Total, Start, Finish, CX, CY, Radius, Color, false) ->
    {X1, Y1} = polar(CX, CY, Radius, Start),
    {X2, Y2} = polar(CX, CY, Radius, Finish),
    LargeArc = large_arc(Finish - Start),
    Path = iolist_to_binary([
        <<"M ">>, coordinate(CX), $\s, coordinate(CY),
        <<" L ">>, coordinate(X1), $\s, coordinate(Y1),
        <<" A ">>, coordinate(Radius), $\s, coordinate(Radius),
        <<" 0 ">>, integer_to_binary(LargeArc), <<" 1 ">>, coordinate(X2), $\s, coordinate(Y2),
        <<" Z">>
    ]),
    z_tags:render_tag(
        <<"path">>,
        [
            {<<"d">>, Path},
            {<<"fill">>, Color},
            {<<"stroke">>, <<"#ffffff">>},
            {<<"stroke-width">>, 1}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, Total))));
render_pie_segment(Label, Value, Total, Start, Finish, CX, CY, Radius, Color, true) ->
    RingRadius = Radius * 0.72,
    {X1, Y1} = polar(CX, CY, RingRadius, Start),
    {X2, Y2} = polar(CX, CY, RingRadius, Finish),
    LargeArc = large_arc(Finish - Start),
    Path = iolist_to_binary([
        <<"M ">>, coordinate(X1), $\s, coordinate(Y1),
        <<" A ">>, coordinate(RingRadius), $\s, coordinate(RingRadius),
        <<" 0 ">>, integer_to_binary(LargeArc), <<" 1 ">>, coordinate(X2), $\s, coordinate(Y2)
    ]),
    z_tags:render_tag(
        <<"path">>,
        [
            {<<"d">>, Path},
            {<<"fill">>, <<"none">>},
            {<<"stroke">>, Color},
            {<<"stroke-width">>, coordinate(Radius * 0.5)},
            {<<"stroke-linecap">>, <<"butt">>}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, Total)))).

large_arc(Delta) ->
    case Delta > math:pi() of
        true -> 1;
        false -> 0
    end.

polar(CX, CY, Radius, Angle) ->
    {CX + Radius * math:cos(Angle), CY + Radius * math:sin(Angle)}.

render_horizontal_bar([], Width, Height) ->
    render_no_data(Width, Height, undefined);
render_horizontal_bar(Data, Width, Height) ->
    Values = [Value || {_Label, Value, _Color} <- Data],
    {Minimum, Maximum} = value_range(Values),
    LabelWidth = erlang:min(160.0, Width * 0.35),
    PlotX = LabelWidth + 8,
    PlotWidth = erlang:max(1, Width - PlotX - 8),
    Span = nonzero_span(Maximum - Minimum),
    ZeroX = PlotX + (0 - Minimum) / Span * PlotWidth,
    RowHeight = (Height - 20) / length(Data),
    Axis = z_tags:render_tag(
        <<"line">>,
        [
            {<<"x1">>, coordinate(ZeroX)},
            {<<"y1">>, 6},
            {<<"x2">>, coordinate(ZeroX)},
            {<<"y2">>, Height - 6},
            {<<"stroke">>, <<"#777777">>},
            {<<"stroke-width">>, 1}
        ],
        <<>>),
    [Axis | render_horizontal_rows(Data, 0, RowHeight, PlotX, PlotWidth, ZeroX, Minimum, Span)].

render_horizontal_rows([], _Index, _RowHeight, _PlotX, _PlotWidth, _ZeroX, _Minimum, _Span) ->
    [];
render_horizontal_rows([{Label, Value, Color} | Rest], Index, RowHeight, PlotX, PlotWidth, ZeroX, Minimum, Span) ->
    ValueX = PlotX + (Value - Minimum) / Span * PlotWidth,
    X = erlang:min(ZeroX, ValueX),
    BarWidth = abs(ValueX - ZeroX),
    BarHeight = erlang:max(1, RowHeight * 0.62),
    Y = 10 + Index * RowHeight + (RowHeight - BarHeight) / 2,
    LabelY = 10 + Index * RowHeight + RowHeight / 2,
    LabelElement = z_tags:render_tag(
        <<"text">>,
        [
            {<<"x">>, 0},
            {<<"y">>, coordinate(LabelY)},
            {<<"dominant-baseline">>, <<"middle">>}
        ],
        escape(short_label(Label, 24))),
    Bar = z_tags:render_tag(
        <<"rect">>,
        [
            {<<"x">>, coordinate(X)},
            {<<"y">>, coordinate(Y)},
            {<<"width">>, coordinate(BarWidth)},
            {<<"height">>, coordinate(BarHeight)},
            {<<"fill">>, Color}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, undefined)))),
    [LabelElement, Bar | render_horizontal_rows(
        Rest, Index + 1, RowHeight, PlotX, PlotWidth, ZeroX, Minimum, Span)].

render_vertical_bar([], Width, Height) ->
    render_no_data(Width, Height, undefined);
render_vertical_bar(Data, Width, Height) ->
    Values = [Value || {_Label, Value, _Color} <- Data],
    {Minimum, Maximum} = value_range(Values),
    Left = 36,
    Top = 8,
    Bottom = Height - 28,
    PlotWidth = erlang:max(1, Width - Left - 8),
    PlotHeight = erlang:max(1, Bottom - Top),
    Span = nonzero_span(Maximum - Minimum),
    ZeroY = Bottom - (0 - Minimum) / Span * PlotHeight,
    SlotWidth = PlotWidth / length(Data),
    Axis = z_tags:render_tag(
        <<"line">>,
        [
            {<<"x1">>, Left},
            {<<"y1">>, coordinate(ZeroY)},
            {<<"x2">>, Width - 8},
            {<<"y2">>, coordinate(ZeroY)},
            {<<"stroke">>, <<"#777777">>},
            {<<"stroke-width">>, 1}
        ],
        <<>>),
    [Axis | render_vertical_rows(Data, 0, SlotWidth, Left, Bottom, PlotHeight, ZeroY, Minimum, Span)].

render_vertical_rows([], _Index, _SlotWidth, _Left, _Bottom, _PlotHeight, _ZeroY, _Minimum, _Span) ->
    [];
render_vertical_rows([{Label, Value, Color} | Rest], Index, SlotWidth, Left, Bottom, PlotHeight, ZeroY, Minimum, Span) ->
    ValueY = Bottom - (Value - Minimum) / Span * PlotHeight,
    Y = erlang:min(ZeroY, ValueY),
    BarHeight = abs(ValueY - ZeroY),
    BarWidth = erlang:max(1, SlotWidth * 0.7),
    X = Left + Index * SlotWidth + (SlotWidth - BarWidth) / 2,
    LabelX = Left + Index * SlotWidth + SlotWidth / 2,
    Bar = z_tags:render_tag(
        <<"rect">>,
        [
            {<<"x">>, coordinate(X)},
            {<<"y">>, coordinate(Y)},
            {<<"width">>, coordinate(BarWidth)},
            {<<"height">>, coordinate(BarHeight)},
            {<<"fill">>, Color}
        ],
        z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, undefined)))),
    LabelElement = z_tags:render_tag(
        <<"text">>,
        [
            {<<"x">>, coordinate(LabelX)},
            {<<"y">>, Bottom + 18},
            {<<"text-anchor">>, <<"middle">>}
        ],
        escape(short_label(Label, 12))),
    [Bar, LabelElement | render_vertical_rows(
        Rest, Index + 1, SlotWidth, Left, Bottom, PlotHeight, ZeroY, Minimum, Span)].

render_line([], Width, Height) ->
    render_no_data(Width, Height, undefined);
render_line([{_FirstLabel, _FirstValue, LineColor} | _] = Data, Width, Height) ->
    Values = [Value || {_Label, Value, _Color} <- Data],
    {Minimum, Maximum, TickStep, Ticks} = nice_value_scale(Values),
    Left = 44,
    Top = 10,
    Bottom = Height - 28,
    PlotWidth = erlang:max(1, Width - Left - 8),
    PlotHeight = erlang:max(1, Bottom - Top),
    Span = nonzero_span(Maximum - Minimum),
    Count = length(Data),
    Step = case Count of
        1 -> 0;
        _ -> PlotWidth / (Count - 1)
    end,
    Points = line_points(Data, 0, Step, Left, Bottom, PlotHeight, Minimum, Span, []),
    PolylinePoints = iolist_to_binary(lists:join($\s, [
        [coordinate(X), $,, coordinate(Y)] || {_Label, _Value, X, Y} <- Points
    ])),
    LabelPoints = line_label_points(Points),
    Grid = render_line_grid(
        Ticks,
        TickStep,
        Points,
        Left,
        Width - 8,
        Top,
        Bottom,
        PlotHeight,
        Minimum,
        Span),
    Axis = [
        z_tags:render_tag(<<"line">>, [
            {<<"x1">>, Left}, {<<"y1">>, Bottom},
            {<<"x2">>, Width - 8}, {<<"y2">>, Bottom},
            {<<"stroke">>, <<"#777777">>}, {<<"stroke-width">>, 1}
        ], <<>>),
        z_tags:render_tag(<<"line">>, [
            {<<"x1">>, Left}, {<<"y1">>, Top},
            {<<"x2">>, Left}, {<<"y2">>, Bottom},
            {<<"stroke">>, <<"#777777">>}, {<<"stroke-width">>, 1}
        ], <<>>)
    ],
    Polyline = z_tags:render_tag(
        <<"polyline">>,
        [
            {<<"points">>, PolylinePoints},
            {<<"fill">>, <<"none">>},
            {<<"stroke">>, LineColor},
            {<<"stroke-width">>, 2},
            {<<"stroke-linejoin">>, <<"round">>}
        ],
        <<>>),
    [
        Grid,
        Axis,
        Polyline,
        render_line_markers(Points, LineColor),
        render_line_labels(LabelPoints, Bottom, Left, Width - 8)
    ].

line_points([], _Index, _Step, _Left, _Bottom, _PlotHeight, _Minimum, _Span, Acc) ->
    lists:reverse(Acc);
line_points([{Label, Value, _Color} | Rest], Index, Step, Left, Bottom, PlotHeight, Minimum, Span, Acc) ->
    X = Left + Index * Step,
    Y = Bottom - (Value - Minimum) / Span * PlotHeight,
    line_points(Rest, Index + 1, Step, Left, Bottom, PlotHeight, Minimum, Span, [{Label, Value, X, Y} | Acc]).

render_line_markers(Points, Color) ->
    [
        z_tags:render_tag(
            <<"circle">>,
            [
                {<<"cx">>, coordinate(X)},
                {<<"cy">>, coordinate(Y)},
                {<<"r">>, 3},
                {<<"fill">>, Color}
            ],
            z_tags:render_tag(<<"title">>, [], escape(tooltip(Label, Value, undefined))))
        || {Label, Value, X, Y} <- Points
    ].

render_line_grid(Ticks, TickStep, Points, Left, Right, Top, Bottom, PlotHeight, Minimum, Span) ->
    Horizontal = [
        begin
            Y = Bottom - (Tick - Minimum) / Span * PlotHeight,
            [
                z_tags:render_tag(
                    <<"line">>,
                    [
                        {<<"class">>, <<"z-chart-grid-line z-chart-grid-line-horizontal">>},
                        {<<"x1">>, Left},
                        {<<"y1">>, coordinate(Y)},
                        {<<"x2">>, Right},
                        {<<"y2">>, coordinate(Y)},
                        {<<"stroke">>, <<"#d9d9d9">>},
                        {<<"stroke-opacity">>, <<"0.7">>},
                        {<<"stroke-width">>, 1}
                    ],
                    <<>>),
                z_tags:render_tag(
                    <<"text">>,
                    [
                        {<<"class">>, <<"z-chart-axis-label z-chart-axis-label-y">>},
                        {<<"x">>, Left - 6},
                        {<<"y">>, coordinate(Y)},
                        {<<"text-anchor">>, <<"end">>},
                        {<<"dominant-baseline">>, <<"middle">>},
                        {<<"fill">>, <<"#666666">>},
                        {<<"font-size">>, 10}
                    ],
                    format_axis_value(Tick, TickStep))
            ]
        end
        || Tick <- Ticks
    ],
    Vertical = [
        z_tags:render_tag(
            <<"line">>,
            [
                {<<"class">>, <<"z-chart-grid-line z-chart-grid-line-vertical">>},
                {<<"x1">>, coordinate(X)},
                {<<"y1">>, Top},
                {<<"x2">>, coordinate(X)},
                {<<"y2">>, Bottom},
                {<<"stroke">>, <<"#d9d9d9">>},
                {<<"stroke-opacity">>, <<"0.7">>},
                {<<"stroke-width">>, 1}
            ],
            <<>>)
        || {_Label, _Value, X, _Y} <- Points
    ],
    z_tags:render_tag(
        <<"g">>,
        [
            {<<"class">>, <<"z-chart-grid">>},
            {<<"aria-hidden">>, <<"true">>}
        ],
        [Horizontal, Vertical]).

line_label_points([]) ->
    [];
line_label_points([Point]) ->
    [Point];
line_label_points([First, Second | _] = Points) ->
    {_Label1, _Value1, X1, _Y1} = First,
    {_Label2, _Value2, X2, _Y2} = Second,
    PointSpacing = abs(X2 - X1),
    RequiredSpacing = lists:max([
        line_label_width(Label) + ?LINE_LABEL_GAP
        || {Label, _Value, _X, _Y} <- Points
    ]),
    InitialInterval = erlang:max(1, trunc(math:ceil(RequiredSpacing / PointSpacing))),
    {_LastLabel, _LastValue, Right, _LastY} = lists:last(Points),
    Interval = line_label_interval(Points, X1, Right, InitialInterval),
    line_label_points(Points, Interval, 0).

line_label_interval(Points, Left, Right, Interval) ->
    LabelPoints = line_label_points(Points, Interval, 0),
    case line_labels_fit(LabelPoints, Left, Right) of
        true -> Interval;
        false -> line_label_interval(Points, Left, Right, Interval + 1)
    end.

line_label_points([], _Interval, _Index) ->
    [];
line_label_points([Point | Rest], Interval, Index) when Index rem Interval =:= 0 ->
    [Point | line_label_points(Rest, Interval, Index + 1)];
line_label_points([_Point | Rest], Interval, Index) ->
    line_label_points(Rest, Interval, Index + 1).

line_label_width(Label) ->
    z_string:len(short_label(Label, 12)) * ?LINE_LABEL_CHARACTER_WIDTH.

line_labels_fit([], _Left, _Right) ->
    true;
line_labels_fit([_Point], _Left, _Right) ->
    true;
line_labels_fit([Point1, Point2 | Rest], Left, Right) ->
    {_LabelLeft1, LabelRight1} = line_label_bounds(Point1, Left, Right),
    {LabelLeft2, _LabelRight2} = line_label_bounds(Point2, Left, Right),
    LabelRight1 + ?LINE_LABEL_GAP =< LabelLeft2
        andalso line_labels_fit([Point2 | Rest], Left, Right).

line_label_bounds({Label, _Value, X, _Y}, Left, _Right) when X =< Left ->
    {X, X + line_label_width(Label)};
line_label_bounds({Label, _Value, X, _Y}, _Left, Right) when X >= Right ->
    {X - line_label_width(Label), X};
line_label_bounds({Label, _Value, X, _Y}, _Left, _Right) ->
    HalfWidth = line_label_width(Label) / 2,
    {X - HalfWidth, X + HalfWidth}.

render_line_labels(Points, Bottom, Left, Right) ->
    [render_line_label(Point, Bottom, Left, Right) || Point <- Points].

render_line_label({Label, _Value, X, _Y}, Bottom, Left, Right) ->
    z_tags:render_tag(
        <<"text">>,
        [
            {<<"class">>, <<"z-chart-axis-label z-chart-axis-label-x">>},
            {<<"x">>, coordinate(X)},
            {<<"y">>, Bottom + 18},
            {<<"text-anchor">>, line_label_anchor(X, Left, Right)},
            {<<"fill">>, <<"#666666">>},
            {<<"font-size">>, 10}
        ],
        escape(short_label(Label, 12))).

line_label_anchor(X, Left, _Right) when X =< Left ->
    <<"start">>;
line_label_anchor(X, _Left, Right) when X >= Right ->
    <<"end">>;
line_label_anchor(_X, _Left, _Right) ->
    <<"middle">>.

nice_value_scale(Values) ->
    {Minimum0, Maximum0} = value_range(Values),
    {ScaleMinimum, ScaleMaximum} = case Minimum0 == Maximum0 of
        true -> {0, 1};
        false -> {Minimum0, Maximum0}
    end,
    Step = nice_step((ScaleMaximum - ScaleMinimum) / (?LINE_TICK_COUNT - 1)),
    Minimum = math:floor(ScaleMinimum / Step) * Step,
    Maximum = math:ceil(ScaleMaximum / Step) * Step,
    TickIntervals = round((Maximum - Minimum) / Step),
    Ticks = [normalize_tick(Minimum + I * Step, Step) || I <- lists:seq(0, TickIntervals)],
    {Minimum, Maximum, Step, Ticks}.

nice_step(Value) ->
    Exponent = math:floor(math:log(Value) / math:log(10)),
    Magnitude = math:pow(10, Exponent),
    Fraction = Value / Magnitude,
    NiceFraction = if
        Fraction =< 1 -> 1;
        Fraction =< 2 -> 2;
        Fraction =< 5 -> 5;
        true -> 10
    end,
    NiceFraction * Magnitude.

normalize_tick(Value, Step) when abs(Value) < Step / 1000 ->
    0;
normalize_tick(Value, _Step) ->
    Value.

format_axis_value(Value, Step) when Step >= 1 ->
    integer_to_binary(round(Value));
format_axis_value(Value, Step) ->
    Exponent = math:floor(math:log(Step) / math:log(10)),
    case Exponent >= -12 of
        true ->
            Decimals = -trunc(Exponent),
            float_to_binary(Value * 1.0, [{decimals, Decimals}, compact]);
        false ->
            float_to_binary(Value * 1.0, [{scientific, 6}])
    end.

render_no_data(Width, Height, undefined) ->
    render_no_data_1(Width, Height, <<"No data">>);
render_no_data(Width, Height, Context) ->
    render_no_data_1(Width, Height, ?__("No data", Context)).

render_no_data_1(Width, Height, Text) ->
    z_tags:render_tag(
        <<"text">>,
        [
            {<<"x">>, coordinate(Width / 2)},
            {<<"y">>, coordinate(Height / 2)},
            {<<"text-anchor">>, <<"middle">>},
            {<<"dominant-baseline">>, <<"middle">>},
            {<<"fill">>, <<"#777777">>}
        ],
        escape(Text)).

value_range(Values) ->
    {erlang:min(0, lists:min(Values)), erlang:max(0, lists:max(Values))}.

nonzero_span(Span) when Span == 0 -> 1;
nonzero_span(Span) -> Span.

should_render_legend(Type, HideTable, auto) ->
    HideTable andalso (Type =:= pie orelse Type =:= donut);
should_render_legend(Type, HideTable, <<"auto">>) ->
    should_render_legend(Type, HideTable, auto);
should_render_legend(Type, HideTable, "auto") ->
    should_render_legend(Type, HideTable, auto);
should_render_legend(Type, _HideTable, Value) ->
    is_category_chart(Type) andalso z_convert:to_bool(Value).

is_category_chart(pie) -> true;
is_category_chart(donut) -> true;
is_category_chart(horizontal_bar) -> true;
is_category_chart(vertical_bar) -> true;
is_category_chart(line) -> false.

render_legend([], _Context) ->
    <<>>;
render_legend(Data, Context) ->
    Items = [
        z_tags:render_tag(
            <<"li">>,
            [],
            [render_swatch(Color), $\s, escape(Label)])
        || {Label, _Value, Color} <- Data
    ],
    z_tags:render_tag(
        <<"ul">>,
        [
            {<<"class">>, <<"z-chart-legend">>},
            {<<"aria-label">>, ?__("Chart legend", Context)}
        ],
        Items).

render_table(Type, Data, Title, Params, Context) ->
    LabelHeader = case proplists:get_value(label_header, Params) of
        undefined -> ?__("Label", Context);
        LabelHeaderValue -> normalize_text(LabelHeaderValue, ?MAX_LABEL_LENGTH, Context)
    end,
    ValueHeader = case proplists:get_value(value_header, Params) of
        undefined -> ?__("Value", Context);
        ValueHeaderValue -> normalize_text(ValueHeaderValue, ?MAX_LABEL_LENGTH, Context)
    end,
    Header = z_tags:render_tag(
        <<"thead">>,
        [],
        z_tags:render_tag(<<"tr">>, [], [
            z_tags:render_tag(<<"th">>, [{<<"scope">>, <<"col">>}], escape(LabelHeader)),
            z_tags:render_tag(<<"th">>, [{<<"scope">>, <<"col">>}], escape(ValueHeader))
        ])),
    Body = z_tags:render_tag(
        <<"tbody">>,
        [],
        [
            z_tags:render_tag(<<"tr">>, [], [
                z_tags:render_tag(
                    <<"th">>,
                    [{<<"scope">>, <<"row">>}],
                    render_table_label(Type, Label, Color)),
                z_tags:render_tag(<<"td">>, [], format_number(PointValue))
            ])
            || {Label, PointValue, Color} <- Data
        ]),
    z_tags:render_tag(
        <<"table">>,
        [{<<"class">>, <<"z-chart-data">>}],
        [
            z_tags:render_tag(<<"caption">>, [], escape(Title)),
            Header,
            Body
        ]).

render_table_label(line, Label, _Color) ->
    escape(Label);
render_table_label(_Type, Label, Color) ->
    [render_swatch(Color), $\s, escape(Label)].

render_swatch(Color) ->
    z_tags:render_tag(
        <<"svg">>,
        [
            {<<"class">>, <<"z-chart-swatch">>},
            {<<"xmlns">>, <<"http://www.w3.org/2000/svg">>},
            {<<"viewBox">>, <<"0 0 12 12">>},
            {<<"width">>, <<"1em">>},
            {<<"height">>, <<"1em">>},
            {<<"aria-hidden">>, <<"true">>},
            {<<"focusable">>, <<"false">>}
        ],
        z_tags:render_tag(
            <<"rect">>,
            [
                {<<"x">>, 1},
                {<<"y">>, 1},
                {<<"width">>, 10},
                {<<"height">>, 10},
                {<<"rx">>, 1},
                {<<"fill">>, Color},
                {<<"stroke">>, <<"#555555">>},
                {<<"stroke-width">>, 1}
            ],
            <<>>)).

tooltip(Label, Value, undefined) ->
    iolist_to_binary([Label, <<": ">>, format_number(Value)]);
tooltip(Label, Value, Total) ->
    Percentage = Value * 100 / Total,
    iolist_to_binary([
        Label, <<": ">>, format_number(Value),
        <<" (">>, format_percentage(Percentage), <<"%)">>
    ]).

short_label(Label, Length) ->
    z_string:truncatechars(Label, Length, <<"…"/utf8>>).

color(Index, ColorCount, Colors) ->
    PaletteLength = length(Colors),
    BaseColor = lists:nth((Index rem PaletteLength) + 1, Colors),
    Shade = Index div PaletteLength,
    ShadeCount = (ColorCount + PaletteLength - 1) div PaletteLength,
    shade_color(BaseColor, Shade, ShadeCount).

shade_color(Color, 0, _ShadeCount) ->
    Color;
shade_color(<<"#", R1, R2, G1, G2, B1, B2>>, Shade, ShadeCount) ->
    R = hex_byte(R1, R2),
    G = hex_byte(G1, G2),
    B = hex_byte(B1, B2),
    Driver = shade_driver(R, G, B),
    Target = shade_target(Driver, Shade, ShadeCount),
    case Target > Driver of
        true ->
            Amount = (Target - Driver) / (255 - Driver),
            color_binary(
                mix_channel(R, 255, Amount),
                mix_channel(G, 255, Amount),
                mix_channel(B, 255, Amount));
        false ->
            Amount = (Driver - Target) / Driver,
            color_binary(
                mix_channel(R, 0, Amount),
                mix_channel(G, 0, Amount),
                mix_channel(B, 0, Amount))
    end.

shade_driver(R, G, B) ->
    {_Distance, Driver} = lists:min([
        {abs(R - 128), R},
        {abs(G - 128), G},
        {abs(B - 128), B}
    ]),
    Driver.

shade_target(Driver, Shade, ShadeCount) ->
    % Keep dark shades readable and spread the required shades over the remaining range.
    MinTarget = round(Driver * (1.0 - ?MAX_DARKEN_AMOUNT)),
    Targets = [
        Value
        || Delta <- lists:seq(1, 255),
           Value <- [Driver + Delta, Driver - Delta],
           Value >= MinTarget,
           Value =< 255
    ],
    TargetCount = length(Targets),
    Position = (Shade * TargetCount + ShadeCount - 1) div ShadeCount,
    lists:nth(Position, Targets).

mix_channel(Channel, Target, Amount) ->
    round(Channel + (Target - Channel) * Amount).

hex_byte(High, Low) ->
    hex_value(High) * 16 + hex_value(Low).

hex_value(Digit) when Digit >= $0, Digit =< $9 -> Digit - $0;
hex_value(Digit) when Digit >= $A, Digit =< $F -> Digit - $A + 10;
hex_value(Digit) when Digit >= $a, Digit =< $f -> Digit - $a + 10.

color_binary(R, G, B) ->
    <<"#", (hex_digit(R bsr 4)), (hex_digit(R band 15)),
        (hex_digit(G bsr 4)), (hex_digit(G band 15)),
        (hex_digit(B bsr 4)), (hex_digit(B band 15))>>.

hex_digit(Value) when Value < 10 -> $0 + Value;
hex_digit(Value) -> $a + Value - 10.

coordinate(Value) when is_integer(Value) ->
    integer_to_binary(Value);
coordinate(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 3}, compact]).

format_number(Value) when is_integer(Value) ->
    integer_to_binary(Value);
format_number(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 6}, compact]).

format_percentage(Value) ->
    float_to_binary(Value, [{decimals, 1}, compact]).

escape(Value) ->
    z_html:escape(Value).
