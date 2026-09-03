%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Render bounded chart data as inline SVG and an optional data table.
%% @end

-module(z_chart_svg).
-moduledoc(<<
    "Render a small, single-series chart as inline SVG.\n\n",
    "The renderer does not fetch data, load external resources, emit scripts, or\n",
    "accept arbitrary SVG/CSS. All text is escaped and all dimensions, colors, and\n",
    "data sizes are bounded before rendering.\n"
>>).

-export([render/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MAX_POINTS, 256).
-define(MAX_LABEL_LENGTH, 512).
-define(MAX_CLASS_LENGTH, 256).
-define(MAX_DIMENSION, 4096).
-define(MIN_DIMENSION, 64).
-define(MAX_VALUE, 1000000000000000).

-define(DEFAULT_WIDTH, 400).
-define(DEFAULT_HEIGHT, 240).

-define(PALETTE, [
    <<"#4477aa">>,
    <<"#ee6677">>,
    <<"#228833">>,
    <<"#ccbb44">>,
    <<"#66ccee">>,
    <<"#aa3377">>,
    <<"#bbbbbb">>,
    <<"#000000">>
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
            Data = sort_data(Data0, proplists:get_value(sort, Params, false)),
            Colors = normalize_colors(proplists:get_value(colors, Params)),
            ColoredData = colorize_data(Type, Data, Colors),
            Width = normalize_dimension(proplists:get_value(width, Params), ?DEFAULT_WIDTH),
            Height0 = normalize_dimension(proplists:get_value(height, Params), ?DEFAULT_HEIGHT),
            Height = chart_height(Type, Height0, length(Data)),
            Id = normalize_id(proplists:get_value(id, Params)),
            Title = normalize_title(proplists:get_value(title, Params), Context),
            Class = normalize_class(proplists:get_value(class, Params)),
            AriaDescribedBy = normalize_reference(proplists:get_value(aria_describedby, Params)),
            HideTable = z_convert:to_bool(proplists:get_value(hide_table, Params, false)),
            Svg = render_svg(Type, ColoredData, Width, Height, Id, Title, AriaDescribedBy, Context),
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

normalize_colors(Colors) when is_list(Colors) ->
    case normalize_colors(Colors, 0, []) of
        [] -> ?PALETTE;
        Valid -> lists:reverse(Valid)
    end;
normalize_colors(_Colors) ->
    ?PALETTE.

normalize_colors(_Colors, 16, Acc) ->
    Acc;
normalize_colors([Color | Rest], Count, Acc) ->
    case normalize_color(Color) of
        {ok, Valid} -> normalize_colors(Rest, Count + 1, [Valid | Acc]);
        error -> normalize_colors(Rest, Count + 1, Acc)
    end;
normalize_colors(_ImproperOrEmpty, _Count, Acc) ->
    Acc.

-spec colorize_data(chart_type(), [data_point()], [binary()]) -> [colored_data_point()].
colorize_data(line, [], _Colors) ->
    [];
colorize_data(line, [{_Label, _Value, CustomColor} | _] = Data, Colors) ->
    LineColor = case CustomColor of
        undefined -> color(0, Colors);
        _ -> CustomColor
    end,
    [{Label, Value, LineColor} || {Label, Value, _Color} <- Data];
colorize_data(Type, Data, Colors) ->
    colorize_data(Type, Data, Colors, 0, []).

colorize_data(_Type, [], _Colors, _Index, Acc) ->
    lists:reverse(Acc);
colorize_data(Type, [{Label, Value, CustomColor} | Rest], Colors, Index, Acc) ->
    PointColor = case CustomColor of
        undefined -> color(Index, Colors);
        _ -> CustomColor
    end,
    colorize_data(Type, Rest, Colors, Index + 1, [{Label, Value, PointColor} | Acc]).

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
    MaxBytes = MaxLength * 4,
    Prefix = case byte_size(Text) > MaxBytes of
        true -> binary:part(Text, 0, MaxBytes);
        false -> Text
    end,
    z_string:truncatechars(z_string:sanitize_utf8(Prefix), MaxLength, <<>>).

render_svg(Type, Data, Width, Height, Id, Title, AriaDescribedBy, Context) ->
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
        render_plot(Type, Data, Width, Height, Context)
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

render_plot(pie, Data, Width, Height, Context) ->
    render_pie(Data, Width, Height, false, Context);
render_plot(donut, Data, Width, Height, Context) ->
    render_pie(Data, Width, Height, true, Context);
render_plot(horizontal_bar, Data, Width, Height, _Context) ->
    render_horizontal_bar(Data, Width, Height);
render_plot(vertical_bar, Data, Width, Height, _Context) ->
    render_vertical_bar(Data, Width, Height);
render_plot(line, Data, Width, Height, _Context) ->
    render_line(Data, Width, Height).

render_pie(Data, Width, Height, IsDonut, Context) ->
    PieData = [Point || {_Label, Value, _Color} = Point <- Data, Value > 0],
    case lists:sum([Value || {_Label, Value, _Color} <- PieData]) of
        Total when Total > 0 ->
            CX = Width / 2,
            CY = Height / 2,
            Radius = erlang:max(1, erlang:min(Width, Height) / 2 - 8),
            case PieData of
                [{Label, Value, Color}] ->
                    render_full_pie(Label, Value, CX, CY, Radius, Color, IsDonut);
                _ ->
                    {Segments, _Angle} = lists:mapfoldl(
                        fun({Label, Value, Color}, StartAngle) ->
                            Delta = 2 * math:pi() * Value / Total,
                            Segment = render_pie_segment(
                                Label,
                                Value,
                                Total,
                                StartAngle,
                                StartAngle + Delta,
                                CX,
                                CY,
                                Radius,
                                Color,
                                IsDonut),
                            {Segment, StartAngle + Delta}
                        end,
                        -math:pi() / 2,
                        PieData),
                    Segments
            end;
        _ ->
            render_no_data(Width, Height, Context)
    end.

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
    {Minimum, Maximum} = value_range(Values),
    Left = 36,
    Top = 8,
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
    [Axis, Polyline, render_line_markers(Points, LineColor), render_line_labels(Points, Bottom)].

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

render_line_labels([], _Bottom) ->
    [];
render_line_labels([First], Bottom) ->
    [render_line_label(First, Bottom)];
render_line_labels(Points, Bottom) ->
    [render_line_label(hd(Points), Bottom), render_line_label(lists:last(Points), Bottom)].

render_line_label({Label, _Value, X, _Y}, Bottom) ->
    z_tags:render_tag(
        <<"text">>,
        [
            {<<"x">>, coordinate(X)},
            {<<"y">>, Bottom + 18},
            {<<"text-anchor">>, <<"middle">>}
        ],
        escape(short_label(Label, 16))).

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

color(Index, Colors) ->
    PaletteLength = length(Colors),
    BaseColor = lists:nth((Index rem PaletteLength) + 1, Colors),
    Shade = Index div PaletteLength,
    shade_color(BaseColor, Shade).

shade_color(Color, 0) ->
    Color;
shade_color(<<"#", R1, R2, G1, G2, B1, B2>>, Shade) ->
    R = hex_byte(R1, R2),
    G = hex_byte(G1, G2),
    B = hex_byte(B1, B2),
    Driver = shade_driver(R, G, B),
    Target = shade_target(Driver, Shade),
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

shade_target(Driver, Shade) ->
    % 31 is coprime with 255, so all possible shades are visited without repeats.
    Position = ((Shade * 31 - 1) rem 255) + 1,
    Targets = [
        Value
        || Delta <- lists:seq(1, 255),
           Value <- [Driver + Delta, Driver - Delta],
           Value >= 0,
           Value =< 255
    ],
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
