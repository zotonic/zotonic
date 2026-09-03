-module(chart_svg_tests).
-moduledoc(<<"Tests for bounded and escaped inline SVG chart rendering.\n">>).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

pie_chart_test() ->
    {ok, Html} = render([
        {id, <<"test-chart">>},
        {type, pie},
        {title, <<"Responses">>},
        {data, [[<<"Yes">>, 42], [<<"No">>, 8]]}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"<figure ">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"<svg ">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"<path ">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"<table ">>)),
    ?assertEqual(2, length(binary:matches(Html, <<"class='z-chart-swatch'">>))),
    ?assertEqual(nomatch, binary:match(Html, <<"class='z-chart-legend'">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"Yes: 42 (84.0%)">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"<script">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"<style">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"<foreignObject">>)),
    ?assertEqual(nocache, scomp_base_chart:vary([], context())).

supported_types_test() ->
    Data = [[<<"One">>, -1], [<<"Two">>, 3], [<<"Three">>, 2]],
    TypeElements = [
        {pie, <<"<path ">>},
        {donut, <<"<path ">>},
        {horizontal_bar, <<"<rect ">>},
        {vertical_bar, <<"<rect ">>},
        {line, <<"<polyline ">>}
    ],
    lists:foreach(
        fun({Type, Element}) ->
            {ok, Html} = render([{type, Type}, {data, Data}]),
            ?assertMatch({_, _}, binary:match(Html, Element))
        end,
        TypeElements).

line_chart_has_grid_and_axis_labels_test() ->
    Data = [
        [<<"January">>, 12], [<<"February">>, 19], [<<"March">>, 15],
        [<<"April">>, 28], [<<"May">>, 24], [<<"June">>, 35]
    ],
    {ok, Html} = render([{type, line}, {width, 800}, {data, Data}]),
    ?assertEqual(5, length(binary:matches(
        Html,
        <<"class='z-chart-grid-line z-chart-grid-line-horizontal'">>))),
    ?assertEqual(6, length(binary:matches(
        Html,
        <<"class='z-chart-grid-line z-chart-grid-line-vertical'">>))),
    ?assertEqual(5, length(binary:matches(
        Html,
        <<"class='z-chart-axis-label z-chart-axis-label-y'">>))),
    ?assertEqual(6, length(binary:matches(
        Html,
        <<"class='z-chart-axis-label z-chart-axis-label-x'">>))),
    ?assertMatch({_, _}, binary:match(Html, <<">40</text>">>)).

line_chart_grid_keeps_unlabelled_points_test() ->
    Data = [
        [<<"January">>, 1], [<<"February">>, 2], [<<"March">>, 3],
        [<<"April">>, 4], [<<"May">>, 5], [<<"June">>, 6],
        [<<"July">>, 7], [<<"August">>, 8], [<<"September">>, 9],
        [<<"October">>, 10], [<<"November">>, 11], [<<"December">>, 12]
    ],
    {ok, SpaciousHtml} = render([{type, line}, {width, 1200}, {data, Data}]),
    ?assertEqual(12, length(binary:matches(
        SpaciousHtml,
        <<"class='z-chart-grid-line z-chart-grid-line-vertical'">>))),
    ?assertEqual(12, length(binary:matches(
        SpaciousHtml,
        <<"class='z-chart-axis-label z-chart-axis-label-x'">>))),
    ?assertMatch({match, _}, re:run(
        SpaciousHtml,
        <<"class='z-chart-axis-label z-chart-axis-label-x'[^>]*text-anchor='start'">>)),
    ?assertMatch({match, _}, re:run(
        SpaciousHtml,
        <<"class='z-chart-axis-label z-chart-axis-label-x'[^>]*text-anchor='end'">>)),
    {ok, Html} = render([{type, line}, {width, 800}, {data, Data}]),
    ?assertEqual(12, length(binary:matches(
        Html,
        <<"class='z-chart-grid-line z-chart-grid-line-vertical'">>))),
    ?assertEqual(6, length(binary:matches(
        Html,
        <<"class='z-chart-axis-label z-chart-axis-label-x'">>))),
    ?assertMatch({_, _}, binary:match(Html, <<">January</text>">>)),
    ?assertMatch({_, _}, binary:match(Html, <<">March</text>">>)),
    ?assertMatch({_, _}, binary:match(Html, <<">November</text>">>)),
    ?assertEqual(nomatch, binary:match(Html, <<">February</text>">>)),
    ?assertEqual(nomatch, binary:match(Html, <<">December</text>">>)).

line_chart_uses_nice_axis_values_test() ->
    {ok, Html} = render([
        {type, line},
        {data, [[<<"Low">>, -3], [<<"High">>, 17]]}
    ]),
    ?assertEqual(6, length(binary:matches(
        Html,
        <<"class='z-chart-axis-label z-chart-axis-label-y'">>))),
    ?assertMatch({_, _}, binary:match(Html, <<">-5</text>">>)),
    ?assertMatch({_, _}, binary:match(Html, <<">20</text>">>)).

accepted_data_shapes_test() ->
    {ok, MapHtml} = render([
        {type, pie},
        {data, #{<<"Yes">> => 42, <<"No">> => 8}}
    ]),
    ?assertMatch({_, _}, binary:match(MapHtml, <<"Yes">>)),
    ?assertMatch({_, _}, binary:match(MapHtml, <<"No">>)),
    {ok, ListsHtml} = render([
        {type, horizontal_bar},
        {labels, [<<"First">>, <<"Second">>]},
        {values, [10, 20]}
    ]),
    ?assertMatch({_, _}, binary:match(ListsHtml, <<"First">>)),
    ?assertMatch({_, _}, binary:match(ListsHtml, <<"Second">>)),
    {ok, DataWinsHtml} = render([
        {type, pie},
        {data, [[<<"Data">>, 1]]},
        {labels, [<<"Ignored">>]},
        {values, [2]}
    ]),
    ?assertMatch({_, _}, binary:match(DataWinsHtml, <<"Data">>)),
    ?assertEqual(nomatch, binary:match(DataWinsHtml, <<"Ignored">>)).

row_map_data_test() ->
    {ok, Html} = render([
        {type, pie},
        {data, [
            #{<<"label">> => <<"Yes">>, <<"value">> => <<"42">>, <<"color">> => <<"#123456">>},
            #{label => <<"No">>, value => <<"8.5">>, color => <<"abcdef">>},
            #{<<"label">> => <<"Unknown">>, <<"value">> => <<"5">>}
        ]}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"Yes: 42">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"No: 8.5">>)),
    ?assertEqual(2, length(binary:matches(Html, <<"#123456">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#abcdef">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#228833">>))).

invalid_row_color_uses_palette_test() ->
    {ok, Html} = render([
        {type, horizontal_bar},
        {data, [#{
            <<"label">> => <<"Safe">>,
            <<"value">> => <<"1">>,
            <<"color">> => <<"url(javascript:alert(1))">>
        }]}
    ]),
    ?assertEqual(nomatch, binary:match(Html, <<"url(">>)),
    ?assertEqual(2, length(binary:matches(Html, <<"#4477aa">>))).

utf8_label_truncation_test() ->
    Prefix = binary:copy(<<"🙂"/utf8>>, 512),
    Label = <<Prefix/binary, "not-visible">>,
    {ok, Html} = render([{type, horizontal_bar}, {data, [[Label, 1]]}]),
    ?assertMatch({_, _}, binary:match(Html, Prefix)),
    ?assertEqual(nomatch, binary:match(Html, <<"not-visible">>)).

xss_values_are_escaped_test() ->
    {ok, Html} = render([
        {id, <<"\" onload=\"alert(1)">>},
        {class, <<"safe\" onclick=\"alert(1)">>},
        {title, <<"</title><script>alert(1)</script>">>},
        show_labels,
        {colors, [<<"url(https://example.invalid)">>, <<"ff0000">>]},
        {data, [[<<"</text><foreignObject>bad</foreignObject>">>, 1]]}
    ]),
    ?assertEqual(nomatch, binary:match(Html, <<"<script">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"<foreignObject">>)),
    ?assertEqual(nomatch, binary:match(Html, <<" onload=">>)),
    ?assertEqual(nomatch, binary:match(Html, <<" onclick=">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"url(">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"&lt;/title&gt;&lt;script&gt;">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"&lt;/text&gt;&lt;foreignObject&gt;">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#ff0000">>)).

render_limits_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 1100)],
    {ok, Html} = render([
        {type, vertical_bar},
        {width, 999999},
        {height, 1},
        {data, Data}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"viewBox='0 0 4096 64'">>)),
    ?assertEqual(1001, length(binary:matches(Html, <<"<tr>">>))),
    ?assertEqual(nomatch, binary:match(Html, <<">1001<">>)).

invalid_rows_count_towards_limit_test() ->
    Data = lists:duplicate(1000, invalid) ++ [[<<"too-late">>, 1]],
    {ok, Html} = render([{type, pie}, {data, Data}]),
    ?assertEqual(nomatch, binary:match(Html, <<"too-late">>)),
    ?assertEqual(1, length(binary:matches(Html, <<"<tr>">>))).

external_table_test() ->
    {ok, Html} = render([
        {type, pie},
        {data, [[<<"Yes">>, 1]]},
        {hide_table, true},
        {aria_describedby, <<"result-table">>}
    ]),
    ?assertEqual(nomatch, binary:match(Html, <<"<table ">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"class='z-chart-legend'">>)),
    ?assertEqual(1, length(binary:matches(Html, <<"class='z-chart-swatch'">>))),
    ?assertMatch({_, _}, binary:match(Html, <<"aria-describedby='result-table'">>)).

legend_options_test() ->
    Data = [[<<"One">>, 1], [<<"Two">>, 2]],
    {ok, NoPieLegend} = render([
        {type, pie},
        {data, Data},
        {hide_table, true},
        {legend, false}
    ]),
    ?assertEqual(nomatch, binary:match(NoPieLegend, <<"class='z-chart-legend'">>)),
    {ok, AutoBarLegend} = render([
        {type, horizontal_bar},
        {data, Data},
        {hide_table, true}
    ]),
    ?assertEqual(nomatch, binary:match(AutoBarLegend, <<"class='z-chart-legend'">>)),
    {ok, BarLegend} = render([
        {type, horizontal_bar},
        {data, Data},
        {hide_table, true},
        {legend, true}
    ]),
    ?assertMatch({_, _}, binary:match(BarLegend, <<"class='z-chart-legend'">>)),
    ?assertEqual(2, length(binary:matches(BarLegend, <<"class='z-chart-swatch'">>))),
    {ok, LineLegend} = render([
        {type, line},
        {data, Data},
        {hide_table, true},
        {legend, true}
    ]),
    ?assertEqual(nomatch, binary:match(LineLegend, <<"class='z-chart-legend'">>)).

pie_segment_labels_test() ->
    Data = [[<<"First">>, 70], [<<"Second">>, 25], [<<"Small">>, 5]],
    {ok, PieHtml} = render([{type, pie}, {data, Data}]),
    ?assertEqual(3, length(binary:matches(PieHtml, <<"class='z-chart-segment-label'">>))),
    ?assertEqual(3, length(binary:matches(PieHtml, <<"class='z-chart-segment-label-line'">>))),
    ?assertMatch({_, _}, binary:match(PieHtml, <<">First</text>">>)),
    {ok, DonutHtml} = render([{type, donut}, {data, Data}]),
    ?assertEqual(3, length(binary:matches(DonutHtml, <<"class='z-chart-segment-label'">>))),
    {ok, HiddenHtml} = render([{type, pie}, {show_labels, false}, {data, Data}]),
    ?assertEqual(nomatch, binary:match(HiddenHtml, <<"class='z-chart-segment-labels'">>)).

pie_segment_label_threshold_test() ->
    Data = [[<<"Large">>, 98], [<<"Small">>, 2]],
    {ok, Html} = render([
        {type, pie},
        show_labels,
        {label_min_percent, 3},
        {data, Data}
    ]),
    ?assertEqual(2, length(binary:matches(Html, <<"class='z-chart-segment-label'">>))),
    ?assertMatch({_, _}, binary:match(Html, <<">Large</text>">>)),
    ?assertMatch({_, _}, binary:match(Html, <<">Other</text>">>)),
    ?assertEqual(nomatch, binary:match(Html, <<">Small</text>">>)),
    {ok, BarHtml} = render([{type, vertical_bar}, show_labels, {data, Data}]),
    ?assertEqual(nomatch, binary:match(BarHtml, <<"class='z-chart-segment-labels'">>)).

pie_segment_labels_stay_inside_viewbox_test() ->
    Data = [[integer_to_binary(N), 1] || N <- lists:seq(1, 32)],
    {ok, Html} = render([
        {type, pie},
        {data, Data},
        {width, 400},
        {height, 240}
    ]),
    {match, Matches} = re:run(
        Html,
        <<"class='z-chart-segment-label'[^>]* y='([0-9.]+)'">>,
        [global, {capture, [1], binary}]),
    LabelYs = [z_convert:to_float(Y) || [Y] <- Matches],
    ?assertEqual(length(Data), length(LabelYs)),
    ?assert(lists:all(fun(Y) -> Y >= 13 andalso Y =< 227 end, LabelYs)).

pie_values_are_compacted_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 60)],
    {ok, Html} = render([
        {type, pie},
        {show_labels, false},
        {sort, <<"-value">>},
        {data, Data}
    ]),
    ?assertEqual(51, length(binary:matches(Html, <<"<path ">>))),
    ?assertEqual(52, length(binary:matches(Html, <<"<tr>">>))),
    ?assertMatch({_, _}, binary:match(Html, <<"60: 60">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"11: 11">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"Other: 55">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"10: 10">>)),
    assert_in_order(Html, [<<"11: 11">>, <<"Other: 55">>]).

pie_max_values_option_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 10)],
    {ok, Html} = render([
        {type, donut},
        {show_labels, false},
        {max_pie_values, <<"3">>},
        {data, Data}
    ]),
    ?assertEqual(4, length(binary:matches(Html, <<"<path ">>))),
    ?assertEqual(5, length(binary:matches(Html, <<"<tr>">>))),
    ?assertMatch({_, _}, binary:match(Html, <<"10: 10">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"8: 8">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"Other: 28">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"7: 7">>)),
    {ok, AscendingHtml} = render([
        {type, donut},
        {show_labels, false},
        {sort, <<"+value">>},
        {max_pie_values, 3},
        {data, Data}
    ]),
    assert_in_order(AscendingHtml, [<<"Other: 28">>, <<"8: 8">>]),
    {ok, LabelHtml} = render([
        {type, donut},
        {show_labels, false},
        {sort, <<"-label">>},
        {max_pie_values, 3},
        {data, Data}
    ]),
    assert_in_order(LabelHtml, [<<"8: 8">>, <<"Other: 28">>]).

pie_zero_values_are_omitted_test() ->
    {ok, Html} = render([
        {type, pie},
        {data, [[<<"Zero">>, 0], [<<"Visible">>, 5]]}
    ]),
    ?assertEqual(nomatch, binary:match(Html, <<"Zero">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"Other">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"Visible: 5 (100.0%)">>)),
    ?assertEqual(2, length(binary:matches(Html, <<"<tr>">>))).

color_key_matches_chart_test() ->
    {ok, Html} = render([
        {type, pie},
        {colors, [<<"112233">>, <<"aabbcc">>]},
        {data, [[<<"One">>, 1], [<<"Two">>, 2]]}
    ]),
    ?assertEqual(2, length(binary:matches(Html, <<"#112233">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#aabbcc">>))).

single_color_option_uses_brightness_variations_test() ->
    {ok, Html} = render([
        {type, vertical_bar},
        {color, <<"4477aa">>},
        {palette, [<<"ff0000">>]},
        {data, [[<<"One">>, 1], [<<"Two">>, 2], [<<"Three">>, 3]]}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"#4477aa">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#33597f">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#aec4da">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"#ff0000">>)).

palette_option_overrides_colors_alias_test() ->
    {ok, Html} = render([
        {type, vertical_bar},
        {palette, [<<"112233">>, <<"aabbcc">>]},
        {colors, [<<"ff0000">>]},
        {data, [[<<"One">>, 1], [<<"Two">>, 2]]}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"#112233">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#aabbcc">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"#ff0000">>)).

palette_text_is_parsed_test() ->
    {ok, Html} = render([
        {type, vertical_bar},
        {palette, <<"112233, aabbcc; 445566">>},
        {data, [[<<"One">>, 1], [<<"Two">>, 2], [<<"Three">>, 3]]}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"#112233">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#aabbcc">>)),
    ?assertMatch({_, _}, binary:match(Html, <<"#445566">>)).

default_palette_is_extended_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 16)],
    {ok, Html} = render([{type, vertical_bar}, {hide_table, true}, {data, Data}]),
    lists:foreach(
        fun(Color) ->
            ?assertMatch({_, _}, binary:match(Html, Color))
        end,
        [
            <<"#4477aa">>, <<"#ee6677">>, <<"#228833">>, <<"#ccbb44">>,
            <<"#aa3377">>, <<"#66ccee">>, <<"#ee8866">>, <<"#009988">>,
            <<"#332288">>, <<"#cc79a7">>, <<"#999933">>, <<"#117733">>,
            <<"#ddcc77">>, <<"#882255">>, <<"#44aa99">>, <<"#888888">>
        ]).

palette_uses_shades_after_last_color_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 18)],
    {ok, Html} = render([{type, vertical_bar}, {data, Data}]),
    ?assertEqual(2, length(binary:matches(Html, <<"#4477aa">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#ee6677">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#85a6c7">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#f49ba6">>))).

palette_shades_depend_on_color_count_test() ->
    ShortData = [[integer_to_binary(N), N] || N <- lists:seq(1, 18)],
    LongData = [[integer_to_binary(N), N] || N <- lists:seq(1, 34)],
    {ok, ShortHtml} = render([{type, vertical_bar}, {data, ShortData}]),
    {ok, LongHtml} = render([{type, vertical_bar}, {data, LongData}]),
    ?assertMatch({_, _}, binary:match(ShortHtml, <<"#85a6c7">>)),
    ?assertEqual(nomatch, binary:match(LongHtml, <<"#85a6c7">>)),
    ?assertMatch({_, _}, binary:match(LongHtml, <<"#33597f">>)).

palette_shades_limit_darkening_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 256)],
    {ok, Html} = render([
        {type, horizontal_bar},
        {colors, [<<"ffffff">>]},
        {hide_table, true},
        {data, Data}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"#a6a6a6">>)),
    ?assertEqual(nomatch, binary:match(Html, <<"#a5a5a5">>)).

custom_palette_uses_shades_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 256)],
    {ok, Html} = render([
        {type, horizontal_bar},
        {colors, [<<"000000">>]},
        {hide_table, true},
        {data, Data}
    ]),
    ?assertEqual(1, length(binary:matches(Html, <<"#000000">>))),
    ?assertEqual(1, length(binary:matches(Html, <<"#010101">>))),
    ?assertEqual(1, length(binary:matches(Html, <<"#020202">>))),
    {match, Matches} = re:run(
        Html,
        <<"fill='(#[0-9a-f]{6})'">>,
        [global, {capture, [1], binary}]),
    UniqueColors = lists:usort([Color || [Color] <- Matches]),
    ?assertEqual(256, length(UniqueColors)).

sort_options_test() ->
    Data = [
        #{<<"label">> => <<"Charlie">>, <<"value">> => <<"1">>},
        #{<<"label">> => <<"Alpha">>, <<"value">> => <<"3">>},
        #{<<"label">> => <<"Bravo A">>, <<"value">> => <<"2">>},
        #{<<"label">> => <<"Bravo B">>, <<"value">> => <<"2">>}
    ],
    {ok, SortedHtml} = render([{type, horizontal_bar}, sort, {data, Data}]),
    assert_in_order(SortedHtml, value_descending_order()),
    assert_sort(Data, <<"-value">>, value_descending_order()),
    assert_sort(Data, <<"+value">>, value_ascending_order()),
    assert_sort(Data, <<"value">>, value_ascending_order()),
    assert_sort(Data, <<"-label">>, label_descending_order()),
    assert_sort(Data, <<"+label">>, label_ascending_order()),
    assert_sort(Data, <<"label">>, label_ascending_order()),
    {ok, UnsortedHtml} = render([{type, horizontal_bar}, {data, Data}]),
    assert_in_order(UnsortedHtml, [
        <<"Charlie: 1">>, <<"Alpha: 3">>, <<"Bravo A: 2">>, <<"Bravo B: 2">>
    ]).

invalid_type_test() ->
    ?assertEqual({error, badarg}, z_chart_svg:render([{type, <<"unknown">>}], context())),
    ?assertEqual({ok, <<>>}, scomp_base_chart:render([{type, <<"unknown">>}], #{}, context())).

render(Params) ->
    z_chart_svg:render(Params, context()).

assert_sort(Data, Sort, ExpectedOrder) ->
    {ok, Html} = render([{type, horizontal_bar}, {sort, Sort}, {data, Data}]),
    assert_in_order(Html, ExpectedOrder).

value_descending_order() ->
    [<<"Alpha: 3">>, <<"Bravo A: 2">>, <<"Bravo B: 2">>, <<"Charlie: 1">>].

value_ascending_order() ->
    [<<"Charlie: 1">>, <<"Bravo A: 2">>, <<"Bravo B: 2">>, <<"Alpha: 3">>].

label_descending_order() ->
    [<<"Charlie: 1">>, <<"Bravo B: 2">>, <<"Bravo A: 2">>, <<"Alpha: 3">>].

label_ascending_order() ->
    [<<"Alpha: 3">>, <<"Bravo A: 2">>, <<"Bravo B: 2">>, <<"Charlie: 1">>].

assert_in_order(Html, Needles) ->
    Positions = [
        begin
            {Position, _Length} = binary:match(Html, Needle),
            Position
        end
        || Needle <- Needles
    ],
    ?assertEqual(Positions, lists:sort(Positions)).

context() ->
    #context{site = zotonic_site_testsandbox}.
