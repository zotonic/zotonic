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

xss_values_are_escaped_test() ->
    {ok, Html} = render([
        {id, <<"\" onload=\"alert(1)">>},
        {class, <<"safe\" onclick=\"alert(1)">>},
        {title, <<"</title><script>alert(1)</script>">>},
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
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 300)],
    {ok, Html} = render([
        {type, vertical_bar},
        {width, 999999},
        {height, 1},
        {data, Data}
    ]),
    ?assertMatch({_, _}, binary:match(Html, <<"viewBox='0 0 4096 64'">>)),
    ?assertEqual(257, length(binary:matches(Html, <<"<tr>">>))),
    ?assertEqual(nomatch, binary:match(Html, <<">257<">>)).

invalid_rows_count_towards_limit_test() ->
    Data = lists:duplicate(256, invalid) ++ [[<<"too-late">>, 1]],
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

color_key_matches_chart_test() ->
    {ok, Html} = render([
        {type, pie},
        {colors, [<<"112233">>, <<"aabbcc">>]},
        {data, [[<<"One">>, 1], [<<"Two">>, 2]]}
    ]),
    ?assertEqual(2, length(binary:matches(Html, <<"#112233">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#aabbcc">>))).

palette_uses_shades_after_last_color_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 10)],
    {ok, Html} = render([{type, vertical_bar}, {data, Data}]),
    ?assertEqual(2, length(binary:matches(Html, <<"#4477aa">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#ee6677">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#5a87b4">>))),
    ?assertEqual(2, length(binary:matches(Html, <<"#f07887">>))).

custom_palette_uses_shades_test() ->
    Data = [[integer_to_binary(N), N] || N <- lists:seq(1, 256)],
    {ok, Html} = render([
        {type, horizontal_bar},
        {colors, [<<"000000">>]},
        {hide_table, true},
        {data, Data}
    ]),
    ?assertEqual(1, length(binary:matches(Html, <<"#000000">>))),
    ?assertEqual(1, length(binary:matches(Html, <<"#1f1f1f">>))),
    ?assertEqual(1, length(binary:matches(Html, <<"#3e3e3e">>))),
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
