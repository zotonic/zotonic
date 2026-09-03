-module(export_encoder_xlsx_tests).

-include_lib("eunit/include/eunit.hrl").

styled_cells_are_written_test() ->
    Color = <<"#bbe1fa">>,
    StatusCell = export_encoder:cell(2, #{ background_color => Color }),
    DateCell = export_encoder:cell({2026, 9, 3}, #{ background_color => Color }),
    InvalidCell = export_encoder:cell(5, #{ background_color => <<"not-a-color">> }),
    Styles = export_encoder_xlsx:xlsx_styles([[StatusCell, DateCell, InvalidCell]]),
    StyleLookup = maps:get(lookup, Styles),

    ?assertEqual(3, maps:get(fill_count, Styles)),
    ?assertEqual([<<"FFBBE1FA">>], maps:get(fill_colors, Styles)),
    ?assertEqual(5, maps:get(cell_xf_count, Styles)),
    ?assertEqual(2, length(maps:get(cell_styles, Styles))),
    ?assertEqual(
        <<"<c r=\"A2\" s=\"3\"><v>2</v></c>">>,
        export_encoder_xlsx:encode_cell([2, 1, StatusCell], false, StyleLookup, undefined)),
    ?assertMatch(
        <<"<c r=\"B2\" s=\"4\">", _/binary>>,
        export_encoder_xlsx:encode_cell([2, 2, DateCell], false, StyleLookup, undefined)),
    ?assertEqual(
        <<"<c r=\"C2\"><v>5</v></c>">>,
        export_encoder_xlsx:encode_cell([2, 3, InvalidCell], false, StyleLookup, undefined)).

cell_styles_are_bounded_test() ->
    Cells = [
        export_encoder:cell(N, #{
            background_color => iolist_to_binary(io_lib:format("#~6.16.0B", [N]))
        })
        || N <- lists:seq(0, 64)
    ],
    Styles = export_encoder_xlsx:xlsx_styles([Cells]),
    ?assertEqual(64, length(maps:get(cell_styles, Styles))),
    ?assertEqual(66, maps:get(fill_count, Styles)),
    ?assertEqual(67, maps:get(cell_xf_count, Styles)).
