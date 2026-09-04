-module(export_encoder_csv_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

styled_cell_exports_plain_value_test() ->
    Context = #context{},
    {ok, State} = export_encoder_csv:init([{rsc_props, []}], Context),
    Cell = export_encoder:cell(2, #{ background_color => <<"#BBE1FA">> }),
    {ok, Encoded, _State} = export_encoder_csv:row([Cell, <<"Note">>], State, Context),
    ?assertEqual(<<"2,\"Note\"\r\n">>, Encoded).
