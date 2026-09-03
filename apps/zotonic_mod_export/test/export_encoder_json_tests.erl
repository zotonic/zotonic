-module(export_encoder_json_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

init_without_resource_props_test() ->
    {ok, _State} = export_encoder_json:init([], #context{}).

list_row_uses_headers_test() ->
    [Row] = encode_rows(
        [<<"Name">>, {id, <<"Identifier">>}],
        [[<<"Alice">>, 42]]),
    ?assertEqual(
        #{
            <<"Name">> => <<"Alice">>,
            <<"Identifier">> => 42
        },
        Row).

map_row_is_unchanged_test() ->
    Map = #{
        <<"custom">> => true,
        <<"count">> => 3
    },
    ?assertEqual([Map], encode_rows([<<"Ignored">>], [Map])).

headers_are_unique_and_rows_are_not_truncated_test() ->
    [Row] = encode_rows(
        [<<"Name">>, <<"Name">>, <<>>],
        [[<<"First">>, <<"Second">>, <<"Empty header">>, <<"Extra">>]]),
    ?assertEqual(
        #{
            <<"Name">> => <<"First">>,
            <<"Name_2">> => <<"Second">>,
            <<"column_3">> => <<"Empty header">>,
            <<"column_4">> => <<"Extra">>
        },
        Row).

missing_row_values_are_null_test() ->
    [Row] = encode_rows(
        [<<"First">>, <<"Second">>],
        [[1]]),
    ?assertEqual(
        #{
            <<"First">> => 1,
            <<"Second">> => undefined
        },
        Row).

list_row_values_are_normalized_test() ->
    [Row] = encode_rows(
        [<<"Date">>, <<"Datetime">>, <<"Translation">>, <<"No date">>],
        [[
            {2026, 9, 2},
            {{2026, 9, 2}, {12, 34, 56}},
            #trans{tr = [{en, <<"Hello">>}]},
            ?ST_JUTTEMIS
        ]]),
    ?assertEqual(
        #{
            <<"Date">> => <<"2026-09-02">>,
            <<"Datetime">> => <<"2026-09-02T12:34:56+00:00">>,
            <<"Translation">> => <<"Hello">>,
            <<"No date">> => undefined
        },
        Row).

encode_rows(Headers, Rows) ->
    Context = #context{},
    {ok, State0} = export_encoder_json:init([{rsc_props, []}], Context),
    {ok, Start, State1} = export_encoder_json:header(Headers, State0, Context),
    {EncodedRows, State2} = lists:mapfoldl(
        fun(Row, State) ->
            {ok, Encoded, NewState} = export_encoder_json:row(Row, State, Context),
            {Encoded, NewState}
        end,
        State1,
        Rows),
    {ok, End} = export_encoder_json:footer(undefined, State2, Context),
    z_json:decode(iolist_to_binary([Start, EncodedRows, End])).
