-module(z_csv_parser_tests).

-include_lib("eunit/include/eunit.hrl").


unterminated_row_with_empty_last_field_test() ->
    Data = <<
        "subject\tpredicate\tobject\torder\r\n",
        "1\tsubject\tkeyword1\t\r\n",
        "1\tsubject\tkeyword2\t"
    >>,
    ?assertEqual(
        [
            [<<"subject">>, <<"predicate">>, <<"object">>, <<"order">>],
            [<<"1">>, <<"subject">>, <<"keyword1">>, <<>>],
            [<<"1">>, <<"subject">>, <<"keyword2">>, <<>>]
        ],
        z_csv_parser:scan_data(Data, $\t)).
