%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_datetime_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

timesince_simple_test() ->
    C = #context{},
    ?assertEqual(<<"10 seconds ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, C))),
    ?assertEqual(<<"in 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, C))),
    ?assertEqual(<<"10 hours ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, C))),
    ?assertEqual(<<"now">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, C))),
    ?assertEqual(<<"moments ago">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, C))).

timesince_no_time_indication_test() ->
    C = #context{},
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, [], C))),
    ?assertEqual(<<"10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, [], C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, [], C))),
    ?assertEqual(<<"">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, [], C))),
    ?assertEqual(<<"moments">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, [], C))).

timesince_user_specified_texts_test() ->
    C = #context{},
    ?assertEqual(<<"10 seconds foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,45}}, "foo,bar,baz", C))),
    ?assertEqual(<<"baz 10 seconds">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,25}}, "foo,   bar,    baz", C))),
    ?assertEqual(<<"10 hours">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{20,13,45}}, " ,bar,baz", C))),
    ?assertEqual(<<"bar">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,35}}, " ,bar", C))),
    ?assertEqual(<<"moments foo">>, iolist_to_binary(z_datetime:timesince({{2010,9,27},{10,13,35}}, {{2010,9,27},{10,13,37}}, "foo,bar,baz", C))).

timesince_components_test() ->
    C = #context{},
    Base = {{2020,1,1},{0,0,0}},
    Later = {{2020,1,1},{1,3,0}},
    ?assertEqual(<<"1 hour, 3 minutes ago">>, text(z_datetime:timesince(Base, Later, C))),
    ?assertEqual(<<"in 1 hour, 3 minutes">>, text(z_datetime:timesince(Later, Base, C))),
    ?assertEqual(<<"1 hour ago">>, text(z_datetime:timesince(Base, Later, default, 1, C))),
    ?assertEqual(<<"in 1 hour">>, text(z_datetime:timesince(Later, Base, <<"default">>, 1, C))),
    ?assertEqual(<<"1 hour, 3 minutes">>, text(z_datetime:timesince(Base, Later, [], C))),
    ?assertEqual(<<"1 hour, 3 minutes foo">>, text(z_datetime:timesince(Base, Later, "foo,bar,baz", C))),
    ?assertEqual(<<"baz 1 hour, 3 minutes">>, text(z_datetime:timesince(Later, Base, "foo,bar,baz", C))),
    ?assertEqual(<<"in a few moments">>, text(z_datetime:timesince({{2020,1,1},{0,0,9}}, Base, C))),
    ?assertEqual(<<>>, text(z_datetime:timesince(undefined, Base, C))),
    ?assertEqual(<<>>, text(z_datetime:timesince(Base, undefined, C))).

timesince_units_test() ->
    C = #context{},
    Base = {{2020,1,1},{0,0,0}},
    lists:foreach(fun({Later, Duration}) ->
        ?assertEqual(<<Duration/binary, " ago">>, text(z_datetime:timesince(Base, Later, C))),
        ?assertEqual(<<"in ", Duration/binary>>, text(z_datetime:timesince(Later, Base, C))),
        ?assertEqual(Duration, text(z_datetime:timesince(Base, Later, [], C)))
    end, [
        {{{2020,1,1},{0,0,10}}, <<"10 seconds">>},
        {{{2020,1,1},{0,1,0}}, <<"1 minute">>},
        {{{2020,1,1},{0,2,0}}, <<"2 minutes">>},
        {{{2020,1,1},{1,0,0}}, <<"1 hour">>},
        {{{2020,1,1},{2,0,0}}, <<"2 hours">>},
        {{{2020,1,2},{0,0,0}}, <<"1 day">>},
        {{{2020,1,3},{0,0,0}}, <<"2 days">>},
        {{{2020,2,1},{0,0,0}}, <<"1 month">>},
        {{{2020,3,1},{0,0,0}}, <<"2 months">>},
        {{{2021,1,1},{0,0,0}}, <<"1 year">>},
        {{{2022,1,1},{0,0,0}}, <<"2 years">>}
    ]).

timesince_czech_test() ->
    {ok, Rule} = z_gettext_plural:parse(<<"nplurals=3; plural=n==1 ? 0 : n>=2 && n<=4 ? 1 : 2;">>),
    Table = ets:new(timesince_czech, [set]),
    C = #context{translation_table=Table, language=[cs, en]},
    Base = {{2020,1,1},{0,0,0}},
    try
        ets:insert(Table, [
            {{context, <<"relative time: past">>, {plural, <<"{n} hour">>, <<"{n} hours">>}}, [{cs, {Rule,
                {<<"{n} hodinou">>, <<"{n} hodinami">>, <<"{n} hodinami">>}}}]},
            {{context, <<"relative time: future">>, {plural, <<"{n} hour">>, <<"{n} hours">>}}, [{cs, {Rule,
                {<<"{n} hodinu">>, <<"{n} hodiny">>, <<"{n} hodin">>}}}]},
            {{context, <<"relative time: future">>, {plural, <<"{n} minute">>, <<"{n} minutes">>}}, [{cs, {Rule,
                {<<"{n} minutu">>, <<"{n} minuty">>, <<"{n} minut">>}}}]},
            {<<"{duration} ago">>, [{cs, <<"před {duration}"/utf8>>}]},
            {<<"in {duration}">>, [{cs, <<"za {duration}">>}]}
        ]),
        lists:foreach(fun({N, Expected}) ->
            ?assertEqual(Expected, text(z_datetime:timesince({{2020,1,1},{N,0,0}}, Base, C)))
        end, [{1, <<"za 1 hodinu">>}, {3, <<"za 3 hodiny">>}, {5, <<"za 5 hodin">>}]),
        ?assertEqual(<<"před 1 hodinou"/utf8>>, text(z_datetime:timesince(Base, {{2020,1,1},{1,0,0}}, C))),
        ?assertEqual(<<"před 3 hodinami"/utf8>>, text(z_datetime:timesince(Base, {{2020,1,1},{3,0,0}}, C))),
        ?assertEqual(<<"za 1 hodinu, 3 minuty">>, text(z_datetime:timesince({{2020,1,1},{1,3,0}}, Base, C))),
        ets:insert(Table, {<<"{first}, {second}">>, [{cs, <<"{second} / {first}">>}]}),
        ?assertEqual(<<"za 3 minuty / 1 hodinu">>, text(z_datetime:timesince({{2020,1,1},{1,3,0}}, Base, C))),
        % A missing duration translation uses the English source and rule.
        ?assertEqual(<<"3 hours">>, text(z_datetime:timesince(Base, {{2020,1,1},{3,0,0}}, [], C)))
    after
        ets:delete(Table)
    end.

timesince_six_forms_test() ->
    {ok, Rule} = z_gettext_plural:parse(<<"nplurals=6; plural=n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 && n%100<=99 ? 4 : 5;">>),
    Table = ets:new(timesince_six_forms, [set]),
    C = #context{translation_table=Table, language=[ar, en]},
    Base = {{2020,1,1},{0,0,0}},
    try
        % Distinct test markers make each Arabic plural-category selection visible.
        ets:insert(Table, {{context, <<"relative time: past">>, {plural, <<"{n} year">>, <<"{n} years">>}}, [{ar, {Rule,
            {<<"zero {n}">>, <<"one {n}">>, <<"two {n}">>, <<"few {n}">>, <<"many {n}">>, <<"other {n}">>}}}]}),
        ets:insert(Table, {<<"{duration} ago">>, [{ar, <<"{duration}">>}]}),
        lists:foreach(fun({N, Expected}) ->
            ?assertEqual(Expected, text(z_datetime:timesince(Base, {{2020+N,1,1},{0,0,0}}, C)))
        end, [{1, <<"one 1">>}, {2, <<"two 2">>}, {3, <<"few 3">>}, {11, <<"many 11">>}, {100, <<"other 100">>}])
    after
        ets:delete(Table)
    end.

text(Value) -> iolist_to_binary(Value).

time_add_month_test() ->
    ?assertEqual({{2021,12,1},{0,0,0}}, z_datetime:next_month({{2022,1,1},{0,0,0}}, -1)),
    ?assertEqual({{2022,2,1},{0,0,0}},  z_datetime:next_month({{2022,1,1},{0,0,0}}, 1)),
    ?assertEqual({{2022,2,28},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 1)),
    ?assertEqual({{2022,3,31},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 2)),
    ?assertEqual({{2022,4,30},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 3)),
    ?assertEqual({{2023,1,31},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 12)),
    ?assertEqual({{2023,2,28},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 13)),
    ?assertEqual({{2024,2,29},{0,0,0}}, z_datetime:next_month({{2022,1,31},{0,0,0}}, 25)).

repair_test() ->
    ?assertEqual({{2021,12,1},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,12,1},{0,0,undefined}})),
    ?assertEqual({{2021,12,1},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,12,1},{0,undefined,0}})),
    ?assertEqual({{2021,12,1},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,12,1},{undefined,0,0}})),
    ?assertEqual({{2021,12,1},{0,1,1}}, z_datetime:maybe_fix_datetime({{2021,12,1},{0,0,61}})),
    ?assertEqual({{2021,12,1},{1,1,0}}, z_datetime:maybe_fix_datetime({{2021,12,1},{0,61,0}})),
    ?assertEqual({{2021,11,30},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,12,0},{0,0,0}})),
    ?assertEqual({{2020,12,1},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,0,1},{0,0,0}})),
    ?assertEqual({{2020,11,30},{0,0,0}}, z_datetime:maybe_fix_datetime({{2021,0,0},{0,0,0}})).

bulk_arithmetic_matches_single_steps_test() ->
    Dates = [{{Y,M,D},{23,59,58}} || {Y,M,D} <- [
        {-401,12,31}, {-400,2,28}, {-1,12,31}, {0,2,28}, {1,1,1},
        {1900,2,28}, {2000,2,28}, {2024,2,29}, {2026,12,31}]],
    Units = [{next_second,prev_second}, {next_minute,prev_minute},
        {next_hour,prev_hour}, {next_day,prev_day}, {next_week,prev_week}],
    lists:foreach(fun({Next,Prev}) ->
        lists:foreach(fun(Date) ->
            lists:foreach(fun(N) ->
                Expected = repeat_shift(Date, N, Next, Prev),
                ?assertEqual(Expected, apply(z_datetime, Next, [Date,N])),
                ?assertEqual(Expected, apply(z_datetime, Prev, [Date,-N]))
            end, [-401,-60,-7,-1,0,1,7,60,401])
        end, Dates)
    end, Units).

repeat_shift(Date, 0, _, _) -> Date;
repeat_shift(Date, N, Next, Prev) when N > 0 ->
    repeat_shift(apply(z_datetime, Next, [Date]), N-1, Next, Prev);
repeat_shift(Date, N, Next, Prev) ->
    repeat_shift(apply(z_datetime, Prev, [Date]), N+1, Next, Prev).

bulk_arithmetic_large_offsets_test() ->
    Date = {{2000,2,29},{12,34,56}},
    Cycles = 1000000,
    Future = {{2000 + Cycles*400,2,29},{12,34,56}},
    Past = {{2000 - Cycles*400,2,29},{12,34,56}},
    lists:foreach(fun({Next,Prev,N}) ->
        ?assertEqual(Future, apply(z_datetime, Next, [Date,N])),
        ?assertEqual(Past, apply(z_datetime, Prev, [Date,N])),
        ?assertEqual(Date, apply(z_datetime, Prev, [Future,N]))
    end, [{next_second,prev_second,Cycles*146097*86400},
          {next_minute,prev_minute,Cycles*146097*1440},
          {next_hour,prev_hour,Cycles*146097*24},
          {next_day,prev_day,Cycles*146097},
          {next_week,prev_week,Cycles*20871},
          {next_month,prev_month,Cycles*4800},
          {next_year,prev_year,Cycles*400}]).

bulk_arithmetic_input_shapes_test() ->
    Date = {2024,2,29},
    ?assertEqual(Date, z_datetime:next_day(Date,0)),
    ?assertEqual(Date, z_datetime:prev_week(Date,0)),
    ?assertEqual({{2024,3,1},{0,0,0}}, z_datetime:next_day(Date,1)),
    ?assertEqual({{2024,2,22},{0,0,0}}, z_datetime:prev_week(Date,1)),
    ?assertEqual({{0,2,29},{0,0,0}}, z_datetime:prev_month({0,3,31},1)),
    ?assertEqual({{-1,12,31},{0,0,0}}, z_datetime:prev_month({0,1,31},1)),
    ?assertEqual({{0,1,31},{0,0,0}}, z_datetime:next_month({-1,12,31},1)).
