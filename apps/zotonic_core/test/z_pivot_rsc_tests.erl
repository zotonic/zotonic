%% @author Marc Worrell <marc@worrell.nl>
%% @hidden

-module(z_pivot_rsc_tests).

-include_lib("eunit/include/eunit.hrl").


future_repivot_does_not_delay_update_test() ->
    Now = {{2026, 8, 12}, {12, 0, 0}},
    PivotDate = z_datetime:prev_second(Now, 10),
    FutureDate = z_datetime:next_second(Now, 10),
    UpdateDue = z_datetime:prev_second(Now, 30),
    RepivotDue = z_datetime:next_hour(Now),

    ?assert(UpdateDue < PivotDate),
    ?assertEqual(
        pivot_reschedule,
        z_pivot_rsc:queue_action(RepivotDue, PivotDate, FutureDate)),
    ?assertEqual(
        delay,
        z_pivot_rsc:queue_action(Now, PivotDate, FutureDate)),
    ?assertEqual(
        pivot,
        z_pivot_rsc:queue_action(UpdateDue, PivotDate, FutureDate)).
