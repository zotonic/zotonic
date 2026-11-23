-module(z_test_support_tests).
-include_lib("eunit/include/eunit.hrl").

peer_path_test_() ->
    Paths = code:get_path(),
    z_test_support:isolated(fun() -> ?assertEqual(Paths, code:get_path()) end).

%% The deliberate inner failure must be returned as readable EUnit output,
%% not just an opaque 'error' result from the peer.
peer_failure_report_test_() ->
    z_test_support:isolated(fun() ->
        {error, Output} = z_test_support:run(fun() -> error(deliberate_peer_failure) end),
        ?assert(binary:match(Output, <<"deliberate_peer_failure">>) =/= nomatch),
        ?assert(binary:match(Output, <<"*failed*">>) =/= nomatch)
    end).
