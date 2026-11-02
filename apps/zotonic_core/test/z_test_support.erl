%% @doc Run mock-based EUnit fixtures outside the live integration-test VM.
-module(z_test_support).
-export([isolated/1]).
-include_lib("eunit/include/eunit.hrl").

%% Meck mocks affect every process in a VM, including live testsandbox workers.
%% Run the complete fixture in a peer so setup and cleanup are isolated too.
isolated(Tests) ->
    {setup,
        fun() ->
            {ok, Peer, _} = peer:start_link(#{
                connection => standard_io,
                args => ["-pa" | code:get_path()]
            }),
            Peer
        end,
        fun peer:stop/1,
        fun(Peer) ->
            {timeout, 60, fun() ->
                ?assertEqual(ok, peer:call(Peer, eunit, test, [Tests, [verbose]], 60000))
            end}
        end}.

