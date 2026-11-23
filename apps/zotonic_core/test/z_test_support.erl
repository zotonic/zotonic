%% @doc Run mock-based EUnit fixtures outside the live integration-test VM.
-module(z_test_support).
-export([isolated/1, run/1]).
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
            %% -pa can reorder existing paths. Preserve the parent order exactly,
            %% so the peer does not load an older beam from a dependency directory.
            true = peer:call(Peer, code, set_path, [code:get_path()]),
            Peer
        end,
        fun peer:stop/1,
        fun(Peer) ->
            {timeout, 65, fun() ->
                {Result, Output} = peer:call(Peer, ?MODULE, run, [Tests], 60000),
                io:put_chars(Output),
                ?assertEqual(ok, Result)
            end}
        end}.


%% Capture the peer's EUnit report explicitly. Its standard_io transport does not
%% forward the report to the outer EUnit test's captured output.
run(Tests) ->
    Path = filename:join(os:getenv("TMPDIR", "/tmp"),
        "zotonic-eunit-" ++ os:getpid() ++ "-" ++
        integer_to_list(erlang:unique_integer([positive, monotonic])) ++ ".log"),
    {ok, Io} = file:open(Path, [write, exclusive]),
    Leader = group_leader(),
    try
        true = group_leader(Io, self()),
        Result = eunit:test(Tests, [verbose]),
        ok = file:sync(Io),
        {ok, Output} = file:read_file(Path),
        {Result, Output}
    after
        group_leader(Leader, self()),
        file:close(Io),
        file:delete(Path)
    end.
