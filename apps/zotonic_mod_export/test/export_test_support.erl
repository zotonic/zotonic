%% @doc Isolated mock tests and Cowboy request fixtures for export tests.
-module(export_test_support).
-export([isolated/1, request/1]).
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

-spec request(Overrides) -> cowboy_req:req() when
    Overrides :: map().
request(Overrides) ->
    maps:merge(#{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"https">>,
        host => <<"localhost">>,
        port => 443,
        path => <<"/feed/rss">>,
        qs => <<>>,
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 443},
        pid => self(),
        ref => export_test_listener,
        streamid => 1,
        cert => undefined,
        has_body => false,
        body_length => 0
    }, Overrides).
