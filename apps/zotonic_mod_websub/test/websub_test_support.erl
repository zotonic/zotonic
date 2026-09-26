%% @doc Complete Cowboy request fixtures shared by the WebSub controller tests.
-module(websub_test_support).
-export([request/1]).

-spec request(Overrides) -> cowboy_req:req() when
    Overrides :: map().
request(Overrides) ->
    maps:merge(#{
        method => <<"GET">>,
        version => 'HTTP/1.1',
        scheme => <<"https">>,
        host => <<"localhost">>,
        port => 443,
        path => <<"/.zotonic/websub">>,
        qs => <<>>,
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 443},
        pid => self(),
        ref => websub_test_listener,
        streamid => 1,
        cert => undefined,
        has_body => false,
        body_length => 0
    }, Overrides).
