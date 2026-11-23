-module(zotonic_compress_tests).
-include_lib("eunit/include/eunit.hrl").

%% A downstream command source: exercise the real Cowboy compressor, without a socket.
-export([init/3, data/4, info/3, terminate/3, early_error/5]).

mime_allowlist_test_() ->
    Yes = [<<"text/html">>, <<"TEXT/CSS; charset=utf-8">>, <<"text/csv">>,
        <<"text/x-ubf">>, <<"application/javascript">>, <<"application/x-javascript">>,
        <<"application/ecmascript">>, <<"application/json">>, <<"application/xml">>,
        <<"application/ld+json">>, <<"application/feed+json">>, <<"application/atom+xml">>,
        <<"application/vnd.example+json">>, <<"image/svg+xml">>,
        <<"application/wasm">>, <<"application/x-bert">>, <<"font/ttf">>, <<"font/otf">>,
        <<"application/x-www-form-urlencoded">>, [<<"text/plain">>, <<"; charset=utf-8">>]],
    No = [undefined, <<>>, <<"not a type">>, <<"text/plain; broken">>,
        <<"image/jpeg">>, <<"image/png">>, <<"image/webp">>, <<"image/avif">>,
        <<"image/gif">>, <<"video/mp4">>, <<"audio/mpeg">>, <<"audio/wav">>,
        <<"application/zip">>, <<"application/gzip">>, <<"application/pdf">>,
        <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>,
        <<"application/vnd.oasis.opendocument.text">>, <<"font/woff">>, <<"font/woff2">>,
        <<"application/x-font-woff">>, <<"application/octet-stream">>,
        <<"application/unknown">>, <<"image/json">>],
    [?_assert(z_media_identify:is_mime_compressible(M)) || M <- Yes]
        ++ [?_assertNot(z_media_identify:is_mime_compressible(M)) || M <- No].

complete_response_test() ->
    Body = binary:copy(<<"some repeated text ">>, 100),
    {Headers, Encoded} = response(#{<<"content-type">> => <<"text/plain">>}, Body, true),
    ?assertEqual(<<"gzip">>, maps:get(<<"content-encoding">>, Headers)),
    ?assertEqual(Body, zlib:gunzip(iolist_to_binary(Encoded))),
    ?assertEqual(integer_to_binary(iolist_size(Encoded)), maps:get(<<"content-length">>, Headers)),
    ?assertEqual(<<"accept-encoding">>, maps:get(<<"vary">>, Headers)),
    assert_clean(Headers).

passthrough_test_() ->
    Body = binary:copy(<<"already compressed data">>, 100),
    Cases = [
        {#{<<"content-type">> => <<"image/jpeg">>}, Body, true},
        {#{<<"content-type">> => <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>}, Body, true},
        {#{}, Body, true},
        {#{<<"content-type">> => <<"text/plain">>}, <<"small">>, true},
        {#{<<"content-type">> => <<"text/plain">>}, Body, false},
        {#{<<"content-type">> => <<"text/plain">>, <<"etag">> => <<"\"v1\"">>}, Body, true},
        {#{<<"content-type">> => <<"text/plain">>, <<"content-encoding">> => <<"gzip">>}, Body, true},
        {#{<<"content-type">> => <<"image/jpeg">>, <<"content-encoding">> => <<"br">>}, Body, true},
        {#{<<"content-type">> => <<"text/plain">>, <<"content-encoding">> => <<"identity">>,
            <<"x-zotonic-compress-skip">> => <<"1">>}, Body, true}
    ],
    [?_test(begin
        {H, B} = response(Headers, Data, Accept),
        ?assertEqual(Data, B),
        ?assertEqual(maps:get(<<"content-encoding">>, Headers, undefined),
            maps:get(<<"content-encoding">>, H, undefined)),
        assert_clean(H)
    end) || {Headers, Data, Accept} <- Cases].

stream_test() ->
    {[], S0} = start([], true),
    {[{headers, 200, H}], S1} = cowboy_stream:info(1,
        [{headers, 200, #{<<"content-type">> => <<"text/plain">>, <<"content-length">> => <<"6">>}}], S0),
    ?assertEqual(<<"gzip">>, maps:get(<<"content-encoding">>, H)),
    ?assertNot(maps:is_key(<<"content-length">>, H)),
    assert_clean(H),
    {[{data, nofin, A}], S2} = cowboy_stream:info(1, [{data, nofin, <<"abc">>}], S1),
    {[{data, fin, B}], S3} = cowboy_stream:info(1, [{data, fin, <<"def">>}], S2),
    ?assertEqual(<<"abcdef">>, zlib:gunzip(iolist_to_binary([A, B]))),
    cowboy_stream:terminate(1, normal, S3).

stream_sendfile_passthrough_test() ->
    {[], S0} = start([], true),
    {[{headers, 200, H}], S1} = cowboy_stream:info(1,
        [{headers, 200, #{<<"content-type">> => <<"image/jpeg">>, <<"content-length">> => <<"1000">>}}], S0),
    ?assertNot(maps:is_key(<<"content-encoding">>, H)),
    ?assertEqual(<<"1000">>, maps:get(<<"content-length">>, H)),
    assert_clean(H),
    % If compression were enabled, Cowboy would try to open this nonexistent file.
    Commands = [{data, fin, {sendfile, 0, 1000, "/nonexistent/compression-test.jpg"}}, stop],
    {Result, S2} = cowboy_stream:info(1, Commands, S1),
    ?assertEqual(Commands, Result),
    cowboy_stream:terminate(1, normal, S2).

response(Headers, Body, Accept) ->
    {[{response, 200, H, B}], State} = start([{response, 200, Headers, Body}], Accept),
    cowboy_stream:terminate(1, normal, State),
    {H, B}.

assert_clean(Headers) ->
    ?assertNot(maps:is_key(<<"x-zotonic-compress-skip">>, Headers)).

start(Commands, Accept) ->
    Headers = case Accept of
        true -> #{<<"accept-encoding">> => <<"gzip">>};
        false -> #{}
    end,
    cowboy_stream:init(1, #{headers => Headers}, #{
        stream_handlers => [zotonic_compress_cleanup_h, cowboy_compress_h,
            zotonic_compress_policy_h, ?MODULE],
        test_commands => Commands
    }).

init(_, _, Opts) -> {maps:get(test_commands, Opts), undefined}.
data(_, _, _, State) -> {[], State}.
info(_, Commands, State) -> {Commands, State}.
terminate(_, _, _) -> ok.
early_error(_, _, _, Resp, _) -> Resp.
