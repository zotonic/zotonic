%% @copyright 2026 Marc Worrell
%% @doc Protocol-v3 client integration against a local fake runner. No media tools or DB.
-module(z_media_runner_integration_tests).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(S) -> {"capacity bursts", {timeout, 30, fun() -> capacity_bursts(S) end}} end,
        fun(S) -> {"sustained capacity", {timeout, 30, fun() -> sustained_capacity(S) end}} end,
        fun(S) -> {"capacity deadlines", {timeout, 15, fun() -> capacity_deadlines(S) end}} end,
        fun(S) -> {"long output filenames", fun() -> long_output_filenames(S) end} end,
        fun(S) -> {"local execution limits", fun() -> local_execution_limits(S) end} end,
        fun(S) -> {"recoverable input failures", fun() -> recoverable_input_failures(S) end} end,
        fun(S) -> {"roundtrip", {timeout, 30, fun() -> roundtrip(S) end}} end,
        fun(S) -> {"failures", {timeout, 30, fun() -> failures(S) end}} end,
        fun(S) -> {"polling", {timeout, 30, fun() -> polling(S) end}} end,
        fun(S) -> {"failover", {timeout, 30, fun() -> failover(S) end}} end,
        fun(S) -> {"no_local_fallback", {timeout, 30, fun() -> no_local_fallback(S) end}} end,
        fun(S) -> {"capability", {timeout, 30, fun() -> capability(S) end}} end,
        fun(S) -> {"transport", {timeout, 30, fun() -> transport(S) end}} end,
        fun(S) -> {"self_signed_https", {timeout, 30, fun() -> self_signed_https(S) end}} end,
        fun(S) -> {"path_boundaries", {timeout, 30, fun() -> path_boundaries(S) end}} end,
        fun(S) -> {"preview_publication", {timeout, 30, fun() -> preview_publication(S) end}} end
    ]}.

setup() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    {ok, Registry} = z_media_runner:start_link(),
    Table = ets:new(runner_fixture, [public, set]),
    ets:insert(Table, [{mode, normal}, {uploads, 0}, {submits, 0}]),
    {ok, Server} = mochiweb_http:start([{name, undefined}, {ip, "127.0.0.1"}, {port, 0},
        {loop, fun(Req) -> handle(Req, Table) end}]),
    Port = mochiweb_socket_server:get(Server, port),
    Base = iolist_to_binary(["http://127.0.0.1:", integer_to_list(Port), "/media-runner/jobs"]),
    ets:insert(Table, {base, Base}),
    Dir = "/tmp/runner-integration-" ++ integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Input = filename:join(Dir, "input with spaces.png"),
    ok = file:write_file(Input, binary:copy(<<"input">>, 500000)),
    Keys = [media_runners, media_runner_hostname, media_runner_local_fallback,
        media_runner_wait_timeout, media_runner_max_input_bytes, media_runner_max_output_bytes, environment],
    Saved = [{K, application:get_env(zotonic, K)} || K <- Keys],
    lists:foreach(fun(K) -> application:unset_env(zotonic, K) end, Keys),
    application:set_env(zotonic, media_runner_wait_timeout, 20),
    application:set_env(zotonic, media_runners, [#{
        hostname => iolist_to_binary(["127.0.0.1:", integer_to_list(Port)]),
        protocol => <<"http">>, oauth2_key => <<"test-token">>}]),
    #{registry => Registry, server => Server, table => Table, base => Base, dir => Dir,
        input => Input, output => filename:join(Dir, "out.png"), saved => Saved,
        runners => [#{url => Base, token => <<"test-token">>}]}.

cleanup(#{registry := Registry, server := Server, table := Table, dir := Dir, saved := Saved}) ->
    mochiweb_http:stop(Server),
    gen_server:stop(Registry),
    ets:delete(Table),
    {ok, Files} = file:list_dir(Dir),
    lists:foreach(fun(F) -> file:delete(filename:join(Dir, F)) end, Files),
    file:del_dir(Dir),
    lists:foreach(fun
        ({K, undefined}) -> application:unset_env(zotonic, K);
        ({K, {ok, V}}) -> application:set_env(zotonic, K, V)
    end, Saved),
    z_media_imagemagick:clear_cache().

run(#{input := In, output := Out, runners := Runners}) ->
    Cmd = "ffmpeg -i " ++ z_utils:os_filename(In) ++ " " ++ z_utils:os_filename(Out),
    z_media_runner_job:run(ffmpeg, Cmd, #{read => [In], write => [Out]}, Runners,
        <<"https://client.example/media-runner/callback">>).

local_execution_limits(#{output := Out}) ->
    application:set_env(zotonic, media_runners, []),
    %% Simulate FFmpeg writing a partial output before it exceeds its deadline.
    Cmd = "printf partial > " ++ z_utils:os_filename(Out) ++ "; sleep 0.3",
    Result = z_exec:run(ffmpeg, Cmd, #{timeout => 100}),
    ?assertEqual({error, timeout}, Result),
    ?assertEqual({ok, <<"partial">>}, file:read_file(Out)),
    ?assert(z_video_convert:retryable(Result)),
    ?assertEqual({error, output_limit}, z_exec:run(ffmpeg,
        "printf diagnostic", #{max_size => 4})),
    ?assertEqual({ok, <<"done">>}, z_exec:run(ffprobe, "printf done", #{})),
    %% Keep the old string-returning API, including its truncation semantics.
    ?assertEqual("diag", z_exec:run("printf diagnostic", #{max_size => 4})).

recoverable_input_failures(#{input := In, runners := Runners, table := T}) ->
    Run = fun(Path) -> z_media_runner_job:run(ffprobe, "ffprobe " ++ z_utils:os_filename(Path),
        #{read => [Path]}, Runners, <<"https://client.example/media-runner/callback">>) end,
    application:set_env(zotonic, media_runner_max_input_bytes, 0),
    BadConfig = Run(In),
    ?assertEqual({error, media_runner_configuration}, BadConfig),
    ?assert(z_video_convert:retryable(BadConfig)),
    application:unset_env(zotonic, media_runner_max_input_bytes),
    Missing = Run(In ++ ".missing"),
    ?assertEqual({error, {media_runner_input, enoent}}, Missing),
    ?assert(z_video_convert:retryable(Missing)),
    application:set_env(zotonic, media_runner_max_input_bytes, 1),
    TooLarge = Run(In),
    ?assertMatch({error, {media_runner_input, _}}, TooLarge),
    ?assert(z_video_convert:retryable(TooLarge)),
    ?assertEqual(0, value(T, submits)),
    ?assert(filelib:is_regular(In)),
    ?assertNot(z_video_convert:retryable({error, {media_runner_processing, <<"command_failed">>}})).

roundtrip(#{table := T, input := In, output := Out} = S) ->
    ?assertEqual({ok, <<"done">>}, run(S)),
    ?assertEqual({ok, <<"converted">>}, file:read_file(Out)),
    ?assertEqual(1, value(T, uploads)),
    {ok, _, Hash} = z_media_runner_protocol:hash_file(In),
    ?assertEqual({ok, value(T, {file, Hash})}, file:read_file(In)),
    ?assertEqual({ok, <<"done">>}, run(S)),
    ?assertEqual(1, value(T, uploads)),
    ?assertEqual(#{}, sys:get_state(z_media_runner)),
    lists:foreach(fun(Profile) ->
        ?assertEqual({ok, <<"done">>}, z_media_runner_job:run(Profile,
            "tool " ++ z_utils:os_filename(In) ++ " " ++ z_utils:os_filename(Out),
            #{read => [In], write => [Out]}, maps:get(runners, S),
            <<"https://client.example/media-runner/callback">>))
    end, [imagemagick, imagemagick_pdf, ffmpeg_preview, ffprobe]).

failures(#{table := T, output := Out} = S) ->
    ok = file:write_file(Out, <<"original">>),
    lists:foreach(fun(Mode) ->
        ets:insert(T, {mode, Mode}),
        ?assertMatch({error, _}, run(S)),
        ?assertEqual({ok, <<"original">>}, file:read_file(Out))
    end, [unauthorized, busy, processing_error, bad_hash, unexpected_output, foreign_url]),
    ?assertEqual([], filelib:wildcard(filename:join(filename:dirname(Out), ".download-*"))),
    ?assertEqual(#{}, sys:get_state(z_media_runner)).

polling(#{table := T} = S) ->
    ets:insert(T, {mode, polling}),
    ?assertEqual({ok, <<"done">>}, run(S)).

failover(#{table := T, input := In, output := Out, base := Base} = S) ->
    Runners = [#{url => Base, token => <<"one">>}, #{url => Base, token => <<"two">>}],
    {ok, Job} = z_media_runner_protocol:pack(ffmpeg, "ffmpeg", #{read => [In], write => [Out]}),
    [#{token := First} | _] = z_media_runner_pool:rank(Runners, Job, #{}, #{}),
    ets:insert(T, {reject_token, First}),
    ?assertEqual({ok, <<"done">>}, run(S#{runners => Runners})),
    ?assert(value(T, submits) >= 3).

no_local_fallback(#{dir := Dir, runners := Runners, table := T, input := In}) ->
    Sentinel = filename:join(Dir, "must-not-exist"),
    lists:foreach(fun(Tool) ->
        Path = filename:join(Dir, Tool),
        ok = file:write_file(Path, ["#!/bin/sh\ntouch ", z_utils:os_filename(Sentinel), "\nexit 1\n"]),
        ok = file:change_mode(Path, 8#700)
    end, ["magick", "identify", "convert", "ffmpeg", "ffprobe"]),
    OldPath = os:getenv("PATH"),
    true = os:putenv("PATH", Dir ++ ":" ++ OldPath),
    try
        ets:insert(T, {mode, busy}),
        ?assertMatch(#{available := false}, z_media_imagemagick:selected()),
        ?assertMatch({error, _}, mod_video:video_info(In)),
        Pdf = filename:join(Dir, "document.pdf"),
        ok = file:write_file(Pdf, <<"%PDF-1.4\n1 0 obj\n<<>>\nendobj\n%%EOF\n">>),
        ?assertMatch({error, _}, z_media_identify:identify_file_direct(Pdf, Pdf)),
        ?assertNot(filelib:is_file(Sentinel))
    after os:putenv("PATH", OldPath)
    end,
    Cmd = "touch " ++ z_utils:os_filename(Sentinel),
    application:set_env(zotonic, media_runners, [#{hostname => <<"127.0.0.1:1">>, oauth2_key => <<"token">>}]),
    application:set_env(zotonic, media_runner_local_fallback, true),
    ?assertMatch({error, _}, z_exec:run(ffmpeg, Cmd, #{})),
    ets:insert(T, {mode, busy}),
    ?assertMatch({error, _}, z_media_runner_job:run(ffmpeg, Cmd, #{}, Runners,
        <<"https://client.example/media-runner/callback">>)),
    application:set_env(zotonic, media_runners, invalid),
    ?assertEqual({error, media_runner_configuration}, z_exec:run(ffmpeg, Cmd, #{})),
    ?assertNot(filelib:is_file(Sentinel)),
    application:set_env(zotonic, media_runners, []),
    ?assertEqual({ok, <<"local">>}, z_exec:run(ffprobe, "printf local", #{})),
    ?assertEqual("legacy", z_exec:run("printf legacy")),
    ?assertMatch({error, {command_output, _}}, z_exec:run(ffmpeg, "printf error", #{})).

capability(#{base := Base}) ->
    #{host := Host, port := Port} = uri_string:parse(Base),
    Authority = <<Host/binary, ":", (integer_to_binary(Port))/binary>>,
    application:set_env(zotonic, media_runners, [#{hostname => Authority, protocol => <<"http">>, oauth2_key => <<"test-token">>}]),
    ?assertMatch(#{available := true, major := 7, tool := <<"magick">>}, z_media_imagemagick:selected()),
    ?assertNot(z_media_preview:is_legacy_imagemagick()).

transport(#{table := T, base := Base}) ->
    Url = z_media_runner_protocol:control_url(Base, <<"submit">>),
    ets:insert(T, {mode, redirect}),
    ?assertEqual({error, {http_status, 302}}, z_media_runner_protocol:request(Url, <<"token">>, #{})),
    ets:insert(T, {mode, oversized}),
    ?assertEqual({error, response_too_large}, z_media_runner_http:request(post,
        {binary_to_list(Url), [], "application/json", <<"{}">>}, 2000, 10)),
    ets:insert(T, {mode, stalled}),
    ?assertEqual({error, timeout}, z_media_runner_http:request(post,
        {binary_to_list(Url), [], "application/json", <<"{}">>}, 30)).

self_signed_https(#{dir := Dir, table := T}) ->
    OpenSSL = os:find_executable("openssl"),
    ?assertNotEqual(false, OpenSSL),
    Key = filename:join(Dir, "test-key.pem"),
    Cert = filename:join(Dir, "test-cert.pem"),
    %% The certificate is deliberately self-signed and has the wrong hostname.
    os:cmd(z_utils:os_filename(OpenSSL) ++ " req -x509 -newkey rsa:2048 -nodes -days 1"
        ++ " -subj /CN=wrong.example -keyout " ++ z_utils:os_filename(Key)
        ++ " -out " ++ z_utils:os_filename(Cert) ++ " 2>/dev/null"),
    ?assert(filelib:is_regular(Cert)),
    {ok, Server} = mochiweb_http:start([{name, undefined}, {ip, "127.0.0.1"}, {port, 0},
        {ssl, true}, {ssl_opts, [{keyfile, Key}, {certfile, Cert}]},
        {loop, fun(Req) -> handle(Req, T) end}]),
    try
        Port = mochiweb_socket_server:get(Server, port),
        Url = iolist_to_binary(["https://127.0.0.1:", integer_to_list(Port),
            "/api/model/mediarunner_job/get/capabilities"]),
        lists:foreach(fun(Environment) ->
            application:set_env(zotonic, environment, Environment),
            ?assertMatch({ok, #{<<"imagemagick">> := _}},
                z_media_runner_protocol:request(Url, <<"test-token">>, #{}))
        end, [development, production])
    after mochiweb_http:stop(Server)
    end.

path_boundaries(#{dir := Dir}) ->
    In = filename:join(Dir, "unicode-\x{e9}-quote'file.png"),
    Out = filename:join(Dir, "result.png"),
    ok = file:write_file(In, <<"input">>),
    Command = "magick " ++ z_utils:os_filename(In ++ "[0]") ++ " " ++ z_utils:os_filename(Out),
    {ok, Job} = z_media_runner_protocol:pack(imagemagick, Command, #{read => [In], write => [Out]}),
    Portable = maps:get(<<"command">>, Job),
    ?assertEqual(nomatch, binary:match(Portable, unicode:characters_to_binary(Dir))),
    ?assertNotEqual(nomatch, binary:match(Portable, <<"[0]">>)),
    ?assertEqual(<<"long short">>, z_media_runner_protocol:rewrite(<<"/a/b /a">>,
        [{<<"/a">>, <<"short">>}, {<<"/a/b">>, <<"long">>}])),
    {ok, ProbeJob} = z_media_runner_protocol:pack(ffprobe, "ffprobe", #{read => [In]}),
    [#{<<"id">> := Id}] = maps:get(<<"files">>, ProbeJob),
    Marker = iolist_to_binary(["__ZMR_FILE_", integer_to_list(Id), "__"]),
    Result = #{<<"status">> => <<"ok">>, <<"files">> => [],
        <<"stdout">> => base64:encode(jsx:encode(#{<<"filename">> => Marker}))},
    {ok, JSON} = z_media_runner_protocol:unpack(Result,
        #{read => [In], media_runner_profile => <<"ffprobe">>}),
    ?assertEqual(#{<<"filename">> => unicode:characters_to_binary(In)}, jsx:decode(JSON, [return_maps])).

preview_publication(#{dir := Dir, output := Out}) ->
    application:ensure_all_started(gproc),
    application:ensure_all_started(jobs),
    case jobs:queue_info(media_preview_jobs) of
        undefined -> jobs:add_queue(media_preview_jobs, [{regulators, [{counter, [{limit, 3}]}]}]);
        _ -> ok
    end,
    application:set_env(zotonic, media_runners, []),
    Temp = filename:join(Dir, "preview-temp.png"),
    ok = file:write_file(Out, <<"old preview">>),
    ?assertEqual({error, timeout}, z_media_preview:run_cmd(
        "printf partial > " ++ z_utils:os_filename(Temp) ++ "; sleep 0.3",
        Out, Temp, imagemagick, #{timeout => 100}, undefined)),
    ?assertEqual({ok, <<"old preview">>}, file:read_file(Out)),
    ok = file:delete(Temp),
    ?assertEqual({error, convert_error}, z_media_preview:run_cmd("false", Out, Temp,
        imagemagick, #{}, undefined)),
    ?assertEqual({ok, <<"old preview">>}, file:read_file(Out)),
    ?assertEqual(ok, z_media_preview:run_cmd("printf new > " ++ z_utils:os_filename(Temp),
        Out, Temp, imagemagick, #{}, undefined)),
    ?assertEqual({ok, <<"new">>}, file:read_file(Out)),
    Parent = self(),
    [spawn(fun() ->
        Result = z_media_preview:run_cmd("sleep 0.1; false", Out, Temp ++ integer_to_list(N),
            imagemagick, #{}, undefined),
        Parent ! {preview_done, Result}
    end) || N <- [1,2]],
    [receive {preview_done, Result} -> ?assertEqual({error, convert_error}, Result)
        after 2000 -> error(preview_waiter_stuck) end || _ <- [1,2]],
    ?assertEqual({ok, <<"new">>}, file:read_file(Out)).

value(T, Key) ->
    case ets:lookup(T, Key) of [{Key, V}] -> V; [] -> undefined end.

capacity_bursts(#{table := T} = S) ->
    ets:insert(T, {mode, capacity_burst}),
    ?assertEqual({ok, <<"done">>}, run(S)),
    ?assertEqual(4, value(T, {attempts, "submit"})),
    ?assertEqual(3, value(T, {attempts, "reserve"})),
    ?assertEqual(1, value(T, uploads)),
    ?assertEqual(#{}, sys:get_state(z_media_runner)).

sustained_capacity(#{table := T} = S) ->
    ets:insert(T, [{mode, capacity_wait}, {ready_at, erlang:monotonic_time(millisecond) + 6000}]),
    ?assertEqual({ok, <<"done">>}, run(S)),
    ?assert(value(T, {attempts, "submit"}) > 2).

capacity_deadlines(#{table := T} = S) ->
    application:set_env(zotonic, media_runner_wait_timeout, 2),
    lists:foreach(fun(Operation) ->
        ets:insert(T, {mode, {stall_operation, Operation}}),
        Start = erlang:monotonic_time(millisecond),
        ?assertEqual({error, {media_runner_unavailable, timeout}}, run(S)),
        ?assert(erlang:monotonic_time(millisecond) - Start < 2500),
        ?assertEqual(#{}, sys:get_state(z_media_runner))
    end, ["submit", "reserve"]),
    ets:insert(T, [{mode, capacity_wait}, {ready_at, erlang:monotonic_time(millisecond) + 60000}]),
    Start = erlang:monotonic_time(millisecond),
    ?assertEqual({error, {media_runner_unavailable, 429}}, run(S)),
    ?assert(erlang:monotonic_time(millisecond) - Start < 2500),
    ?assert(value(T, {attempts, "submit"}) > 1).

long_output_filenames(#{dir := Dir} = S) ->
    Path = filename:join(Dir, lists:duplicate(251, $a) ++ ".jpg"),
    lists:foreach(fun(Out) ->
        ok = file:write_file(Out, <<"original">>),
        ?assertEqual({ok, <<"done">>}, run(S#{output => Out})),
        ?assertEqual({ok, <<"converted">>}, file:read_file(Out)),
        ?assertEqual([], filelib:wildcard(filename:join(Dir, ".download-*")))
    end, [Path, list_to_binary(Path)]).

handle(Req, T) ->
    Path = mochiweb_request:get(path, Req),
    Method = mochiweb_request:get(method, Req),
    Body = mochiweb_request:recv_body(10000000, Req),
    Mode = value(T, mode),
    Auth = mochiweb_request:get_header_value("authorization", Req),
    Reject = case value(T, reject_token) of
        undefined -> false;
        Token -> Auth =:= "Bearer " ++ binary_to_list(Token)
    end,
    case {Mode, Reject} of
        {unauthorized, _} -> reply(401, <<>>, Req);
        {busy, _} -> reply(503, <<>>, Req);
        {_, true} -> ets:update_counter(T, submits, 1), reply(503, <<>>, Req);
        {redirect, _} -> mochiweb_request:respond({302, [{"Location", "/must-not-follow"}], <<>>}, Req);
        {oversized, _} -> reply(200, binary:copy(<<"x">>, 100000), Req);
        {stalled, _} -> receive after 100 -> reply(200, <<"{}">>, Req) end;
        _ -> capacity_route(Mode, Method, Path, Body, Req, T)
    end.

capacity_route(Mode, Method, "/api/model/mediarunner_job/post/" ++ Operation = Path, Body, Req, T)
        when Mode =:= capacity_burst; Mode =:= capacity_wait ->
    N = ets:update_counter(T, {attempts, Operation}, 1, {{attempts, Operation}, 0}),
    case Operation of
        "submit" ->
            #{<<"id">> := Id} = jsx:decode(Body, [return_maps]),
            case ets:insert_new(T, {capacity_id, Id}) of
                true -> ok;
                false -> ?assertEqual(value(T, capacity_id), Id)
            end;
        _ -> ok
    end,
    case {Mode, Operation, N} of
        {capacity_burst, "submit", 1} -> json(#{<<"outcome">> => <<"full">>}, Req);
        {capacity_burst, "submit", 2} -> reply(429, <<>>, Req);
        {capacity_burst, "reserve", 1} -> json(#{<<"outcome">> => <<"full">>}, Req);
        {capacity_burst, "reserve", 2} -> json(#{<<"outcome">> => <<"busy">>}, Req);
        {capacity_wait, "submit", _} ->
            case erlang:monotonic_time(millisecond) < value(T, ready_at) of
                true -> json(#{<<"outcome">> => <<"full">>}, Req);
                false -> route(Method, Path, Body, Req, T)
            end;
        _ -> route(Method, Path, Body, Req, T)
    end;
capacity_route({stall_operation, Operation}, _, "/api/model/mediarunner_job/post/" ++ Operation, _, Req, _) ->
    timer:sleep(3000),
    reply(503, <<>>, Req);
capacity_route(_, Method, Path, Body, Req, T) -> route(Method, Path, Body, Req, T).

route('PUT', "/media-runner/jobs/files/" ++ Hash, Body, Req, T) ->
    ?assertEqual("lease", mochiweb_request:get_header_value("x-upload-token", Req)),
    HashBin = list_to_binary(Hash),
    ?assertEqual(HashBin, z_media_runner_protocol:hex(crypto:hash(sha256, Body))),
    ets:insert(T, {{file, HashBin}, Body}),
    ets:update_counter(T, uploads, 1),
    reply(204, <<>>, Req);
route('GET', "/media-runner/jobs/results/" ++ _Hash, _Body, Req, T) ->
    Data = case value(T, mode) of bad_hash -> <<"corrupted">>; _ -> <<"converted">> end,
    reply(200, Data, Req);
route(_, "/api/model/mediarunner_job/get/capabilities", _, Req, _) ->
    json(#{<<"imagemagick">> => #{<<"tool">> => <<"magick">>, <<"version">> => <<"7.1.2">>, <<"major">> => 7}}, Req);
route(_, "/api/model/mediarunner_job/post/reserve", Body, Req, _) ->
    #{<<"hash">> := _Hash} = jsx:decode(Body, [return_maps]),
    json(#{<<"outcome">> => <<"upload">>, <<"upload_token">> => <<"lease">>}, Req);
route(_, "/api/model/mediarunner_job/post/submit", Body, Req, T) ->
    Job = jsx:decode(Body, [return_maps]),
    ok = z_media_runner_protocol:validate(Job),
    ets:update_counter(T, submits, 1),
    Missing = [Hash || #{<<"sha256">> := Hash} <- maps:get(<<"files">>, Job), value(T, {file, Hash}) =:= undefined],
    case Missing of
        [] ->
            Result = result(Job, T),
            Id = maps:get(<<"id">>, Job),
            ets:insert(T, {{result, Id}, Result}),
            case value(T, mode) of
                polling -> ok;
                _ -> callback(Job, Result)
            end,
            json(#{<<"outcome">> => <<"accepted">>}, Req);
        _ -> json(#{<<"outcome">> => <<"missing">>, <<"missing">> => Missing}, Req)
    end;
route(_, "/api/model/mediarunner_job/post/status", Body, Req, T) ->
    #{<<"id">> := Id} = jsx:decode(Body, [return_maps]),
    json(#{<<"result">> => value(T, {result, Id})}, Req);
route(_, "/api/model/mediarunner_job/post/received", _, Req, _) -> json(#{}, Req).

result(Job, T) ->
    Hash = z_media_runner_protocol:hex(crypto:hash(sha256, <<"converted">>)),
    Base = value(T, base),
    Url = case value(T, mode) of foreign_url -> <<"http://other.example/result">>;
        _ -> <<Base/binary, "/results/", Hash/binary>> end,
    Files = [#{<<"id">> => maps:get(<<"id">>, F), <<"size">> => 9,
        <<"sha256">> => Hash, <<"url">> => Url} || F <- maps:get(<<"files">>, Job), maps:get(<<"write">>, F)],
    case value(T, mode) of
        processing_error -> #{<<"status">> => <<"error">>, <<"error">> => <<"command_failed">>};
        unexpected_output -> #{<<"status">> => <<"ok">>, <<"stdout">> => <<>>, <<"files">> => [#{<<"id">> => 999}]};
        _ -> #{<<"status">> => <<"ok">>, <<"stdout">> => base64:encode(<<"done">>), <<"files">> => Files}
    end.

callback(Job, Result) ->
    Id = maps:get(<<"id">>, Job),
    Token = maps:get(<<"callback_token">>, Job),
    RD = wrq:create(undefined, 'POST', https, {1,1}, "/media-runner/callback?id=" ++ binary_to_list(Id),
        mochiweb_headers:make([{"authorization", "Bearer " ++ binary_to_list(Token)}, {"content-type", "application/json"}])),
    {true, RD1, State} = controller_media_runner_callback:is_authorized(RD, undefined),
    {{halt, 204}, _, _} = controller_media_runner_callback:process_post(wrq:set_req_body(jsx:encode(Result), RD1), State),
    ok.

json(Map, Req) -> reply(200, jsx:encode(#{<<"status">> => <<"ok">>, <<"result">> => Map}), Req).
reply(Status, Body, Req) -> mochiweb_request:respond({Status, [{"content-type", "application/json"}], Body}, Req).
