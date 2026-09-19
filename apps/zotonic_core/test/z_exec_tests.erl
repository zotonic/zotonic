%% Copyright 2026 Marc Worrell. SPDX-License-Identifier: Apache-2.0
-module(z_exec_tests).
-include_lib("eunit/include/eunit.hrl").

unknown_profile_test() ->
    ?assertEqual({error, unknown_sandbox_profile}, z_exec:run(unknown, "true", #{})).

%% Opt in explicitly: these tests require a built helper and a host permitting
%% sandbox setup. A failure to enforce is a test failure, never a skipped test.
sandbox_test_() ->
    case os:getenv("ZOTONIC_SANDBOX_TESTS") of
        "1" -> {timeout, 90, fun sandbox_checks/0};
        _ -> []
    end.

sandbox_checks() ->
    {ok, _} = application:ensure_all_started(erlexec),
    ?assertMatch({ok, _}, z_exec:sandbox_status()),
    %% Ensure the documented opt-out works in Erlang, YAML/JSON and env config.
    OldMode = application:get_env(zotonic, exec_sandbox),
    try
        lists:foreach(fun(Mode) ->
            application:set_env(zotonic, exec_sandbox, Mode),
            ?assertEqual({ok, <<"ok">>}, z_exec:run(file, "printf ok", #{}))
        end, [disabled, <<"disabled">>, "disabled", required, <<"required">>, "required"]),
        application:set_env(zotonic, exec_sandbox, <<"invalid">>),
        ?assertEqual({error, invalid_sandbox_mode}, z_exec:run(file, "printf ok", #{}))
    after
        case OldMode of
            undefined -> application:unset_env(zotonic, exec_sandbox);
            {ok, Mode} -> application:set_env(zotonic, exec_sandbox, Mode)
        end
    end,
    Probe = os:getenv("ZOTONIC_SANDBOX_PROBE"),
    ?assertNotEqual(false, Probe),
    application:set_env(zotonic, exec_sandbox_profiles, #{file => #{execute => [Probe]}}),
    try
        ?assertEqual({ok, <<>>}, z_exec:run(file, quote(Probe), #{}))
    after
        application:unset_env(zotonic, exec_sandbox_profiles)
    end,
    Dir = z_convert:to_list(z_tempfile:new()) ++ "-test",
    ok = file:make_dir(Dir),
    Secret = filename:join(Dir, "secret"),
    Input = filename:join(Dir, "input ' quoted.ppm"),
    Output = filename:join(Dir, "output.png"),
    Video = filename:join(Dir, "output.mp4"),
    Pdf = filename:join(Dir, "input.pdf"),
    Fixture = filename:absname(filename:join([filename:dirname(?FILE), "data", "sandbox.pdf"])),
    {ok, _} = file:copy(Fixture, Pdf),
    os:putenv("ZOTONIC_TEST_SECRET", "must-not-be-inherited"),
    ok = file:write_file(Secret, <<"secret">>),
    ok = file:write_file(Input, <<"P3\n1 1\n255\n255 0 0\n">>),
    try
        ?assertEqual({ok, <<"ok">>}, z_exec:run(file, "printf ok", #{})),
        ?assertEqual({ok, <<>>}, z_exec:run(file, "printf '%s' \"$ZOTONIC_TEST_SECRET\"", #{})),
        ?assertMatch({error, _}, z_exec:run(file, ["read x < ", quote(Secret)], #{})),
        ?assertMatch({error, _}, z_exec:run(file, ["printf bad > ", quote(Secret)], #{})),
        ?assertEqual({ok, <<"secret">>}, file:read_file(Secret)),
        ?assertMatch({ok, _}, z_exec:run(file, ["file -b --mime-type ", quote(Input)], #{read => [Input]})),
        ?assertMatch({error, _}, z_exec:run(file, "exec /bin/ls", #{})),
        %% CI copies the successful probe into /usr/lib to catch accidental
        %% recursive EXECUTE rights on readable runtime library directories.
        case os:type() of
            {unix, linux} ->
                DeniedExec = os:getenv("ZOTONIC_SANDBOX_DENIED_EXEC"),
                ?assertNotEqual(false, DeniedExec),
                ?assert(filelib:is_regular(DeniedExec)),
                ?assertMatch({error, _}, z_exec:run(file, ["exec ", quote(DeniedExec)], #{}));
            _ -> ok
        end,
        ?assertEqual({error, output_limit}, z_exec:run(file, "printf abc", #{max_size => 2})),
        ?assertEqual({error, output_limit}, z_exec:run(file, "printf abc >&2", #{max_size => 2})),
        ?assertEqual({error, timeout}, z_exec:run(file, "while :; do :; done", #{timeout => 100})),
        %% Background shell descendants must be killed with their process group.
        ?assertEqual({error, timeout}, z_exec:run(file,
            "(while :; do :; done) & wait", #{timeout => 100})),
        ?assertMatch({error, _}, z_exec:run(file, "exit 1", #{write => [Output]})),
        ?assertNot(filelib:is_file(Output)),
        ok = file:make_symlink(Secret, Output),
        ?assertMatch({error, {sandbox_output, _, not_regular}},
                     z_exec:run(file, "true", #{write => [Output]})),
        ok = file:delete(Output),
        Convert = case os:find_executable("magick") of
            false -> "convert";
            _ -> "magick"
        end,
        ?assertMatch({ok, _}, z_exec:run(imagemagick,
            [Convert, " ", quote(Input), " ", quote(Output)],
            #{read => [Input], write => [Output]})),
        ?assertMatch({ok, <<137, "PNG", _/binary>>}, file:read_file(Output)),
        %% Test the PDF delegate without changing the root-owned ImageMagick
        %% policy (which may intentionally disable its PDF coder altogether).
        ?assertMatch({ok, _}, z_exec:run(imagemagick_pdf,
            ["gs -q -dSAFER -dBATCH -dNOPAUSE -sDEVICE=png16m -r72 -sOutputFile=",
             quote(Output), " ", quote(filename:absname(Pdf))],
            #{read => [Pdf], write => [Output]})),
        ?assertMatch({ok, <<137, "PNG", _/binary>>}, file:read_file(Output)),
        ?assertMatch({ok, _}, z_exec:run(ffmpeg,
            ["ffmpeg -nostdin -v error -f lavfi -i color=red:s=16x16:d=0.2 "
             "-c:v libx264 -pix_fmt yuv420p -movflags +faststart -y ", quote(Video)],
            #{write => [Video]})),
        ?assertMatch({ok, _}, z_exec:run(ffprobe,
            ["ffprobe -v error -show_format -of json ", quote(Video)], #{read => [Video]})),
        ?assertMatch({ok, _}, z_exec:run(ffmpeg,
            ["ffmpeg -nostdin -v error -i ", quote(Video), " -frames:v 1 -y ", quote(Output)],
            #{read => [Video], write => [Output]})),
        ?assertMatch({ok, _}, z_exec:run(imagemagick,
            [case Convert of "magick" -> "magick identify "; _ -> "identify " end, quote(Output)], #{read => [Output]}))
    after
        os:unsetenv("ZOTONIC_TEST_SECRET"),
        file:del_dir_r(Dir)
    end.

quote(Path) -> z_filelib:os_filename(Path).
