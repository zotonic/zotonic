#!/usr/bin/env escript
%%! -noshell

%% download.escript
%%
%% Small self-contained downloader implemented entirely in Erlang.
%%
%% Features:
%%   * Downloads a file over HTTPS using the OTP ssl and inets
%%     applications.
%%   * Verifies the server certificate and hostname.
%%   * Optionally verifies the downloaded file against a supplied
%%     SHA-256 checksum.
%%   * Optionally marks the downloaded file as executable.
%%
%% The goal is to provide a secure bootstrap mechanism without relying
%% on external tools such as curl, wget, sha256sum, or chmod. Any
%% system capable of running Erlang/OTP can use this script.
%%
%% The file is downloaded into memory, verified, and only written to
%% disk after all checks have passed.

main(Args) ->
    case parse_args(Args) of
        {ok, #{url := Url,
               output := Output,
               sha256 := Sha256,
               executable := Executable}} ->
            ok = start_apps(),

            Bin = download(Url),

            ok = maybe_verify_sha256(Bin, Sha256),
            ok = file:write_file(Output, Bin),
            ok = maybe_make_executable(Output, Executable);

        error ->
            usage(),
            halt(1)
    end.

start_apps() ->
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    ok.

download(Url) ->
    case httpc:request(
           get,
           {Url, []},
           [{ssl, [{verify, verify_peer},
                   {cacerts, public_key:cacerts_get()},
                   {customize_hostname_check, [
                       {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                   ]}
                  ]
            }],
           [{body_format, binary}])
    of
        {ok, {{_, 200, _}, _Headers, Bin}} ->
            Bin;
        {ok, {{_, Status, Reason}, _Headers, _Body}} ->
            io:fwrite(standard_error, "Download failed: HTTP ~p ~s~n", [Status, Reason]),
            halt(1);
        Error ->
            io:fwrite(standard_error, "Download failed: ~p~n", [Error]),
            halt(1)
    end.

maybe_verify_sha256(_Bin, undefined) ->
    ok;

maybe_verify_sha256(Bin, ExpectedHex) ->
    Actual =
        crypto:hash(sha256, Bin),

    Expected =
        binary:decode_hex(
          unicode:characters_to_binary(ExpectedHex)),

    case Actual of
        Expected ->
            ok;
        _ ->
            io:fwrite(standard_error, "SHA256 verification failed~n", []),
            halt(1)
    end.

maybe_make_executable(_File, false) ->
    ok;

maybe_make_executable(File, true) ->
    case file:change_mode(File, 8#755) of
        ok ->
            ok;
        Error ->
            io:fwrite(standard_error, "Failed to make ~s executable: ~p~n", [File, Error]),
            halt(1)
    end.

parse_args([Url, Output | Rest]) ->
    parse_flags(Rest,
                #{url => Url,
                  output => Output,
                  sha256 => undefined,
                  executable => false});
parse_args(_) ->
    error.

parse_flags([], Config) ->
    {ok, Config};

parse_flags(["--sha256", Hash | Rest], Config) ->
    parse_flags(Rest, Config#{sha256 => Hash});

parse_flags(["--executable" | Rest], Config) ->
    parse_flags(Rest, Config#{executable => true});

parse_flags(_, _) ->
    error.

usage() ->
    io:format(
      "Usage:~n"
      "  download.escript URL OUTPUT [--sha256 HASH] [--executable]~n"
      "~n"
      "Examples:~n"
      "  download.escript https://example.com/file ./file~n"
      "~n"
      "  download.escript https://example.com/file ./file "
      "--sha256 abc123~n"
      "~n"
      "  download.escript https://example.com/file ./file "
      "--sha256 abc123 --executable~n").
