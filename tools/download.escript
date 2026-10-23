#!/usr/bin/env escript
%%! -noshell

%%--------------------------------------------------------------------
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
%%   * Optionally uses an HTTPS proxy.
%%
%% The goal is to provide a secure bootstrap mechanism without relying
%% on external tools such as curl, wget, sha256sum, or chmod. Any
%% system capable of running Erlang/OTP can use this script.
%%
%% The file is downloaded into memory, verified, and only written to
%% disk after all checks have passed.
%%
%% Usage:
%%
%%   download.escript URL OUTPUT
%%   download.escript URL OUTPUT --sha256 HASH
%%   download.escript URL OUTPUT --sha256 HASH --executable
%%   download.escript URL OUTPUT --proxy HOST PORT
%%
%%--------------------------------------------------------------------

main(Args) ->
    case parse_args(Args) of
        {ok, #{url := Url,
               output := Output,
               sha256 := Sha256,
               executable := Executable,
               proxy := Proxy}} ->
            ok = start_apps(),
            ok = maybe_set_proxy(Proxy),

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

maybe_set_proxy(undefined) ->
    ok;

maybe_set_proxy({Host, Port}) ->
    ok = httpc:set_options([
        {https_proxy, {{Host, Port}, []}}
    ]).

download(Url) ->
    case httpc:request(
           get,
           {Url, []},
           ssl_opts(),
           [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Bin}} ->
            Bin;
        {ok, {{_, Status, Reason}, _Headers, _Body}} ->
            io:fwrite(standard_error, "Download failed: HTTP ~p ~p~n", [Status, Reason]),
            halt(1);
        Error ->
            io:fwrite(standard_error, "Download failed: ~p~n", [Error]),
            halt(1)
    end.

ssl_opts() ->
    [{ssl,
      [{verify, verify_peer},
       {cacerts, public_key:cacerts_get()},
       {customize_hostname_check,
        [{match_fun,
          public_key:pkix_verify_hostname_match_fun(https)}]}]}].

maybe_verify_sha256(_Bin, undefined) ->
    ok;

maybe_verify_sha256(Bin, ExpectedHex) ->
    try
        Expected =
            binary:decode_hex(
              unicode:characters_to_binary(ExpectedHex)),

        Actual = crypto:hash(sha256, Bin),

        case Actual of
            Expected ->
                ok;
            _ ->
                io:fwrite(standard_error, "SHA256 verification failed~n", []),
                halt(1)
        end
    catch
        _:_ ->
            io:fwrite(standard_error, "Invalid SHA256 value: ~s~n", [ExpectedHex]),
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
    parse_flags(
      Rest,
      #{url => Url,
        output => Output,
        sha256 => undefined,
        executable => false,
        proxy => undefined});

parse_args(_) ->
    error.

parse_flags([], Config) ->
    {ok, Config};

parse_flags(["--sha256", Hash | Rest], Config) ->
    parse_flags(Rest, Config#{sha256 => Hash});

parse_flags(["--executable" | Rest], Config) ->
    parse_flags(Rest, Config#{executable => true});

parse_flags(["--proxy", Host, Port | Rest], Config) ->
    parse_flags(Rest, Config#{proxy => {Host, list_to_integer(Port)}});

parse_flags(_, _) ->
    error.

usage() ->
    io:fwrite(
      "Usage:~n"
      "  download.escript URL OUTPUT "
      "[--sha256 HASH] "
      "[--executable] "
      "[--proxy HOST PORT]~n"
      "~n"
      "Examples:~n"
      "  download.escript https://example.com/file ./file~n"
      "~n"
      "  download.escript https://example.com/file ./file "
      "--sha256 abc123~n"
      "~n"
      "  download.escript https://example.com/file ./file "
      "--sha256 abc123 --executable~n"
      "~n"
      "  download.escript https://example.com/file ./file "
      "--proxy proxy.company.com 8080~n").
