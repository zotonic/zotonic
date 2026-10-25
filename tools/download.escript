#!/usr/bin/env escript
%%! -noshell

%% download.escript
%%
%% This script can download and verify  a file with Erlang.
%%
%% Functions:
%%   * Download a file over HTTPS with the ssl and inets
%%     applications.
%%   * Check the server certificate and hostname.
%%   * Check the file against an optional SHA-256 checksum.
%%   * Make the file executable when requested.
%%   * Use an HTTPS proxy when requested.
%%
%% This script uses Erlang/OTP. It does not need 
%% tools such as curl, wget, sha256sum, or chmod.
%%
%% The script stores the downloaded file in memory.
%% It checks the file before it writes the file to disk.
%%
%% Usage:
%%
%%   download.escript URL OUTPUT
%%   download.escript URL OUTPUT --sha256 HASH
%%   download.escript URL OUTPUT --sha256 HASH --executable
%%   download.escript URL OUTPUT --proxy HOST PORT
%%

-define(MAX_HEADER_SIZE, 64 * 1024).
-define(MAX_BODY_SIZE, 64 * 1024 * 1024).
-define(HTTP_TIMEOUT, 120000).

main(Args) ->
    logger:set_primary_config(level, warning),
    case parse_args(Args) of
        {ok, #{url := Url,
               output := Output,
               sha256 := Sha256,
               executable := Executable,
               proxy := Proxy}} ->
            {ok, _} = application:ensure_all_started([ssl, inets]),

            ok = maybe_set_proxy(Proxy),

            Bin = download(Url),

            ok = maybe_verify_sha256(Bin, Sha256),
            ok = file:write_file(Output, Bin),
            ok = maybe_make_executable(Output, Executable);
        error ->
            halt_with(usage(), [])
    end.

maybe_set_proxy(undefined) ->
    ok;
maybe_set_proxy({Host, Port}) ->
    ok = httpc:set_options([{https_proxy, {{Host, Port}, []}}]).

download(Url) ->
    case httpc:request(get, {Url, []}, http_opts(), opts()) of
        {ok, {{_, 200, _}, _Headers, Bin}} ->
            Bin;
        {ok, {{_, Status, Reason}, _Headers, _Body}} ->
            halt_with("Download failed. HTTP status: ~p. Reason: ~p.~n",
                      [Status, Reason]);
        Error ->
            halt_with("Download failed. Error: ~p.~n", [Error])
    end.

http_opts() ->
    [{timeout, ?HTTP_TIMEOUT},
     {ssl, ssl_opts()},
     {relaxed, true}].

ssl_opts() ->
    MatchFun = public_key:pkix_verify_hostname_match_fun(https),
    [{verify, verify_peer},
     {cacerts, public_key:cacerts_get()},
     {customize_hostname_check, [{match_fun, MatchFun}]}].

opts() ->
    %% GitHub can send a very large Content-Security-Policy header.
    [{max_header_size, ?MAX_HEADER_SIZE},
     {max_body_size, ?MAX_BODY_SIZE},
     {body_format, binary}].

maybe_verify_sha256(Bin, undefined) ->
    io:fwrite(standard_error,
              "Warning: No SHA-256 checksum was supplied.~n"
              "SHA-256 value of the downloaded file: ~s~n",
              [binary:encode_hex(crypto:hash(sha256, Bin), lowercase)]);
maybe_verify_sha256(Bin, ExpectedHex) ->
    case catch binary:decode_hex(list_to_binary(ExpectedHex)) of
        <<Expected:32/binary>> ->
            case crypto:hash(sha256, Bin) of
                Expected ->
                    ok;
                _ ->
                    halt_with(
                      "SHA-256 check failed. The file does not match the expected checksum.~n"
                      "Check the file source before you change the expected checksum.~n",
                      [])
            end;
        _ ->
            halt_with(
              "Invalid SHA-256 checksum: ~s~n"
              "Enter a valid 64-character hexadecimal value.~n",
              [ExpectedHex])
    end.

maybe_make_executable(_File, false) ->
    ok;
maybe_make_executable(File, true) ->
    case file:change_mode(File, 8#755) of
        ok ->
            ok;
        Error ->
            halt_with("Cannot set file as executable: ~s. Error: ~p.~n",
                      [File, Error])
    end.

parse_args([Url, Output | Rest]) ->
    parse_flags(Rest, #{url => Url,
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
    "Usage:\n"
    "  download.escript URL OUTPUT [--sha256 HASH] "
    "[--executable] [--proxy HOST PORT]\n"
    "\n"
    "Arguments:\n"
    "  URL       Address of the file to download.\n"
    "  OUTPUT    Path of the output file.\n"
    "\n"
    "Options:\n"
    "  --sha256 HASH    Check the file against this SHA-256 value.\n"
    "  --executable     Set the output file as executable.\n"
    "  --proxy HOST PORT\n"
    "                   Send the request through this HTTPS proxy.\n"
    "\n"
    "Examples:\n"
    "  download.escript https://example.com/file ./file\n"
    "\n"
    "  download.escript https://example.com/file ./file "
    "--sha256 HASH\n"
    "\n"
    "  download.escript https://example.com/file ./file "
    "--sha256 HASH --executable\n"
    "\n"
    "  download.escript https://example.com/file ./file "
    "--proxy proxy.company.com 8080\n".

halt_with(Format, Args) ->
    io:fwrite(standard_error, Format, Args),
    halt(1).
