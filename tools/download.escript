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
    ok = httpc:set_options([ {https_proxy, {{Host, Port}, []}} ]).

download(Url) ->
    case httpc:request( get, {Url, []}, ssl_opts(), [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Bin}} ->
            Bin;
        {ok, {{_, Status, Reason}, _Headers, _Body}} ->
            halt_with("Download failed: HTTP ~p ~p~n", [Status, Reason]);
        Error ->
            halt_with("Download failed: ~p~n", [Error])
    end.

ssl_opts() ->
    MatchFun = public_key:pkix_verify_hostname_match_fun(https),
    [{ssl, [ {verify, verify_peer},
             {cacerts, public_key:cacerts_get()},
             {customize_hostname_check, [{match_fun, MatchFun}]}
           ]
     }
    ].

maybe_verify_sha256(Bin, undefined) ->
    io:fwrite(standard_error, "Note: no SHA256 configured, downloaded file has: ~s~n",
              [binary:encode_hex(crypto:hash(sha256, Bin), lowercase)]);
maybe_verify_sha256(Bin, ExpectedHex) ->
    case catch binary:decode_hex(list_to_binary(ExpectedHex)) of
        <<Expected:32/binary>> -> 
            case crypto:hash(sha256, Bin) of
                Expected -> ok;
                _ ->
                    halt_with("SHA256 mismatch: the downloaded file does not match the configured checksum.~n"
                              "Verify the source before updating the checksum.~n", [])
            end;
        _ ->
            halt_with("Invalid SHA256 value: ~s~n", [ExpectedHex])
    end.

maybe_make_executable(_File, false) -> ok;
maybe_make_executable(File, true) ->
    case file:change_mode(File, 8#755) of
        ok -> ok;
        Error ->
            halt_with("Failed to make ~s executable: ~p~n", [File, Error])
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
    "Usage:~n"
    "  download.escript URL OUTPUT [--sha256 HASH] [--executable] [--proxy HOST PORT]~n"
    "~n"
    "Examples:~n"
    "  download.escript https://example.com/file ./file~n"
    "~n"
    "  download.escript https://example.com/file ./file --sha256 abc123~n"
    "~n"
    "  download.escript https://example.com/file ./file --sha256 abc123 --executable~n"
    "~n"
    "  download.escript https://example.com/file ./file --proxy proxy.company.com 8080~n".

halt_with(Format, Args) ->
    io:fwrite(standard_error, Format, Args),
    halt(1).
