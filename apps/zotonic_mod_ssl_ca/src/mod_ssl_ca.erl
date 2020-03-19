%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2020 Marc Worrell, Maas-Maarten Zeeman
%% @doc CA supplied certificate handling

%% Copyright 2012 - 2020 Marc Worrell, Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_ssl_ca).

-mod_title("SSL - CA").
-mod_description("Use SSL Certificate from a Certificiate Authority.").
-mod_provides([]).
-mod_depends([]).
-mod_prio(100).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_ssl_options/2,
    ssl_options/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SNI_CACHE_TIME, 60).

%% @doc Return the certificates of this site.
observe_ssl_options(#ssl_options{server_name=_NormalizedHostnameBin}, Context) ->
    z_depcache:memo(fun() -> ssl_options(Context) end, sni_ssl_ca, ?SNI_CACHE_TIME, Context).

ssl_options(Context) ->
    case cert_files(Context) of
        {ok, CertFiles} ->
            PemFile = proplists:get_value(keyfile, CertFiles),
            case check_keyfile(PemFile) of
                ok -> {ok, CertFiles};
                {error, _} -> undefined
            end;
        {error, _} ->
            undefined
    end.

cert_files(Context) ->
    SSLDir = cert_dir(Context),
    Files = scan_cert_dir(SSLDir),
    files_to_ssl_options(Files).

files_to_ssl_options(#{ bundle := CaCrt, crt := Crt, pem := Pem }) ->
    Options = [
        {certfile, Crt},
        {keyfile, Pem},
        {cacertfile, CaCrt}
    ] ++ z_ssl_dhfile:dh_options(),
    {ok, Options};
files_to_ssl_options(#{ crt := Crt, pem := Pem }) ->
    Options = [
        {certfile, Crt},
        {keyfile, Pem}
    ] ++ z_ssl_dhfile:dh_options(),
    {ok, Options};
files_to_ssl_options(#{ bundle := CaCrt, crt := Crt, key := Key }) ->
    Options = [
        {certfile, Crt},
        {keyfile, Key},
        {cacertfile, CaCrt}
    ] ++ z_ssl_dhfile:dh_options(),
    {ok, Options};
files_to_ssl_options(#{ crt := Crt, key := Key }) ->
    Options = [
        {certfile, Crt},
        {keyfile, Key}
    ] ++ z_ssl_dhfile:dh_options(),
    {ok, Options};
files_to_ssl_options(_Files) ->
    {error, notfound}.

cert_dir(Context) ->
    PrivSSLDir = filename:join([z_path:site_dir(Context), "priv", "security", "ca"]),
    case filelib:is_dir(PrivSSLDir) of
        true ->
            PrivSSLDir;
        false ->
            {ok, SecurityDir} = z_config_files:security_dir(),
            filename:join([ SecurityDir, z_context:site(Context), "ca" ])
    end.

scan_cert_dir( Dir ) ->
    Files = filelib:wildcard( filename:join( Dir, "*" ) ),
    lists:foldl(
        fun
            (F, Acc) ->
                case filelib:is_dir(F) of
                    true ->
                        Acc;
                    false ->
                        case filename:basename(F) of
                            "cabundle.crt" ->
                                Acc#{ bundle => F };
                            "bundle.crt" ->
                                Acc#{ bundle => F };
                            Basename ->
                                case filename:extension(Basename) of
                                    "" -> Acc;
                                    ".crt" ->
                                        case filename:basename(Basename, ".ca.crt") of
                                            Basename -> Acc#{ crt => F };
                                            _ -> Acc#{ bundle => F }
                                        end;
                                    ".key" ->
                                        case classify_key_file(F) of
                                            pem -> Acc#{ pem => F };
                                            key -> Acc#{ key => F };
                                            error -> Acc
                                        end;
                                    ".pem" ->
                                        Acc#{ pem => F };
                                    _Ext ->
                                        Acc
                                end
                        end
                end
        end,
        #{},
        Files).

classify_key_file(File) ->
    case file:read_file(File) of
        {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
            key;
        {ok, <<"-----BEGIN RSA PRIVATE KEY", _/binary>>} ->
            pem;
        _ ->
            error
    end.

check_keyfile(PemFile) ->
    case file:read_file(PemFile) of
        {ok, Bin} ->
            case public_key:pem_decode(Bin) of
                [] ->
                    lager:error("No private PEM key found in ~p", [PemFile]),
                    {error, no_private_keys_found};
                _ ->
                    ok
            end;
        {error, _} = Error ->
            lager:error("Cannot read key file ~p, error: ~p",
                        [PemFile, Error]),
            Error
    end.
