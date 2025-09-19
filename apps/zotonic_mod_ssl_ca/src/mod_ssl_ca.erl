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
-moduledoc("
See also

[mod\\_ssl\\_letsencrypt](/id/doc_module_mod_ssl_letsencrypt), [Port configurations](/id/doc_developerguide_configuration_port_ssl_configuration#ref-port-ssl-configuration)

The mod\\_ssl\\_ca module adds support for using SSL certificates bought from a Certificate Authority.

A free alternative to CA provided tickets is Let’s Encrypt, see [mod\\_ssl\\_letsencrypt](/id/doc_module_mod_ssl_letsencrypt).



Certificate and key files
-------------------------

The certificate and key files are placed into the site sub-directory of the security directory. The subdirectory will
be: `sitename/ca/`

Where *sitename* must be replaced with the name of your site.

The security directory can be found by inspecting the output of:


```erlang
bin/zotonic config
```

The Zotonic *security* directory can be in one of the following directories:

*   The environment variable `ZOTONIC_SECURITY_DIR`
*   The `~/.zotonic/security` directory
*   The `/etc/zotonic/security` directory (only on Linux)
*   The OS specific directory for application data files

The OS specific directories are:

*   On Unix: `~/.config/zotonic/security/`
*   On macOS: `~/Library/Application Support/zotonic/security/`

The default is the OS specific directory.

If there is a directory `priv/security/ca` inside your site’s OTP application folder then that directory will be used.

The filenames are checked against their extension. When you copy your files to the `ca` directory then you need to
ensure that they have the right extensions.

The following file extensions are expected:

`*.pem` or `*.key`

This holds the private key for the encryption. The key must be unlocked and in PKCS#1 format (see below).

`*.crt`

This is the certificate. Usually it is supplied by the certificate authority where you bought it. It can also be a self
signed certificate, see below.

`*.ca.crt`, `cabundle.crt` or `bundle.crt`

This is the (optional) *CA bundle* that contains root and intermediate certificates for the certificate authority that
issued the `.crt` certificate.

The certificate authority will supply these. All supplied certificates are concatenated, with the root certificate last.

The concatenation is a literal command, like:


```bash
cat intermediate.crt root.crt > cabundle.crt
```

Due to caching, it can take up to a minute before the new certificates are used.



Format of the private key
-------------------------

The Erlang SSL implementation accepts PKCS#1 and PKCS#8 format keys. OpenSSL generates (since 2010) PKCS#8 format keys.

A PKCS#1 key starts with:


```none
-----BEGIN RSA PRIVATE KEY-----
```

A PKCS#8 key starts with:


```none
-----BEGIN PRIVATE KEY-----
```

If there are problems then check if the `.key` or `.pem` file starts with one of the above strings.



Using SSL certificates
----------------------

If you order a SSL certificate, the signing authority will ask you which kind of web server you are using and a CSR
file. For the web server, select *other*. For the CSR, use the following command:


```bash
openssl req -out certificate.csr -new -newkey rsa:2048 -nodes -keyout certificate.key
```

When OpenSSL asks for the *Common Name* then fill in the site’s hostname (e.g. *www.example.com*).

From the SSL certificate authority you will receive a signed `.crt` file and maybe a `cabundle.crt` file.

See the section *Certificate and key files* above for instructions how to use the `.crt` and `.key` files.



Generating a self signed certificate
------------------------------------

There is no need to make your own self signed certificate as Zotonic will generate one for every site.

Nevertheless, if you want to use your own self signed certificate, then run the following commmands:


```bash
openssl req -x509 -nodes -days 3650 -subj '/CN=www.example.com' -newkey rsa:2048 \\
     -keyout certificate.key -out certificate.crt
```

This generates a private key of 2048 bits and a certificate that is valid for 10 years.
").

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
                    ?LOG_ERROR(#{
                        text => <<"No private PEM key found">>,
                        in => zotonic_mod_ssl_ca,
                        result => error,
                        reason => no_private_keys_found,
                        filename => PemFile
                    }),
                    {error, no_private_keys_found};
                _ ->
                    ok
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                text => <<"Cannot read key file">>,
                in => zotonic_mod_ssl_ca,
                result => error,
                reason => Reason,
                filename => PemFile
            }),
            Error
    end.
