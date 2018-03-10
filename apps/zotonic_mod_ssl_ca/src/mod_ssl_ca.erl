%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2016 Marc Worrell, Maas-Maarten Zeeman
%%
%% @doc CA supplied certificate handling

%% Copyright 2012 - 2016 Marc Worrell, Maas-Maarten Zeeman
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
    observe_ssl_options/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SNI_CACHE_TIME, 60).

%% @doc Return the certificates of this site.
observe_ssl_options(#ssl_options{server_name=_NormalizedHostnameBin}, Context) ->
    z_depcache:memo(fun() -> ssl_options(Context) end, sni_ssl_ca, ?SNI_CACHE_TIME, Context).

ssl_options(Context) ->
    {ok, CertFiles} = cert_files(Context),
    CertFile = proplists:get_value(certfile, CertFiles),
    KeyFile = proplists:get_value(keyfile, CertFiles),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {false, false} ->
            lager:info("mod_ssl_ca: no ~p and ~p files, skipping.",
                       [CertFile, KeyFile]),
            undefined;
        {false, true} ->
            lager:info("mod_ssl_ca: no ~p file (though there is a key file), skipping.",
                       [CertFile]),
            undefined;
        {true, false} ->
            lager:info("mod_ssl_ca: no ~p file (though there is a crt file), skipping.",
                       [KeyFile]),
            undefined;
        {true, true} ->
            case check_keyfile(KeyFile, Context) of
                ok -> {ok, CertFiles};
                {error, _} -> undefined
            end
    end.

cert_files(Context) ->
    SSLDir = filename:join([z_path:site_dir(Context), "priv", "ssl", "ca"]),
    Sitename = z_convert:to_list(z_context:site(Context)),
    Files = [
        {certfile, filename:join(SSLDir, Sitename++".crt")},
        {keyfile, filename:join(SSLDir, Sitename++".pem")},
        {password, z_convert:to_list(m_config:get_value(mod_ssl, password, "", Context))}
    ] ++ z_ssl_certs:dh_options(),
    CaCertFile = filename:join(SSLDir, Sitename++".ca.crt"),
    case filelib:is_file(CaCertFile) of
        false -> {ok, Files};
        true -> {ok, [{cacertfile, CaCertFile} | Files]}
    end.

check_keyfile(KeyFile, Context) ->
    Site = z_context:site(Context),
    case file:read_file(KeyFile) of
        {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
            lager:error("Need RSA private key file. Use: `openssl rsa -in ssl/ca/~p.key -out ssl/ca/~p.pem`",
                        [Site, Site]),
            {error, need_rsa_private_key};
        {ok, Bin} ->
            case public_key:pem_decode(Bin) of
                [] ->
                    lager:error("No private keys found in ~p", [KeyFile]),
                    {error, no_private_keys_found};
                _ ->
                    ok
            end;
        {error, _} = Error ->
            lager:error("Cannot read key file ~p, error: ~p",
                        [KeyFile, Error]),
            Error
    end.
