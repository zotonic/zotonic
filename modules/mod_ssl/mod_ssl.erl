%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2016 Marc Worrell, Maas-Maarten Zeeman
%%
%% @doc SSL pages for Zotonic.

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

-module(mod_ssl).

-mod_title("HTTPS / SSL").
-mod_description("Provides https/ssl connectivity.").
-mod_provides([ssl]).
-mod_depends([]).
-mod_prio(100).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    observe_ssl_options/2
]).

-include_lib("zotonic.hrl").


%% @doc Return the certificates of this site.
%% @todo check for keys in a directory for the hostname (optional)
%% @todo cache the result of these checks for 1 minute (or so)
observe_ssl_options(#ssl_options{server_name=_NormalizedHostnameBin}, Context) ->
    {ok, CertFiles} = cert_files(Context),
    CertFile = proplists:get_value(certfile, CertFiles),
    KeyFile = proplists:get_value(keyfile, CertFiles),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {false, false} ->
            undefined;
        {false, true} ->
            lager:info("[~p] mod_ssl: no ~p file (though there is a key file), skipping.",
                       [z_context:site(Context), CertFile]),
            undefined;
        {true, false} ->
            lager:info("[~p] mod_ssl: no ~p file (though there is a crt file), skipping.",
                       [z_context:site(Context), KeyFile]),
            undefined;
        {true, true} ->
            case check_keyfile(KeyFile, Context) of
                ok -> CertFiles;
                {error, _} -> undefined
            end
    end.

check_keyfile(KeyFile, Context) ->
    Site = z_context:site(Context),
    case file:read_file(KeyFile) of
        {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
            lager:error("[~p] Need RSA private key file. Use: `openssl rsa -in ssl/~p.key -out ssl/~p.pem`",
                        [Site, Site, Site]),
            {error, need_rsa_private_key};
        {ok, Bin} ->
            case public_key:pem_decode(Bin) of
                [] ->
                    lager:error("[~p] No private keys found in ~p", [Site, KeyFile]),
                    {error, no_private_keys_found};
                _ ->
                    ok
            end;
        {error, _} = Error ->
            lager:error("[~p] Cannot read key file ~p, error: ~p",
                        [Site, KeyFile, Error]),
            Error
    end.

cert_files(Context) ->
    SSLDir = filename:join(z_path:site_dir(Context), "ssl"),
    Sitename = z_convert:to_list(z_context:site(Context)),
    Files = [
        {certfile, filename:join(SSLDir, Sitename++".crt")},
        {keyfile, filename:join(SSLDir, Sitename++".pem")},
        {password, m_config:get_value(mod_ssl, password, "", Context)}
    ] ++ z_ssl_certs:dh_options(),
    CaCertFile = filename:join(SSLDir, Sitename++".ca.crt"),
    case filelib:is_file(CaCertFile) of
        false -> {ok, Files};
        true -> {ok, [{cacertfile, CaCertFile} | Files]}
    end.

