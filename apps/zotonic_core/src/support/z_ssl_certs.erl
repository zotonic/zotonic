%% @author Marc Worrell <marc@worrell.nl>
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012-2016  Marc Worrell, Maas-Maarten Zeeman
%% @doc SSL support functions, create self-signed certificates

%% Copyright 2012-2016 Marc Worrell, Maas-Maarten Zeeman
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

-module(z_ssl_certs).
-author('Marc Worrell <marc@worrell.nl>').
-author('Maas-Maarten Zeeman <mmzeeman@xs4all.nl>').

-export([
    ssl_listener_options/0,
    sni_fun/1,
    dh_options/0,
    is_dhfile/1,
    ensure_dhfile/0,

    sign/2,
    get_ssl_options/1,
    get_ssl_options/2,

    ciphers/0,

    sni_self_signed/1,
    ensure_self_signed/2,
    decode_cert/1
]).

-include_lib("zotonic.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(BITS, "4096").
-define(DHBITS, "2048").

%% @doc Return the options to use for non-sni ssl
%% @todo should we use the hostname as configured in the OS for the certs?
-spec ssl_listener_options() -> list(ssl:ssl_option()).
ssl_listener_options() ->
    {ok, CertOptions} = ensure_self_signed(zotonic_site_status),
    [
        {sni_fun, fun ?MODULE:sni_fun/1},
        {ciphers, ciphers()}
    ]
    ++ CertOptions
    ++ dh_options().

%% @doc Callback for SSL SNI, match the hostname to a set of keys
-spec sni_fun(string()) -> [ssl:ssl_option()] | undefined.
sni_fun(Hostname) ->
    NormalizedHostname = normalize_hostname(Hostname),
    NormalizedHostnameBin = z_convert:to_binary(NormalizedHostname),
    case z_sites_dispatcher:get_site_for_hostname(NormalizedHostnameBin) of
        undefined ->
            %% @todo Serve the correct cert for sites that are down or disabled
            sni_self_signed(zotonic_site_status, z_context:new(zotonic_site_status));
        {ok, Site} ->
            get_ssl_options(NormalizedHostnameBin, z_context:new(Site))
    end.

-spec sni_self_signed(z:context()) -> list(ssl:ssl_option()) | undefined.
sni_self_signed(Context) ->
    sni_self_signed(z_context:site(Context), Context).

sni_self_signed(Site, Context) ->
    {ok, SSLOptions} = get_self_signed_files(Site),
    CertFile = proplists:get_value(certfile, SSLOptions),
    KeyFile = proplists:get_value(keyfile, SSLOptions),
    case filelib:is_file(CertFile) andalso filelib:is_file(KeyFile) of
        true ->
            SSLOptions;
        false ->
            case ensure_self_signed(Site, Context) of
                {ok, SSLOptionsNew} -> SSLOptionsNew;
                {error, _} -> undefined
            end
    end.

%% @doc Sign data using the current private key and sha256
%% @todo If needed more often then cache the decoded private key.
-spec sign(iolist(), z:context()) -> {ok, binary()} | {error, term()}.
sign(Data, Context) ->
    Hostname = z_context:hostname(Context),
    case get_ssl_options(Hostname, Context) of
        SSLOptions when is_list(SSLOptions) ->
            KeyFile = proplists:get_value(keyfile, SSLOptions),
            {ok, PemKeyBin} = file:read_file(KeyFile),
            [PemKeyData] = public_key:pem_decode(PemKeyBin),
            PemKey = public_key:pem_entry_decode(PemKeyData),
            {ok, public_key:sign(Data, sha256, PemKey)};
        undefined ->
            {error, nocerts}
    end.

%% @doc Fetch the ssi options for the site context.
-spec get_ssl_options(z:context()) -> list(ssl:ssl_option()) | undefined.
get_ssl_options(Context) ->
    get_ssl_options(z_context:hostname(Context), Context).

%% @doc Fetch the ssl options for the given hostname and site context. If there is
%%      is no module observing ssl_options, then return the self signed certificates.
-spec get_ssl_options(binary(), z:context()) -> list(ssl:ssl_option()) | undefined.
get_ssl_options(Hostname, Context) ->
    case z_notifier:first(#ssl_options{server_name=Hostname}, Context) of
        {ok, SSLOptions} ->
            SSLOptions;
        undefined ->
            sni_self_signed(z_context:site(Context), Context)
    end.


%% @doc Fetch the paths of all self-signed certificates
-spec get_self_signed_files(Site :: atom()) -> {ok, list(ssl:ssl_option())}.
get_self_signed_files(Site) ->
    SSLDir = filename:join([z_sites_manager:get_site_dir(Site), "priv", "ssl", "self-signed"]),
    Options = [
        {certfile, filename:join(SSLDir, "self-signed"++?BITS++".crt")},
        {keyfile, filename:join(SSLDir, "self-signed"++?BITS++".pem")}
    ],
    {ok, Options}.

normalize_hostname(Hostname) when is_list(Hostname) ->
    [ to_lower(C) || C <- Hostname, is_valid_hostname_char(C) ].

to_lower(C) when C >= $A, C =< $Z -> C + 32;
to_lower(C) -> C.

is_valid_hostname_char($.) -> true;
is_valid_hostname_char(C) when C >= $a, C =< $z -> true;
is_valid_hostname_char(C) when C >= $A, C =< $Z -> true;
is_valid_hostname_char(C) when C >= $0, C =< $9 -> true;
is_valid_hostname_char($-) -> true;
is_valid_hostname_char(_) -> false.


%% @doc Check if all certificates are available in the site's ssl directory
%% @todo Disentangle use of the fallback site and zotonic_site_status
-spec ensure_self_signed(atom()) ->  {ok, list()} | {error, term()}.
ensure_self_signed(Site) ->
    ensure_self_signed(Site, undefined).

-spec ensure_self_signed(atom(), #context{}|undefined) ->  {ok, list()} | {error, term()}.
ensure_self_signed(Site, Context) ->
    {ok, Certs} = get_self_signed_files(Site),
    CertFile = proplists:get_value(certfile, Certs),
    KeyFile = proplists:get_value(keyfile, Certs),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {false, false} ->
            generate_self_signed(site_hostname(Context), Certs);
        {false, true} ->
            lager:error("Missing cert file ~p, regenerating keys", [CertFile]),
            generate_self_signed(site_hostname(Context), Certs);
        {true, false} ->
            lager:error("Missing pem file ~p, regenerating keys", [KeyFile]),
            generate_self_signed(site_hostname(Context), Certs);
        {true, true} ->
            case check_keyfile(KeyFile) of
                ok -> {ok, Certs};
                {error, _}=E -> E
            end
    end.

site_hostname(undefined) ->
    {ok, LocalHostname} = inet:gethostname(),
    z_convert:to_binary(LocalHostname);
site_hostname(Context) ->
    case z_context:hostname(Context) of
        None when None =:= none; None =:= <<"none">> ->
            {ok, LocalHostname} = inet:gethostname(),
            z_convert:to_binary(LocalHostname);
        ConfiguredHostname ->
            ConfiguredHostname
    end.

check_keyfile(Filename) ->
    case file:read_file(Filename) of
        {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
            {error, {need_rsa_private_key, Filename, "use: openssl rsa -in sitename.key -out sitename.pem"}};
        {ok, Bin} ->
            case public_key:pem_decode(Bin) of
                [] -> {error, {no_private_keys_found, Filename}};
                _ -> ok
            end;
        {error, _} = Error ->
            {error, {cannot_read_pemfile, Filename, Error}}
    end.

-spec generate_self_signed(string()|binary(), list()) -> {ok, list()} | {error, term()}.
generate_self_signed(NormalizedHostname, Opts) ->
    PemFile = proplists:get_value(keyfile, Opts),
    lager:info("Generating self-signed ssl keys for ~s in ~s", [NormalizedHostname, PemFile]),
    case z_filelib:ensure_dir(PemFile) of
        ok ->
            KeyFile = filename:rootname(PemFile) ++ ".key",
            CertFile = proplists:get_value(certfile, Opts),
            Command = "openssl req -x509 -nodes"
                    ++ " -days 3650"
                    ++ " -sha256"
                    ++ " -subj '/CN="++z_convert:to_list(NormalizedHostname)++"'"
                    ++ " -newkey rsa:"++?BITS++" "
                    ++ " -keyout "++z_utils:os_filename(KeyFile)
                    ++ " -out "++z_utils:os_filename(CertFile),
            lager:debug("SSL: ~p", [Command]),
            Result = os:cmd(Command),
            lager:debug("SSL: ~p", [Result]),
            case file:read_file(KeyFile) of
                {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
                    os:cmd("openssl rsa -in "++KeyFile++" -out "++PemFile),
                    {ok, Opts};
                {ok, <<"-----BEGIN RSA PRIVATE KEY", _/binary>>} ->
                    file:rename(KeyFile, PemFile),
                    {ok, Opts};
                _Error ->
                    lager:error("Failed generating self-signed ssl keys for ~s in ~s (output was ~s)",
                                [NormalizedHostname, PemFile, Result]),
                    {error, openssl}
            end;
        {error, _} = Error ->
            {error, {ensure_dir, Error, PemFile}}
    end.

%% @doc Return the dh key to be used. Needed for better forward secrecy with the DH key exchange
-spec dh_options() -> [ssl:ssl_option()].
dh_options() ->
    [{dhfile, dhfile()}].

is_dhfile(Filename) ->
    case file:read_file(Filename) of
        {ok, <<"-----BEGIN DH PARAMETERS", _/binary>>} -> true;
        _ -> false
    end.

ensure_dhfile() ->
    ensure_dhfile(dhfile()).

dhfile() ->
    case z_config:get(ssl_dhfile) of
        undefined -> filename:join([z_utils:lib_dir(priv), "ssl", "dh"++?DHBITS++".pem"]);
        Filename -> Filename
    end.

ensure_dhfile(Filename) ->
    case filelib:is_file(Filename) of
        true ->
            ok;
        false ->
            ok = z_filelib:ensure_dir(Filename),
            % Maybe use the -dsaparam, maybe we shouldn't https://www.openssl.org/news/secadv/20160128.txt
            % Though without this the generation will take a long time indeed...
            lager:info("Generating ~s bits DH key, this will take a long time (saving to ~p)",
                       [?DHBITS, Filename]),
            Command = "openssl dhparam -out " ++ z_utils:os_filename(Filename) ++ " " ++ ?DHBITS,
            lager:debug("SSL: ~p", [Command]),
            Result = os:cmd(Command),
            lager:debug("SSL: ~p", [Result]),
            case is_dhfile(Filename) of
                true -> ok;
                false ->
                    lager:error("Failed generating DH file in ~p (output was ~p)",
                                [Filename, Result]),
                    {error, openssl_dhfile}
            end
    end.


%% @todo reorder cipher list? See: https://sparanoid.com/note/http2-and-ecdsa-cipher-suites/
%% ECDHE-RSA-AES128-GCM-SHA256 and ECDHE-ECDSA-AES128-GCM-SHA256 should be at the top.
%% Otherwise Chrome will give ERR_SPDY_INADEQUATE_TRANSPORT_SECURITY
%% There is a problem with Firefox, which *needs* a cipher suite not implemented by Erlang
%% https://github.com/tatsuhiro-t/lucid/blob/ce8654a75108c15cc786424b3faf1a8e945bfd53/README.rst#current-status
ciphers() ->
    ssl:cipher_suites().


%% @doc Decode a certificate file, return common_name, not_after etc.
-spec decode_cert(filename:filename()) -> list().
decode_cert(CertFile) ->
    {ok, CertData} = file:read_file(CertFile),
    PemEntries = public_key:pem_decode(CertData),
    case public_key:pem_entry_decode(hd(PemEntries)) of
        {'Certificate', #'TBSCertificate'{} = TBS, _, _} ->
            #'Validity'{notAfter = NotAfter} = TBS#'TBSCertificate'.validity,
            Subject = decode_subject(TBS#'TBSCertificate'.subject),
            SANs = decode_sans(TBS#'TBSCertificate'.extensions),
            {ok, [
                {not_after, decode_time(NotAfter)},
                {common_name, proplists:get_value(cn, Subject)},
                {subject_alt_names, SANs}
            ]};
        _ ->
            {error, not_a_certificate}
    end.

decode_time({utcTime, [Y1,Y2,_M1,_M2,_D1,_D2,_H1,_H2,_M3,_M4,_S1,_S2,$Z] = T}) ->
    case list_to_integer([Y1,Y2]) of
        N when N >= 50 ->
            decode_time({generalTime, [$1,$9|T]});
        _ ->
            decode_time({generalTime, [$2,$0|T]})
    end;
decode_time({_,[Y1,Y2,Y3,Y4,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,$Z]}) ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    {{Year, Month, Day}, {Hour, Min, Sec}}.

decode_subject({rdnSequence, _} = R) ->
    {rdnSequence, List} = pubkey_cert_records:transform(R, decode),
    lists:foldl(
            fun
                (#'AttributeTypeAndValue'{type=?'id-at-commonName', value=CN}, Acc) ->
                    [{cn, decode_value(CN)}|Acc];
                (_, Acc) ->
                    Acc
            end,
            [],
            lists:flatten(List)).

decode_sans([]) ->
    [];
decode_sans([#'Extension'{extnID=?'id-ce-subjectAltName', extnValue=V} | _]) ->
    case 'OTP-PUB-KEY':decode('SubjectAltName', iolist_to_binary(V)) of
        {ok, Vs} -> lists:map(fun decode_value/1, Vs);
        _ -> []
    end;
decode_sans([_|Exts]) ->
    decode_sans(Exts).

decode_value({dNSName, Name}) -> iolist_to_binary(Name);
decode_value({printableString, P}) -> iolist_to_binary(P);
decode_value({utf8String, B}) -> B.

