%% @author Marc Worrell <marc@worrell.nl>
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012-2019 Marc Worrell, Maas-Maarten Zeeman
%% @doc SSL support functions, create self-signed certificates

%% Copyright 2012-2019 Marc Worrell, Maas-Maarten Zeeman
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

    sign/2,
    get_ssl_options/1,
    get_ssl_options/2,

    ciphers/0,

    sni_self_signed/1,
    ensure_self_signed/1,
    decode_cert/1
]).

-include_lib("zotonic.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(BITS, "4096").

%% @doc Return the options to use for non-sni ssl
%% @todo should we use the hostname as configured in the OS for the certs?
-spec ssl_listener_options() -> list(ssl:ssl_option()).
ssl_listener_options() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, CertOptions} = ensure_self_signed(Hostname),
    [
        {versions, ['tlsv1.2', 'tlsv1.1', 'tlsv1']},
        {sni_fun, fun ?MODULE:sni_fun/1},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {ciphers, ciphers()}
    ]
    ++ CertOptions
    ++ z_ssl_dhfile:dh_options().

%% @doc Callback for SSL SNI, match the hostname to a set of keys
-spec sni_fun(string()) -> [ ssl:ssl_option() ] | undefined.
sni_fun(Hostname) ->
    NormalizedHostname = normalize_hostname(Hostname),
    NormalizedHostnameBin = z_convert:to_binary(NormalizedHostname),
    case get_site_for_hostname(NormalizedHostnameBin) of
        {ok, Site} ->
            get_ssl_options(NormalizedHostnameBin, z_context:new(Site));
        {error, nosite} ->
            %% @todo Serve the correct cert for sites that are down or disabled
            {ok, Hostname} = inet:gethostname(),
            sni_self_signed(Hostname)
    end.

-spec sni_self_signed( string() | binary() ) -> list( ssl:ssl_option() ) | undefined.
sni_self_signed(Hostname) ->
    HostnameS = z_convert:to_list(Hostname),
    case get_self_signed_files(HostnameS) of
        {ok, SSLOptions} ->
            CertFile = proplists:get_value(certfile, SSLOptions),
            KeyFile = proplists:get_value(keyfile, SSLOptions),
            case filelib:is_file(CertFile) andalso filelib:is_file(KeyFile) of
                true ->
                    SSLOptions;
                false ->
                    case ensure_self_signed(HostnameS) of
                        {ok, SSLOptionsNew} -> SSLOptionsNew;
                        {error, _} -> undefined
                    end
            end;
        {error, no_security_dir} ->
            undefined
    end.

%% @doc Sign data using the current private key and sha256
%% @todo If needed more often then cache the decoded private key.
-spec sign(iodata(), z:context()) -> {ok, binary()} | {error, term()}.
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

%% @doc Find the site or fallback site that will handle the request
-spec get_site_for_hostname( binary() ) -> {ok, atom()} | {error, nosite}.
get_site_for_hostname(Hostname) ->
    case z_sites_dispatcher:get_site_for_hostname(Hostname) of
        {ok, Site} ->
            case z_sites_manager:wait_for_running(Site) of
                ok -> {ok, Site};
                {error, _} -> {error, nosite}
            end;
        undefined ->
            {error, nosite}
    end.

%% @doc Fetch the ssi options for the site context.
-spec get_ssl_options( z:context() | undefined ) -> list( ssl:ssl_option() ) | undefined.
get_ssl_options(Context) ->
    get_ssl_options(site_hostname(Context), Context).

%% @doc Fetch the ssl options for the given hostname and site context. If there is
%%      is no module observing ssl_options, then return the self signed certificates.
-spec get_ssl_options( binary(), z:context() ) -> list( ssl:ssl_option() ) | undefined.
get_ssl_options(Hostname, Context) when is_binary(Hostname) ->
    case z_notifier:first(#ssl_options{ server_name=Hostname }, Context) of
        {ok, SSLOptions} ->
            SSLOptions;
        undefined ->
            sni_self_signed( z_convert:to_list(Hostname) )
    end.


-spec get_self_signed_files( string() ) -> {ok, list( ssl:ssl_option() )} | {error, no_security_dir}.
get_self_signed_files(Hostname) ->
    case z_config_files:security_dir() of
        {ok, SecurityDir} ->
            SSLDir = filename:join(SecurityDir, "self-signed"),
            get_self_signed_files(Hostname, SSLDir);
        {error, _} ->
            {error, no_security_dir}
    end.

%% @doc Fetch the paths of all self-signed certificates
-spec get_self_signed_files( string(), file:filename_all() ) -> {ok, list( ssl:ssl_option() )}.
get_self_signed_files( Hostname, SSLDir ) ->
    Options = [
        {certfile, filename:join(SSLDir, Hostname ++ "-self-signed" ++ ?BITS ++ ".crt")},
        {keyfile, filename:join(SSLDir, Hostname ++ "-self-signed" ++ ?BITS ++".pem")}
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
-spec ensure_self_signed( string() ) ->  {ok, list( ssl:ssl_option() )} | {error, term()}.
ensure_self_signed(Hostname) ->
    {ok, Certs} = get_self_signed_files(Hostname),
    CertFile = proplists:get_value(certfile, Certs),
    KeyFile = proplists:get_value(keyfile, Certs),
    case {filelib:is_file(CertFile), filelib:is_file(KeyFile)} of
        {false, false} ->
            generate_self_signed(Hostname, Certs);
        {false, true} ->
            lager:error("Missing cert file ~p, regenerating keys", [CertFile]),
            generate_self_signed(Hostname, Certs);
        {true, false} ->
            lager:error("Missing pem file ~p, regenerating keys", [KeyFile]),
            generate_self_signed(Hostname, Certs);
        {true, true} ->
            case check_keyfile(KeyFile) of
                ok -> {ok, Certs};
                {error, _} = E -> E
            end
    end.

-spec site_hostname(z:context() | undefined) -> binary().
site_hostname(undefined) ->
    {ok, LocalHostname} = inet:gethostname(),
    z_convert:to_binary(LocalHostname);
site_hostname(Context) ->
    case z_context:hostname(Context) of
        <<"none">> ->
            site_hostname(undefined);
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

-spec generate_self_signed( string(), proplists:proplist() ) -> {ok, list()} | {error, term()}.
generate_self_signed(Hostname, Opts) ->
    PemFile = proplists:get_value(keyfile, Opts),
    lager:info("Generating self-signed ssl keys in '~s'", [PemFile]),
    case z_filelib:ensure_dir(PemFile) of
        ok ->
            _ = file:change_mode(filename:dirname(PemFile), 8#00700),
            KeyFile = filename:rootname(PemFile) ++ ".key",
            CertFile = proplists:get_value(certfile, Opts),
            ServerName = z_convert:to_list( server_name() ),
            Command = "openssl req -x509 -nodes"
                    ++ " -days 3650"
                    ++ " -sha256"
                    ++ " -subj '/CN="++Hostname
                             ++"/O="++ServerName
                             ++"'"
                    ++ " -newkey rsa:"++?BITS++" "
                    ++ " -keyout "++z_utils:os_filename(KeyFile)
                    ++ " -out "++z_utils:os_filename(CertFile),
            lager:debug("SSL: ~p", [Command]),
            Result = os:cmd(Command),
            lager:debug("SSL: ~p", [Result]),
            case file:read_file(KeyFile) of
                {ok, <<"-----BEGIN PRIVATE KEY", _/binary>>} ->
                    os:cmd("openssl rsa -in "++KeyFile++" -out "++PemFile),
                    _ = file:change_mode(KeyFile, 8#00600),
                    _ = file:change_mode(PemFile, 8#00600),
                    _ = file:change_mode(CertFile, 8#00644),
                    lager:info("SSL: Generated SSL self-signed certificate in '~s'", [KeyFile]),
                    {ok, Opts};
                {ok, <<"-----BEGIN RSA PRIVATE KEY", _/binary>>} ->
                    file:rename(KeyFile, PemFile),
                    _ = file:change_mode(PemFile, 8#00600),
                    _ = file:change_mode(CertFile, 8#00644),
                    lager:info("SSL: Generated SSL self-signed certificate in '~s'", [KeyFile]),
                    {ok, Opts};
                _Error ->
                    lager:error("SSL: Failed generating self-signed ssl keys in '~s' (output was ~s)",
                                [PemFile, Result]),
                    {error, openssl}
            end;
        {error, _} = Error ->
            {error, {ensure_dir, Error, PemFile}}
    end.

-spec server_name() -> binary().
server_name() ->
    case z_convert:to_binary( z_config:get(server_header) ) of
        <<>> -> <<"Zotonic">>;
        Name -> filter_server_name(Name, <<>>)
    end.

-spec filter_server_name(binary(), binary()) -> binary().
filter_server_name(<<>>, Acc) ->
    Acc;
filter_server_name(<<C, Rest/binary>>, Acc) when C >= $a, C =< $z ->
    filter_server_name(Rest, <<Acc/binary, C>>);
filter_server_name(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    filter_server_name(Rest, <<Acc/binary, C>>);
filter_server_name(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    filter_server_name(Rest, <<Acc/binary, C>>);
filter_server_name(<<32, Rest/binary>>, Acc) ->
    filter_server_name(Rest, <<Acc/binary, 32>>);
filter_server_name(<<_, Rest/binary>>, Acc) ->
    filter_server_name(Rest, Acc).


%% @todo reorder cipher list? See: https://sparanoid.com/note/http2-and-ecdsa-cipher-suites/
%% ECDHE-RSA-AES128-GCM-SHA256 and ECDHE-ECDSA-AES128-GCM-SHA256 should be at the top.
%% Otherwise Chrome will give ERR_SPDY_INADEQUATE_TRANSPORT_SECURITY
%% There is a problem with Firefox, which *needs* a cipher suite not implemented by Erlang
%% https://github.com/tatsuhiro-t/lucid/blob/ce8654a75108c15cc786424b3faf1a8e945bfd53/README.rst#current-status
ciphers() ->
     [
        "ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
        "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
        "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
        "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
        "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
        "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
        "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
        "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
        "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
        "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
        "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
        "ECDH-RSA-AES128-SHA","AES128-SHA"
    ].
    % ssl:cipher_suites().


%% @doc Decode a certificate file, return common_name, not_after etc.
-spec decode_cert(file:filename_all()) -> {ok, list()} | {error, not_a_certificate}.
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

