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

    sni_self_signed/1,
    ensure_self_signed/1
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
        {versions, [ 'tlsv1.2' ]},
        {sni_fun, fun ?MODULE:sni_fun/1},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {ciphers, zotonic_ssl_certs:ciphers()}
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
            {ok, LocalHostname} = inet:gethostname(),
            sni_self_signed(LocalHostname)
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
            Options = #{
                hostname => Hostname,
                servername => server_name()
            },
            case zotonic_ssl_certs:ensure_self_signed(CertFile, KeyFile, Options) of
                ok ->
                    {ok, [
                        {certfile, CertFile},
                        {keyfile, KeyFile}
                    ]};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            {error, {ensure_dir, Error, PemFile}}
    end.

%% @doc Return the name of the server, defaults to 'Zotonic' or
%  the alphanumerical part of the server_header config.
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
