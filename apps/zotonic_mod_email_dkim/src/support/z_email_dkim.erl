%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016-2021 Arjan Scherpenisse
%% @doc Support functions for signing e-mail messages using DKIM

%% Copyright 2016-2021 Arjan Scherpenisse
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

-module(z_email_dkim).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([ensure_configured/1, cert_files/1, dns_entry/1, dns_entry_domain/1, mimemail_options/1]).

%% @doc Called by mod_email_dkim:init/1 to generate the DKIM key files
%% on first install.
ensure_configured(Context) ->
    case is_configured(Context) of
        true ->
            ok;
        false ->
            generate_key(Context)
    end.

%% @doc Return a {Priv, Pub} tuple listing the paths to the private
%% and public key files
cert_files(Context) ->
    KeyDir = keydir(Context),
    {filename:join(KeyDir, "dkim.key"), filename:join(KeyDir, "dkim.pub")}.

keydir(Context) ->
    {ok, SecurityDir} = z_config_files:security_dir(),
    filename:join([ SecurityDir, z_context:site(Context), "dkim" ]).

%% @doc Return the DNS domain that is used to place the TXT record in
dns_entry_domain(Context) ->
    z_convert:to_list(dkim_selector(Context)) ++ "._domainkey." ++ z_email:email_domain(Context).

%% @doc Generate the DNS entry to be put in the TXT record; this info
%% is shown in the admin to aid configuration.
dns_entry(Context) ->
    {_, PubKeyFile} = cert_files(Context),
    {ok, <<"-----BEGIN PUBLIC KEY-----\n", PubKey0/binary>>} = file:read_file(PubKeyFile),
    PubKey = binary:replace(binary:replace(PubKey0, <<"-----END PUBLIC KEY-----">>, <<>>), <<"\n">>, <<>>, [global]),
    "v=DKIM1; k=rsa; p=" ++ z_convert:to_list(PubKey).

%% @doc Generate an RSA key using the openssl utility
generate_key(Context) ->
    maybe_move_0x_keys(Context),
    {PrivKeyFile, PubKeyFile} = cert_files(Context),
    ok = z_filelib:ensure_dir(PrivKeyFile),
    ok = z_filelib:ensure_dir(PubKeyFile),
    PrivKeyCmd = "openssl genrsa -out " ++ z_filelib:os_filename(PrivKeyFile) ++ " 1024",
    os:cmd(PrivKeyCmd),
    PubKeyCmd = "openssl rsa "
        ++ " -in "  ++ z_filelib:os_filename(PrivKeyFile)
        ++ " -out " ++ z_filelib:os_filename(PubKeyFile)
        ++ " -pubout -outform PEM",
    os:cmd(PubKeyCmd),
    file:change_mode(PrivKeyFile, 8#00700),
    ok.

maybe_move_0x_keys(Context) ->
    KeyDir = filename:join([ z_path:site_dir(Context), "priv", "dkim" ]),
    Sitename = z_convert:to_list(z_context:site(Context)),
    KeyFile = filename:join(KeyDir, Sitename++".dkim.key"),
    PubFile = filename:join(KeyDir, Sitename++".dkim.pub"),
    case filelib:is_regular(KeyFile) andalso filelib:is_regular(PubFile) of
        true ->
            {NewKey, NewPub} = cert_files(Context),
            lager:info("Moving old DKIM keys to \"~s\"", [ filename:dirname(NewKey) ]),
            z_filelib:ensure_dir(NewKey),
            {ok, _} = file:copy(KeyFile, NewKey),
            {ok, _} = file:copy(PubFile, NewPub),
            file:delete(KeyFile),
            file:delete(PubFile),
            file:change_mode(KeyFile, 8#00700),
            file:del_dir( filename:dirname(KeyFile) );
        false ->
            ok
    end.


%% @doc Create the options list which are passed to gen_smtp's mimemail:encode/2 function.
mimemail_options(Context) ->
    case is_configured(Context) of
        false ->
            [];
        true ->
            Now = calendar:universal_time(),
            Expires = z_datetime:next_year(Now),
            [{dkim, [{s, dkim_selector(Context)},
                     {d, z_convert:to_binary(z_email:email_domain(Context))},
                     {c, {simple, simple}},
                     {t, Now},
                     {x, Expires},
                     {private_key, {pem_plain, get_priv_key(Context)}}]}]
    end.

%% @doc Return the DKIM 'selector', which is the first part of the
%% _domainkey subdomain, defaults to "zotonic".
dkim_selector(Context) ->
    m_config:get_value(site, dkim_selector, <<"zotonic">>, Context).

%% @doc Check whether we have the DKIM keys or not
is_configured(Context) ->
    {PrivKeyFile, PubKeyFile} = cert_files(Context),
    filelib:is_regular(PrivKeyFile) andalso filelib:is_regular(PubKeyFile).

%% @doc Return the key contents, used in the actual signing. Cached to
%% prevent file access on every mail sent.
get_priv_key(Context) ->
    z_depcache:memo(
        fun() ->
            {PrivKeyFile, _} = cert_files(Context),
            {ok, Contents} = file:read_file(PrivKeyFile),
            Contents
        end, dkim_priv_key, ?DAY, Context).
