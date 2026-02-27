%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016-2025 Arjan Scherpenisse
%% @doc DKIM signing of outgoing emails
%% @end

%% Copyright 2016-2025 Arjan Scherpenisse
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

-module(mod_email_dkim).
-moduledoc("
Signs outgoing e-mails with DomainKeys Identified Mail Signatures ([RFC 6376](https://tools.ietf.org/html/rfc6376)).

DKIM (DomainKeys Identified Mail) is an important authentication mechanism to help protect both email receivers and
email senders from forged and phishing email.



How does it work?
-----------------

DKIM works by signing each e-mail that Zotonic sends with a private key. The public key part is exposed through a DNS
TXT record, with which email receiver can check whether the email actually originated from the domain that it claimed to
come from.

Note

The generating of the keypair depends on the `openssl` utility to be available in `$PATH`.

This RSA keypair is generated automatically when the module is installed, and the private/public keys are put in the
directory `sitename/dkim/`. When the module is active and the keypair has been generated, all outgoing e-mail will be signed.



DNS configuration
-----------------

The receiving e-mail server checks the validity of the signature by doing a DNS lookup. To configure DKIM, you will need
to add this DNS entry to your domain where you send the mail from.

In the admin, the page `/admin/email/dkim`, available under (“Modules” / “DKIM e-mail setup”) provides
information how to configure this DNS entry, including the text to copy-paste into the DNS record.



### DKIM selector

By default, the DKIM selector is set to the string `zotonic`. This will result in DNS lookups to the
`zotonic._domainkey.yoursite.com` domain. You can change the selector name by adding a config value called `site.dkim_selector`.

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_email_dkim_options`: Handle `email_dkim_options` notifications using `z_email_dkim:mimemail_options`.

").
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-mod_title("Email DKIM signing").
-mod_description("Signs outgoing e-mails with DomainKeys Identified Mail Signatures (RFC 6376)").
-mod_prio(500).
-mod_config([
        #{
            module => site,
            key => dkim_selector,
            type => string,
            default => "zotonic",
            description => "The DKIM selector to use for signing e-mails. This is used to find the public key in the DNS TXT record."
                           "Defaults to 'zotonic'."
        }
    ]).

-export([
    init/1,
    observe_email_dkim_options/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

init(Context) ->
    z_email_dkim:ensure_configured(Context),
    ok.

observe_email_dkim_options(#email_dkim_options{}, Context) ->
    z_email_dkim:mimemail_options(Context).

