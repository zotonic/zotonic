%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016 Arjan Scherpenisse
%% @doc DKIM signing of outgoing emails

%% Copyright 2016 Arjan Scherpenisse
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
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-mod_title("Email DKIM signing").
-mod_description("Signs outgoing e-mails with DomainKeys Identified Mail Signatures (RFC 6376)").
-mod_prio(500).

-export([
         init/1,
         observe_admin_menu/3,
         observe_dkim_admin_info/2
        ]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

init(Context) ->
    z_email_dkim:ensure_configured(Context),
    ok.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_dkim,
                parent=admin_modules,
                label=?__("DKIM e-mail setup", Context),
                url={admin_dkim},
                visiblecheck={acl, use, mod_email_dkim}}
     |Acc].

observe_dkim_admin_info(dkim_admin_info, Context) ->
    {Priv, Pub} = z_email_dkim:cert_files(Context),
    [
     {privkey_filepath, Priv},
     {pubkey_filepath, Pub},
     {dns_entry_domain, z_email_dkim:dns_entry_domain(Context)},
     {dns_entry, z_email_dkim:dns_entry(Context)}
    ].
