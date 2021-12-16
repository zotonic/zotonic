%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2016-2021 Arjan Scherpenisse
%% @doc DKIM signing of outgoing emails

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

-module(mod_email_dkim).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-mod_title("Email DKIM signing").
-mod_description("Signs outgoing e-mails with DomainKeys Identified Mail Signatures (RFC 6376)").
-mod_prio(500).

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

