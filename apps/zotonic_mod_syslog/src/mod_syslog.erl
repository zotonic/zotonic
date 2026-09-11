%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2026 Marc Worrell
%% @doc Log http requests to the syslog.
%% @end

%% Copyright 2021-2026 Marc Worrell
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

-module(mod_syslog).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "operator", "module", "logging_and_monitoring", "api_and_integration", "send_and_receive"
    ]
}).
-moduledoc("
Syslog integration module for forwarding Zotonic access logs to external syslog services.

This module subscribes to `$SYS/site/<name>/log/access` and sends access log events
to syslog.

It uses the following node-wide Zotonic configuration options:

- `syslog_ident`: The ident used to open the syslog port. It will be prepended to each message. Default: `zotonic`.
- `syslog_opts`: The options used to open the syslog port. Default: `[ndelay]`.
- `syslog_facility`: The facility part of the priority. Default: `local0`.
- `syslog_level`: The level part of the priority. Default: `info`.

For more information, see the Erlang syslog module documentation at https://github.com/Vagabond/erlang-syslog
and your system's syslog manual (`man 3 syslog`).
").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Syslog").
-mod_description("Log http requests to the syslog.").
-mod_depends([]).
-mod_provides([]).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    init/1,
    handle_site_log_access/1
]).

init(Context) ->
    Site = z_context:site(Context),
    SiteBinary = z_convert:to_binary(Site),

    ok = z_mqtt:subscribe([<<"$SYS">>, <<"site">>, SiteBinary, <<"log">>, <<"access">>],
                          {?MODULE, handle_site_log_access, []},
                          self(),
                          z_acl:sudo(Context)),

    ok.

handle_site_log_access(#{ message := #{ payload := Log }}) ->
    _ = z_syslog_logger:log(Log),
    ok;
handle_site_log_access(_Msg) ->
    ok.

