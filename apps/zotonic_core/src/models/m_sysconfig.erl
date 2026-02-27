%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2026 Marc Worrell
%% @doc Get system configuration keys.
%% @end

%% Copyright 2016-2026 Marc Worrell
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

-module(m_sysconfig).
-moduledoc("
Note

System configurations are only accessible from templates, using `m.sysconfig`, for users with administrator rights.

Gives access to the Zotonic system configuration from the `zotonic.config` file(s).

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/+key/...` | Look up system configuration key `+key` via `z_config:get/1` (admin-only); unknown keys return `undefined`. |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.

Known Keys
----------

The following keys are defined in `z_config:all/0`:

| Key | Description |
| --- | --- |
| `environment` | Runtime environment profile such as development, test, or production. |
| `zotonic_apps` | Path where user applications are located. |
| `security_dir` | Directory containing TLS and security-related files. |
| `data_dir` | Directory for persistent runtime data. |
| `log_dir` | Directory for log output files. |
| `cache_dir` | Directory for cache files. |
| `password` | Global bootstrap/management password setting when configured. |
| `admin_email` | Default administrator/system email address. |
| `timezone` | Default server timezone. |
| `listen_ip` | IPv4 listen address for HTTP listener. |
| `listen_ip6` | IPv6 listen address for HTTP listener. |
| `listen_port` | HTTP listen port. |
| `ssl_listen_port` | HTTPS listen port. |
| `port` | Public HTTP port used in URL generation. |
| `ssl_port` | Public HTTPS port used in URL generation. |
| `max_connections` | Maximum HTTP connections. |
| `ssl_max_connections` | Maximum HTTPS connections. |
| `dbhost` | PostgreSQL host. |
| `dbport` | PostgreSQL port. |
| `dbuser` | PostgreSQL user name. |
| `dbpassword` | PostgreSQL password. |
| `dbdatabase` | PostgreSQL database name. |
| `dbschema` | PostgreSQL schema name. |
| `security_headers` | Enable or disable default security headers. |
| `smtp_verp_as_from` | Use VERP envelope sender behavior for outgoing email. |
| `smtp_no_mx_lookups` | Disable MX lookups for SMTP delivery. |
| `smtp_relay` | Enable relay delivery via configured SMTP relay. |
| `smtp_username` | Username for SMTP relay authentication. |
| `smtp_password` | Password for SMTP relay authentication. |
| `smtp_host` | SMTP relay host. |
| `smtp_port` | SMTP relay port. |
| `smtp_ssl` | Use SSL/TLS when connecting to SMTP relay. |
| `smtp_relay_tls_options` | TLS options for SMTP relay connection. |
| `smtp_listen_ip` | Listen address for inbound SMTP listener. |
| `smtp_listen_port` | Listen port for inbound SMTP listener. |
| `smtp_starttls` | Enable STARTTLS for inbound SMTP. |
| `smtp_spamd_ip` | Spamd host for spam filtering integration. |
| `smtp_spamd_port` | Spamd port for spam filtering integration. |
| `smtp_dns_blocklist` | DNS blocklists consulted for inbound SMTP checks. |
| `smtp_dns_allowlist` | DNS allowlists consulted for inbound SMTP checks. |
| `smtp_delete_sent_after` | Retention window for sent-email log rows. |
| `smtp_max_senders` | Max sender processes/concurrency for SMTP sending. |
| `mqtt_listen_ip` | Listen address for MQTT listener (IPv4). |
| `mqtt_listen_ip6` | Listen address for MQTT listener (IPv6). |
| `mqtt_listen_port` | MQTT TCP listen port. |
| `mqtt_listen_ssl_port` | MQTT TLS listen port. |
| `mqtt_max_connections` | Maximum MQTT connections (plain TCP). |
| `mqtt_ssl_max_connections` | Maximum MQTT connections (TLS). |
| `inet_backlog` | TCP listen backlog size for HTTP listener. |
| `inet_acceptor_pool_size` | Number of HTTP acceptor processes. |
| `ssl_backlog` | TLS listen backlog size. |
| `ssl_acceptor_pool_size` | Number of HTTPS acceptor processes. |
| `ssl_dhfile` | Diffie-Hellman parameter file for TLS. |
| `filewatcher_enabled` | Enable filesystem watcher for site/app changes. |
| `filewatcher_scanner_enabled` | Enable periodic scanner fallback for filewatcher. |
| `filewatcher_scanner_interval` | Interval in ms for periodic filewatch scanner. |
| `syslog_ident` | Syslog ident/program name. |
| `syslog_opts` | Syslog logger options. |
| `syslog_facility` | Syslog facility used for logging. |
| `syslog_level` | Minimum syslog severity level. |
| `proxy_allowlist` | Trusted proxy allowlist for forwarded headers. |
| `ip_allowlist` | Global IP allowlist for request access. |
| `ip_allowlist_system_management` | IP allowlist for system-management endpoints. |
| `sessionjobs_limit` | Maximum number of session job workers. |
| `sidejobs_limit` | Maximum number of sidejob workers. |
| `log_http_metrics_buffer_size` | Buffer size for HTTP metrics logging. |
| `server_header` | Value used for HTTP `Server` response header. |
| `html_error_path` | Directory containing static HTML error pages. |
").
-author("Marc Worrell <marc@worrell.nl").

-behaviour(zotonic_model).

%% interface functions
-export([
    m_get/3
]).

%% @doc Fetch the value for the key from a model source
-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ Key | Rest ], _Msg, Context) ->
    try
        KeyAtom = erlang:binary_to_existing_atom(Key, utf8),
        case z_acl:is_admin(Context) of
            true -> {ok, {z_config:get(KeyAtom), Rest}};
            false -> {error, eacces}
        end
    catch
        error:badarg ->
            {ok, {undefined, Rest}}
    end;
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.
