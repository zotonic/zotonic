%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2010-2015 Marc Worrell, 2014 Arjan Scherpenisse
%% @doc Wrapper for Zotonic application environment configuration

%% Copyright 2010-2015 Marc Worrell, 2014 Arjan Scherpenisse
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

-module(z_config).
-author("Marc Worrell <marc@worrell.nl>").

%% API export
-export([
         get/1,
         get/2,

         init_app_env/0
        ]).


-include_lib("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Copy some zotonic config settings over to other applications
-spec init_app_env() -> ok.
init_app_env() ->
    application:set_env(cowmachine, proxy_whitelist, ?MODULE:get(proxy_whitelist)),
    application:set_env(cowmachine, ip_whitelist, ?MODULE:get(ip_whitelist)),
    ok.


%% @doc Get value from config file (cached)
%%
%% Some config settings can be overruled by environment settings.
-spec get(atom()) -> any().
get(listen_ip) ->
    case os:getenv("ZOTONIC_IP") of
        false -> ?MODULE:get(listen_ip, default(listen_ip));
        Any when Any == []; Any == "*"; Any == "any" -> any;
        Ip -> Ip
    end;
get(listen_port) ->
    case os:getenv("ZOTONIC_PORT") of
        false -> ?MODULE:get(listen_port, default(listen_port));
        Port -> z_convert:to_integer(Port)
    end;
get(ssl_listen_port) ->
    case os:getenv("ZOTONIC_SSL_PORT") of
        false -> ?MODULE:get(ssl_listen_port, default(ssl_listen_port));
        "none" -> none;
        Port -> z_convert:to_integer(Port)
    end;
get(Key) ->
    ?MODULE:get(Key, default(Key)).

%% @doc Get value from config file, returning default value when not set (cached).
-spec get(atom(), any()) -> any().
get(Key, Default) ->
	case application:get_env(zotonic, Key) of
		undefined ->
			Default;
		{ok, Value} ->
			Value
	end.

default(timezone) -> <<"UTC">>;
default(listen_ip) -> any;
default(listen_port) -> 8000;
default(ssl_listen_port) -> 8443;
default(port) -> ?MODULE:get(listen_port);
default(ssl_port) -> ?MODULE:get(ssl_listen_port);
default(ssl_only) -> false;
default(smtp_verp_as_from) -> false;
default(smtp_no_mx_lookups) -> false;
default(smtp_relay) -> false;
default(smtp_host) -> "localhost";
default(smtp_port) -> 25;
default(smtp_ssl) -> false;
default(smtp_listen_ip) -> "127.0.0.1";
default(smtp_listen_port) -> 2525;
default(smtp_spamd_port) -> 783;
default(smtp_dnsbl) -> z_email_dnsbl:dnsbl_list();
default(smtp_dnswl) -> z_email_dnsbl:dnswl_list();
default(smtp_delete_sent_after) -> 240;
default(inet_backlog) -> 500;
default(inet_acceptor_pool_size) -> 100;
default(ssl_backlog) -> ?MODULE:get(inet_backlog);
default(ssl_acceptor_pool_size) -> ?MODULE:get(inet_acceptor_pool_size);
default(ssl_dhfile) -> undefined;
default(dbhost) -> "localhost";
default(dbport) -> 5432;
default(dbuser) -> "zotonic";
default(dbpassword) -> "";
default(dbdatabase) -> "zotonic";
default(dbschema) -> "public";
default(filewatcher_enabled) -> true;
default(filewatcher_scanner_enabled) -> false;
default(syslog_ident) -> "zotonic";
default(syslog_opts) -> [ndelay];
default(syslog_facility) -> local0;
default(syslog_level) -> info;
default(user_sites_dir) -> "user/sites";
default(user_modules_dir) -> "user/modules";
default(proxy_whitelist) -> local;
default(ip_whitelist) -> local;
default(sessionjobs_limit) -> erlang:max(erlang:system_info(process_limit) div 10, 10000);
default(sidejobs_limit) -> erlang:max(erlang:system_info(process_limit) div 2, 50000);
default(_) -> undefined.
