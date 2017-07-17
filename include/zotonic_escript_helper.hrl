%% @author M, <tantemelki@gmail.com>
%% @copyright 2017
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%	 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% usage zotonic generate-edoc
%%%-------------------------------------------------------------------

-define(HOME, zotonic_setup:get_home_path()).
-define(ZOTONIC, zotonic_setup:get_zotonic_path()).
-define(ZOTONIC_BIN, ?ZOTONIC ++ "/bin").
-define(ZOTONIC_SCRIPTS, ?ZOTONIC ++ "/src/scripts").
-define(ERL, "erl").
-define(MAKE, "make").
-define(MAX_PORTS, "ulimit -n|sed 's/unlimited/100000/'").
-define(MAX_PROCESSES, 10000000).
-define(KERNEL_POLL, true).
-define(USER_EBIN_DIR, zotonic_setup:get_user_ebin_dir()).
-define(NODENAME, "zotonic001").
-define(NODEHOST, net_adm:localhost()).
-define(PA, ?ZOTONIC ++ "/_checkouts/*/ebin " ++ ?ZOTONIC ++ "/_build/default/lib/*/ebin " ++ ?USER_EBIN_DIR ++ " " ++
    ?ZOTONIC ++ "/modules/*/deps/*/ebin " ++ ?ZOTONIC ++ "/priv/modules/*/deps/*/ebin " ++
    ?ZOTONIC ++ "/priv/sites/*/deps/*/ebin " ++ ?ZOTONIC ++ "/priv/sites/*/modules/*/deps/*/ebin").

-define(ZOTONIC_DISTRIBUTED, zotonic_setup:distributed_detected()).
-ifdef(ZOTONIC_DISTRIBUTED).
-define(NAME_ARG, "-name").
-else.
-define(NAME_ARG, "-sname").
-endif.

-define(ZOTONIC_PIDFILE, ?ZOTONIC ++ "/zotonic.pid").
-define(Modules, zotonic_setup:get_user_modules_dir()).
-define(Sites, zotonic_setup:get_user_sites_dir()).

-define(ZOTONIC_PORT, 8040).
-define(ZOTONIC_LISTEN_PORT, 8040).
-define(ZOTONIC_PORT_SSL, 8043).
-define(ZOTONIC_SSL_LISTEN_PORT, 8043).
-define(ZOTONIC_SMTP_BOUNCE_PORT, 2535).

-define(EBIN_DIR, ?ZOTONIC ++ "/_build/default/lib/zotonic/ebin").
