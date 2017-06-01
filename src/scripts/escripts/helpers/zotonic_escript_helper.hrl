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

-define(ZOTONIC_BIN, "$ZOTONIC/bin").
-define(ERL, "erl").
-define(MAKE, "make").
-define(MAX_PORTS, "ulimit -n|sed 's/unlimited/100000/'").
-define(MAX_PROCESSES, 10000000).
-define(KERNEL_POLL, true).
-define(USER_EBIN_DIR, "").
-define(PA, "$ZOTONIC/_checkouts/*/ebin $ZOTONIC/_build/default/lib/*/ebin $USER_EBIN_DIR $ZOTONIC/modules/*/deps/*/ebin $ZOTONIC/priv/modules/*/deps/*/ebin $ZOTONIC/priv/sites/*/deps/*/ebin $ZOTONIC/priv/sites/*/modules/*/deps/*/ebin").
-define(NODENAME, "zotonic001").
-define(ZOTONIC_DISTRIBUTED, "").

-ifdef(ZOTONIC_DISTRIBUTED).
-define(NAME_ARG, "-name").
-define(NODEHOST, "").
-else.
-define(NAME_ARG, "-sname").
-define(NODEHOST, "").
-endif.

-define(ZOTONIC_PIDFILE, "$ZOTONIC/zotonic.pid").
-define(ERL_CALL, "").
-define(ZOTONIC_CALL, "").
-define(MODULES, "").
-define(SITES, "").
