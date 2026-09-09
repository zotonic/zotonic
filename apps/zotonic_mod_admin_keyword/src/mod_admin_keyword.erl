%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Admin tools for analyzing keyword usage and overlap.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(mod_admin_keyword).
-moduledoc(#{
    zotonic_keywords => ["reference", "site_administrator", "module", "metadata", "categorization", "monitor"]
}).
-moduledoc("
Add keyword analysis tools to the Zotonic administration interface.

The keyword dashboard shows how often resources are connected to keywords with
the `subject` predicate. It also compares keyword pairs using their number of
shared resources, Jaccard similarity, and smaller-set overlap.


Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_admin_menu`: Add the keyword dashboard to the Structure menu.
").

-mod_title("Admin keyword analysis").
-mod_description("Analyze keyword use and overlap.").
-mod_prio(600).
-mod_depends([admin]).
-mod_provides([]).

-export([
    observe_admin_menu/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


-spec observe_admin_menu(#admin_menu{}, [#menu_item{}], z:context()) -> [#menu_item{}].
observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = admin_keyword,
            parent = admin_structure,
            label = ?__("Keywords", Context),
            url = admin_keyword,
            visiblecheck = {acl, use, mod_admin_keyword}
        }
        | Acc
    ].
