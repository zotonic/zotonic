%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Search engine optimization.  Provides admin interface for the SEO modules.
%% @end

%% Copyright 2009-2023 Marc Worrell
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

-module(mod_seo).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Search Engine Optimization (SEO)").
-mod_description("Structured data, Google Analytics").
-mod_prio(600).
-mod_depends([base, admin]).
-mod_provides([seo]).

%% interface functions
-export([
         observe_admin_menu/3
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").


observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
     #menu_item{id=admin_seo,
                parent=admin_modules,
                label=?__("SEO", Context),
                url={admin_seo},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].
