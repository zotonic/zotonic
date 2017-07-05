%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Creates an editable overview of all categories.

%% Copyright 2009 Marc Worrell
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

-module(controller_admin_category_sorter).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/1
]).

-include_lib("zotonic_core/include/controller_html_helper.hrl").

is_authorized(Context) ->
    z_admin_controller_helper:is_authorized(mod_admin_category, Context).

html(Context) ->
    Html = z_template:render("admin_category_sorter.tpl", [{page_admin_category_sorter, true}], Context),
    z_context:output(Html, Context).

