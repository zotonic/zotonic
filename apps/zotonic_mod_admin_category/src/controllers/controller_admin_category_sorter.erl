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
-moduledoc("
Shows the admin category screen where you can edit the [category](/id/doc_glossary#term-category) tree, rearranging the
categories, adding new categories, or removing existing ones.

Todo

Extend documentation
").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    service_available/1,
    is_authorized/1,
    process/4
]).

service_available(Context) ->
    Context1 = z_context:set_noindex_header(Context),
    Context2 = z_context:set_nocache_headers(Context1),
    {true, Context2}.

is_authorized(Context) ->
    z_controller_helper:is_authorized([ {use, z_context:get(acl_module, Context, mod_admin_category)} ], Context).

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    Html = z_template:render("admin_category_sorter.tpl", [{page_admin_category_sorter, true}], Context),
    z_context:output(Html, Context).

