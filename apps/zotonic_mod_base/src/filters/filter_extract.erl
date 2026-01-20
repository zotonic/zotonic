%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2026 Maas-Maarten Zeeman
%% @doc Retrieve a property from a map, proplist or resource.

%% Copyright 2026 Maas-Maarten Zeeman
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


-module(filter_extract).
-export([extract/3]).

extract(undefined, _Prop, _Context) ->
    [];
extract(List, Prop, Context) when is_list(List) ->
	[ extract_value(Elt, Prop, Context) || Elt <- List ];
extract(Item, Prop, Context) ->
    extract_value(Item, Prop, Context).

extract_value(Item, Prop, Context) ->
    z_template_compiler_runtime:find_value(Prop, Item, #{}, Context).
