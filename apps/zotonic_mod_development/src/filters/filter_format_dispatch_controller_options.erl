%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2023 Maas-Maarten Zeeman
%% @doc 'format_dispatch_controller_options' Formats controller options

%% Copyright 2023 Maas-Maarten Zeeman
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

-module(filter_format_dispatch_controller_option).
-export([
    format_dispatch_controller_options/2,
    format_dispatch_controller_options/3
]).

format_dispatch_controller_options(zotonic_dispatch_module, _Context) ->
    undefined;
format_dispatch_controller_options(zotonic_dispatch_file, _Context) ->
    undefined;
format_dispatch_controller_options(Option, Context) ->
    format_pprint:pprint(Option, Context).

format_dispatch_controller_options(zotonic_dispatch_module, _Controller, _Context) ->
    undefined;
format_dispatch_controller_options(zotonic_dispatch_file, _Controller, _Context) ->
    undefined;
format_dispatch_controller_options(Option, _Controller, Context) ->
    format_pprint:pprint(Option, Context).

