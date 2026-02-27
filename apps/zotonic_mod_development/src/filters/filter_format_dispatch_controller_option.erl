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
-moduledoc("
Format dispatch controller options.

See [mod_development](/id/doc_module_mod_development)

Todo

Not yet documented.
").
-export([
    format_dispatch_controller_option/2,
    format_dispatch_controller_option/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

format_dispatch_controller_option(Option, Context) ->
    format_option(Option, Context).

format_dispatch_controller_option(Option, _Controller, Context) ->
    format_option(Option, Context).

format_option({zotonic_dispatch_file, _}, _Context) ->
    undefined;
format_option({zotonic_dispatch_module, _}, _Context) ->
    undefined;

format_option({id, Value}, Context) ->
    SafeValue = pprint(Value, Context),
    <<"<span class=\"label label-primary\">id | ", SafeValue/binary, "</span>">>;
format_option({acl, Value}, Context) ->
    SafeValue = pprint(Value, Context),
    <<"<span class=\"label label-warning\">acl | ", SafeValue/binary, "</span>">>;
format_option({template, Value}, Context) ->
    SafeValue = pprint(Value, Context),
    <<"<span class=\"label label-info\">template |", SafeValue/binary, "</span>">>;

format_option({Key, Value}, Context) ->
    SafeKey = pprint(Key, Context),
    SafeValue = pprint(Value, Context),
    <<"<span class=\"label label-default\">", SafeKey/binary, " | ", SafeValue/binary, "</span>">>;
format_option(Value, Context) ->
    SafeValue = pprint(Value, Context),
    <<"<span class=\"label label-default\">", SafeValue/binary, "</span>">>.

pprint(Something, Context) ->
    filter_pprint:pprint(Something, Context).

