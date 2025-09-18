%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2023 Maas-Maarten Zeeman
%% @doc 'format_dispatch_path_element' Formats an element in a path pattern of a dispatch rule

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

-module(filter_format_dispatch_path_element).
-moduledoc("
Format dispatch path element so variables will be more visible.

See [mod\\_development](/id/doc_module_mod_development)

Todo

Not yet documented.
").
-export([format_dispatch_path_element/2]).


format_dispatch_path_element(Atom, _Context) when is_atom(Atom) ->
    Var = z_convert:to_binary(Atom),
    SafeVar = z_html:escape(Var),
    <<"<code><var class=\"\">", SafeVar/binary, "</var></code>">>;
format_dispatch_path_element(Elt, _Context) when is_list(Elt) orelse is_binary(Elt) ->
    format_exact_match(Elt);
format_dispatch_path_element(Elt, Context) ->
    filter_pprint:pprint(Elt, Context).

format_exact_match(Elt) ->
    Exact = z_convert:to_binary(Elt),
    SafeExact = z_html:escape(Exact),
    <<"<span>", SafeExact/binary, "</span>">>.
