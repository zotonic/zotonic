%% @author Driebit <info@driebit.nl>
%% @copyright 2024 Driebit
%% @doc is_supported_version filter, checks if the editor template to include exists
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

-module(filter_is_supported_version).
-author("Driebit <info@driebit.nl>").
-export([is_supported_version/2]).

is_supported_version(undefined, _Context) ->
    true;
is_supported_version(<<"newest">>, _Context) ->
    true;
is_supported_version("newest", _Context) ->
    true;
is_supported_version(VersionString, Context) ->
    VersionBinary = z_convert:to_binary(VersionString),
    EditorTemplate = <<"tinymce-", VersionBinary/binary, "/_editor.tpl">>,
    case z_module_indexer:find(template, EditorTemplate, Context) of
        {ok, _Found} ->
            true;
        {error, _Reason} ->
            z:warning(
                "tinymce: version from config (~s) is unsupported, defaulting to newest.",
                [ VersionString ],
                [ {module, ?MODULE}, {line, ?LINE} ],
                Context),
            false
    end.
