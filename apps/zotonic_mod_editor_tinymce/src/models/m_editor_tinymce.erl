%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2026 Marc Worrell
%% @doc Model for mod_editor_tinymce
%% @end

%% Copyright 2017-2026 Marc Worrell
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

-module(m_editor_tinymce).
-moduledoc("
Model exposing TinyMCE editor version metadata used by admin/editor integration.

Possible version identifiers:

* `newest` - always resolve to the bundled newest TinyMCE version.
* `4.9.3` - legacy TinyMCE 4 template set (`tinymce-4.9.3`).
* `5.10.2` - TinyMCE 5 template set (`tinymce-5.10.2`).

Any configured value must match an available `tinymce-<version>` template directory; unsupported values fall back to
`newest` when requesting `version_current`.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/version/...` | Return configured TinyMCE version identifier (`newest`, `4.9.3`, `5.10.2`, or another configured value). |
| `get` | `/version_current/...` | Return effective TinyMCE version in use (resolves `newest` and unsupported configured values to the bundled latest version). |

`/+name` marks a variable path segment. A trailing `/...` means extra path segments are accepted for further lookups.
").

-behaviour (zotonic_model).

-export([
    m_get/3,

    version/1,
    version_current/1
]).


-include("../tinymce_version.hrl").

-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"version">> | Rest ], _Msg, Context) ->
    {ok, {version(Context), Rest}};
m_get([ <<"version_current">> | Rest ], _Msg, Context) ->
    {ok, {version_current(Context), Rest}};
m_get(_Vs, _Msg, _Context) ->
    {error, unknown_path}.

%% @doc Return the configured tinyMCE version. Either the tinyMCE version or 'newest'.
-spec version(z:context()) -> binary().
version(Context) ->
    case m_config:get_value(mod_editor_tinymce, version, Context) of
        <<>> -> <<"newest">>;
        undefined -> <<"newest">>;
        Version -> Version
    end.

%% @doc Return the configured tinyMCE version. 'newest' is mapped to the actual version.
%% If the configured version is not supported, the newest version will be returned.
-spec version_current(z:context()) -> binary().
version_current(Context) ->
    case version(Context) of
        <<"newest">> ->
            z_convert:to_binary(?TINYMCE_VERSION);
        Version ->
            EditorTemplate = <<"tinymce-", Version/binary, "/_editor.tpl">>,
            case z_module_indexer:find(template, EditorTemplate, Context) of
                {ok, _Found} ->
                    Version;
                {error, _Reason} ->
                    z:warning(
                        "tinymce: version from config (~s) is unsupported, defaulting to newest.",
                        [ Version ],
                        [ {module, ?MODULE}, {line, ?LINE} ],
                        Context),
                    z_convert:to_binary(?TINYMCE_VERSION)
            end
    end.
