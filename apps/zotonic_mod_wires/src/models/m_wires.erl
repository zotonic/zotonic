%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Resolve convention-based widget dependencies for the widget manager.
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

-module(m_wires).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "backend_developer", "model",
        "javascript", "user_interface_and_interaction"
    ]
}).
-moduledoc("
Resolve JavaScript dependencies for automatic widget loading in `module#mod_wires`.
The widget manager uses this model when a `do_<name>` class has no registered
jQuery function yet.

Available Model API Paths
-------------------------

| Method | Path pattern | Description |
| --- | --- | --- |
| `get` | `/widget/+name` | Resolve `js/modules/z.<name>.js` and return a list containing its asset descriptor. |

`+name` is a single widget name containing letters, digits, underscores or
hyphens, without the `do_` prefix. Resolution uses the site's library index,
including enabled modules. The descriptor contains `key`, a timestamped `url`,
`type`, `media`, `rel` and `title`, as returned by `z_lib_include:assets/3`.
The request payload is ignored. The model resolves the asset; it does not load
it or initialize the widget.

For example, load the library for `do_autofocus` in the browser:

```javascript
cotonic.ready.then(() => cotonic.broker.call(
    'bridge/origin/model/wires/get/widget/autofocus'
)).then(response => {
    if (response.payload.status !== 'ok') {
        throw new Error(response.payload.error);
    }
    return z_assets.ensure(response.payload.result);
});
```

Normally the widget manager performs this lookup and initializes the widget
after loading. The asset loader shares loaded and pending files with lib tags
and dynamic updates.

Configuration and errors
-------------------------

Configuration module `mod_wires`, key `widget_autoload`, defaults to `true`.
Setting it to `false` disables resolution through this model; explicitly
included widgets continue to work.

| Error | Meaning |
| --- | --- |
| `disabled` | Automatic widget loading is disabled for the site. |
| `badarg` | The widget name is empty or contains unsupported characters. |
| `enoent` | The conventional library file is absent from the site library index. |
| `unknown_path` | The requested model path is not supported. |

This read-only model has no additional ACL restriction: it exposes descriptors
for public library files. It does not accept arbitrary paths or URLs.

Direct Erlang calls to `m_get/3` return `{ok, {Assets, Rest}}` on success or
`{error, Reason}` on failure. `Rest` contains any remaining model path segments.
").
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(zotonic_model).
-export([
    m_get/3
]).

%% @doc Return the widget library descriptor when automatic loading is enabled.
-spec m_get(Path, Msg, Context) -> Result when
    Path :: list(),
    Msg :: zotonic_model:opt_msg(),
    Context :: z:context(),
    Result :: zotonic_model:return().
m_get([<<"widget">>, Name | Rest], _Msg, Context) when is_binary(Name) ->
    case m_config:get_boolean(mod_wires, widget_autoload, true, Context) of
        true ->
            case widget(Name, Context) of
                {ok, Assets} ->
                    {ok, {Assets, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, disabled}
    end;
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% Only simple widget names are accepted; never turn a class into an arbitrary
%% URL or repository path. Resolve through the indexer and normal lib URL code.
widget(Name, Context) ->
    case re:run(Name, <<"^[a-zA-Z0-9_-]+$">>, [{capture, none}]) of
        match ->
            File = <<"js/modules/z.", Name/binary, ".js">>,
            case z_module_indexer:find(lib, File, Context) of
                {ok, _} -> {ok, z_lib_include:assets([File], [], Context)};
                {error, _} -> {error, enoent}
            end;
        nomatch ->
            {error, badarg}
    end.
