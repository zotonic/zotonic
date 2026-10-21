---
keywords:
  - reference
  - frontend_developer
  - template
  - javascript
  - performance
  - url
---

::: aside
See also

[script](script.md), [javascript](javascript.md) and `module#mod_development`.
:::

Include CSS and JavaScript libraries, combining files of each type into a single request.

The `lib` tag generates `<link>` elements for CSS and `<script>` elements for JavaScript. Filenames refer to files in module or site `priv/lib` directories. CSS and JavaScript use separate requests; files of the same type retain their declaration order.

```django
{% lib "css/site.css" "css/components.css" "js/site.js" %}
```

The generated URLs include a checksum of the files' modification times, so changing a file changes its URL. Development settings can disable bundling or minification.

## Duplicate inclusions

During a render, libraries already included or queued by an earlier `lib` tag are filtered out. For example:

```django
{% lib "js/a.js" "js/b.js" %}
{% lib "js/b.js" "js/c.js" %}
```

The second tag includes only `js/c.js`. For JavaScript, the first declaration's options take precedence. CSS declarations with different `media`, `rel` or `title` values are distinct inclusions.

This tracking covers `lib` tags, not manually written `<script>` or `<link>` elements. Generating a URL with `lib_url` does not include or reserve a library and does not flush collected files.

## Collecting libraries

Use `collect` to queue files without emitting their include elements at that position:

```django
{% lib "js/component.js" collect %}

<div class="component">...</div>

{% lib "js/page.js" %}
```

The last tag emits `component.js` before `page.js`, combining them when their options are compatible.

An ordinary, non-collecting lib tag flushes only the types present in its filename list:

- A CSS-only tag flushes pending CSS and leaves pending JavaScript queued.
- A JavaScript-only tag flushes pending JavaScript and leaves pending CSS queued.
- A mixed tag flushes both types.

This also applies when the ordinary tag's files are all duplicates. Another `collect` tag only queues files. Includes, cached render trees and `spaceless` blocks do not themselves flush the surrounding queue.

Collected files retain their original options. Only adjacent compatible groups combine; declarations are not regrouped across incompatible options. CSS groups must agree on `media`, `rel` and `title`. Both types must have compatible loading options, effective minification, cache behavior and absolute-URL settings.

`collect` cannot be combined with enabled `async` or `defer`. A later ordinary tag with `async` or `defer` flushes pending files of its type first, then emits its own include separately; it does not apply those loading options to the queued files.

Collection delays when the browser discovers the files. Intervening inline JavaScript must not depend on queued libraries, and delaying CSS can cause a flash of unstyled content.

## Flushing libraries

Flush queued libraries explicitly without adding filenames:

```django
{% lib flush %}               {# CSS and JavaScript #}
{% lib flush="css" %}         {# CSS only #}
{% lib flush="javascript" %}  {# JavaScript only; "js" also works #}
```

Flush accepts no filenames or other options. Unknown flush values are rejected. An empty queue produces no output, and flushing does not reset duplicate tracking.

The [script](script.md) tag automatically flushes both types before emitting the page's initialization script. Use an explicit CSS flush in the head when styles should load earlier:

```django
<head>
    {% lib "css/site.css" collect %}
    {% lib flush="css" %}
</head>
<body>
    {% lib "js/component.js" collect %}
    ...
    {% script %}
</body>
```

At the end of final page output, remaining collected files are emitted and a warning is logged. Use explicit flush points or `script` to place the include elements correctly, rather than relying on this fallback.

## Options

Supply options after the filenames. Flush is an alternative form with no filenames.

| Option | Default | Description |
| --- | --- | --- |
| `collect` | `false` | Queue files until a matching flush point. Incompatible with enabled `async` or `defer`. |
| `flush` | — | Flush both types with `flush`, or select `"css"`, `"javascript"` or `"js"`. Accepts no filenames or other options. |
| `absolute_url` | `false` | Generate an absolute library URL. |
| `title` | Unset | Set the CSS link's `title` attribute. |
| `media` | `"all"` | Set the CSS link's `media` attribute. |
| `rel` | `"stylesheet"` | Set the CSS link's `rel` attribute. |
| `minify` | Site setting | Request minification. Live reload disables minification. |
| `nocache` | `false` | Use the library route with a browser cache lifetime of at most one second. |
| `async` | `false` | Load CSS or JavaScript asynchronously. JavaScript tags also carry `defer` as a fallback. |
| `defer` | `false` | Load JavaScript in parallel and execute it after HTML parsing. |

Set `site.minification_enabled` to a true value to enable minification for all lib tags. An individual `minify=false` does not override that site setting.

## Dynamic updates and materialized fragments

Wired updates collect library dependencies separately from the HTML. The browser loads missing files before inserting the update and running its initialization JavaScript. Already loaded or loading files are shared, including individual files originally loaded in bundles. `collect` does not delay dependencies beyond the update, and updates wait for their libraries even when a declaration specifies `async` or `defer`.

The template-model path `model/template/get/render_with_assets/<template>` returns `html`, `libs` and `javascript`. Apply this result with `z_assets.apply(result, insertHtml)` so the dependencies load before the insertion callback, followed by the collected JavaScript. The existing `render` path returns HTML only.

`z_template:render_to_iolist/3` resolves collected files at the materialization boundary. Once converted to HTML, a fragment cannot be deduplicated against later fragments. Retain the render tree from `z_template:render/3` when composing or caching fragments that need to share collection and duplicate tracking.

Erlang components can guard JavaScript with library dependencies:

```erlang
Script = z_render:script_with_libs(
    [<<"js/modules/z.live.js">>],
    <<"z_live.subscribe(/* ... */);">>,
    Context),
z_render:add_script(Script, Context).
```

The API returns JavaScript and also works in a `{javascript, Script}` render node.
Use paths relative to `priv/lib`, without the URL's `/lib/` prefix. Dependencies
use timestamped URLs and share the browser's loaded/loading registry. The wrapped
script runs asynchronously after loading; put all dependent code inside it. A
failed load skips the script and logs the error. The `{% live %}` tag and MQTT
wires use this API automatically.
