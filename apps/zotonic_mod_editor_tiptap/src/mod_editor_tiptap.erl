%% Copyright 2026 Zotonic
%% Licensed under the Apache License, Version 2.0.
-module(mod_editor_tiptap).
-mod_title("Tiptap Editor").
-mod_description("Rich text editor with Zotonic links and media, using Tiptap.").
-mod_prio(400).
-mod_depends([mod_base, mod_admin]).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "content_editor", "module",
        "content_authoring", "media_management", "edit", "html", "javascript"
    ]
}).
-moduledoc(<<"
Adds a rich-text editor based on Tiptap and ProseMirror to Zotonic forms.
It provides headings, emphasis, lists, links, Zotonic media, tables, undo/redo,
and full-window editing through a plain JavaScript interface.

## Setup

Enable `mod_editor_tiptap` instead of `mod_editor_tinymce` on the site's Modules
page. It depends on `mod_base` and `mod_admin`, but does not require TinyMCE to be
enabled or load any TinyMCE templates or JavaScript. Existing `_editor.tpl`
includes select this editor when the module is enabled.

```django
{% include \"_editor.tpl\" id=id %}
<textarea id=\"body\" name=\"body\" class=\"z_editor-init\">{{ id.body|escape }}</textarea>
```

The page must include Zotonic's normal JavaScript and `{% script %}` support.
Textareas marked `do_zeditor` can use the existing widget initialization instead.
The original textarea remains the form field; the editor synchronizes its value
before a wired or native form submission.

## Editor lifecycle

Editors initialize near the viewport. Call `z_editor.init()` after showing a
language tab or otherwise revealing fields; the standard admin already does so.
Visited editors stay alive when tabs are hidden, preserving their undo history.
New textareas in AJAX fragments are discovered automatically, and editors whose
elements are removed are destroyed.

| JavaScript API | Behavior |
| --- | --- |
| `z_editor.init()` | Discover editor textareas throughout the document. |
| `z_editor.add(root)` | Discover editor textareas in the supplied element or container. |
| `z_editor.save(root)` | Write changed editor content to textareas in that scope. |
| `z_editor.remove(root)` | Save and destroy editors in that scope and cancel pending initialization. |
| `z_editor_tiptap.get(textareaId)` | Return the mounted Tiptap editor, or `undefined` before initialization or after removal. |

The root argument accepts a DOM element, selector, or jQuery collection. Omitting
it selects the document. Before directly serializing a form, call
`z_editor.save(form)`. Explicit removal followed by addition starts a new undo
history. Content edits emit the existing `z:editorChange` form notification.
Full-window editing preserves the instance; Escape returns to the normal view.

## Profiles and extensions

This module has no `m.config` settings. Configure browser-side profiles in the
site's `_editor_tiptap_overrides_js.tpl`. That template contains JavaScript only:
it is already included inside a `{% javascript %}` block.

```javascript
window.z_editor_tiptap_config = {
    default: {
        toolbar: ['undo', 'redo', '|', 'format', 'bold', 'italic', '|',
                  'bulletList', 'orderedList', '|', 'link', 'zlink', 'zmedia',
                  'table', '|', 'fullscreen']
    },
    summary: { toolbar: ['bold', 'italic', 'link', 'unlink'] }
};
```

Profile selection uses `data-zeditor`'s `config` value, falling back to the field
name without its language suffix: `body$nl` selects `body`. The selected profile
is shallow-merged over `default`; arrays such as `toolbar` and `extensions` are
replaced, not concatenated. Profiles apply when an editor is created.

```html
<textarea name=\"summary$en\" class=\"z_editor-init\"
          data-zeditor='{\"config\":\"summary\"}'></textarea>
```

| Profile option | Purpose |
| --- | --- |
| `toolbar` | Ordered array of control names; `|` inserts a separator. |
| `extensions` | Additional Tiptap extensions, or a function receiving `{ Editor, Extension, Node, Mark }` and returning an array. |
| `buttons` | Custom controls keyed by toolbar name, each with `label`, optional Lucide `icon` data, and `run(editor)`. |
| `transformPastedHTML(html)` | Replace the default paste normalizer. |
| `onCreate(editor, textarea)` | Run additional integration after mounting. |

Toolbar controls include `undo`, `redo`, `format`, `bold`, `italic`, `underline`,
`strike`, `subscript`, `superscript`, `bulletList`, `orderedList`, `indent`,
`outdent`, `blockquote`, `codeBlock`, `alignLeft`, `alignCenter`, `alignRight`,
`ltr`, `rtl`, `horizontalRule`, `hardBreak`, `link`, `unlink`, `zlink`, `zmedia`,
`table`, `removeFormat`, and `fullscreen`.

A smaller toolbar does not remove content types from the document schema, so
reduced frontend forms can preserve content created in the full admin editor.
The extension constructors are also exposed on `z_editor_tiptap`. Build custom
extensions against the bundled Tiptap/ProseMirror versions.

## Template options

`_editor.tpl` accepts these integration options:

| Option | Purpose |
| --- | --- |
| `id` | Resource used as the subject of link/media selection dialogs. |
| `tiptap_overrides_tpl` | JavaScript configuration include; defaults to `_editor_tiptap_overrides_js.tpl`. |
| `is_editor_include` | Load assets and configure the editor without calling `z_editor_init()` in this include. |
| `zmedia_tabs_enabled`, `zmedia_tabs_disabled` | Media chooser tab lists; the disabled list defaults to `[\"new\"]`. |
| `zlink_tabs_enabled`, `zlink_tabs_disabled` | Internal-link chooser tab lists. |

The compatibility templates `_admin_tinymce.tpl` and
`_admin_frontend_tinymce_init.tpl` integrate older callers and frontend admin
without loading TinyMCE. TinyMCE's `tinyInit`, `overrides_tpl`, and plugin settings
are not interpreted; migrate them to Tiptap profiles and extensions.

## Styles and media

Generic document typography is defined in `priv/lib/css/editor-tiptap-body.css`.
Override that asset in the site, or add styles through `_editor_tiptap_head.tpl`,
which loads after the defaults. Scope content styles under `.z-tiptap-body`.
Editor controls and structural styles are in `css/editor-tiptap.css`. The content
is ordinary page DOM, not an iframe, so site CSS can also affect it.

The link/media selection dialogs come from `mod_admin`; their callbacks are
`z_editor_tiptap.linkDone` and `z_editor_tiptap.mediaDone`. Select a media item and
use the media button, or double-click it, to open its properties. The module owns
`_editor_tiptap_media_props.tpl` and `_editor_tiptap_media_preview.tpl`; the latter
uses the shared `admin_media_preview` dispatch. The properties template exposes
blocks for caption help, alignment, size, crop, class, link, and the edit button.
Name a site-specific class control `class`.

Media nodes serialize back to Zotonic `z-media` HTML comments, including their
site-specific options. Render saved bodies through the existing `show_media`
filter. Dialog selections are mapped through intervening editor transactions;
callbacks for removed editors are ignored.

## Permissions and events

The `media_props` `postback_notify`, delegated to `mod_editor_tiptap`, accepts
`id`, JSON-encoded `options`, and `request_id` for the originating dialog.
It requires an authenticated user and a visible resource. Invalid or inaccessible
resources produce an error notification. Malformed option JSON becomes an empty
options map; only the known string/boolean properties are passed to the template.
The handler opens a dialog and does not update a resource.

The editor does not grant permission to save content. Resource updates must use
the normal ACL-checked and HTML-sanitizing Zotonic save paths. Browser-side URL
checks and read-only controls are not server-side authorization.

## Content compatibility

Untouched textareas are not rewritten. After editing, content is serialized using
the supported schema. Standard text formatting, images, links, lists, and tables
are supported; arbitrary HTML, scripts/iframes, and every TinyMCE style or element
are not. Test representative existing bodies before migrating a site.

Paste cleanup removes Office typography and reconstructs basic `mso-list`
paragraphs. It is not the commercial Tiptap Office paste handler: complex list
numbering, embedded images, footnotes, and exact Word layout are not guaranteed.
Cooperative editing with Yjs/Cotonic is not included.

## Building

Browser assets and third-party license notices ship in `priv/lib`. No runtime
CDN or Node.js service is required. SCSS sources live in `priv/lib-src/scss/`;
the JavaScript package and tests live in `priv/lib-src/tiptap/`.

The default build (or `make css`) compiles only CSS using Dart Sass (`sass` on
PATH, overridable with `SASS`). It never runs the JavaScript Makefile or npm.
Use the explicit `js` target to rebuild the browser bundle:

```sh
make -C apps/zotonic_mod_editor_tiptap
make -C apps/zotonic_mod_editor_tiptap js
make -C apps/zotonic_mod_editor_tiptap test
./rebar3 compile
```

Node.js/npm are development dependencies. Package versions are pinned in
`priv/lib-src/tiptap/package-lock.json`; the JS build emits the JavaScript bundle and its
third-party license notices. Zotonic integration code is Apache-2.0 licensed.
">>).

-export([event/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

-spec event(Event, Context) -> Context1 when
    Event :: #postback_notify{},
    Context :: z:context(),
    Context1 :: z:context().
event(#postback_notify{message = <<"media_props">>}, Context) ->
    Id = m_rsc:rid(z_context:get_q(<<"id">>, Context), Context),
    case z_acl:user(Context) =/= undefined
        andalso Id =/= undefined
        andalso m_rsc:is_visible(Id, Context)
    of
        true ->
            z_render:dialog(
                ?__("Media Properties", Context),
                "_editor_tiptap_media_props.tpl",
                [
                    {id, Id},
                    {options, media_options(z_context:get_q(<<"options">>, Context))},
                    {request_id, z_context:get_q(<<"request_id">>, Context)},
                    {level, 5}
                ],
                Context);
        false ->
            z_render:growl_error(?__("You are not allowed to view this media.", Context), Context)
    end.

media_options(Json) when is_binary(Json) ->
    try
        z_json:decode(Json)
    of
        Options when is_map(Options) ->
            maps:filter(
                fun(_K, V) -> is_binary(V) orelse is_boolean(V) end,
                maps:with([
                        <<"caption">>, <<"align">>, <<"size">>, <<"crop">>,
                        <<"class">>, <<"link">>, <<"link_new">>, <<"link_url">>
                    ], Options));
        _ ->
            #{}
    catch
        _:_ -> #{}
    end;
media_options(_) ->
    #{}.
