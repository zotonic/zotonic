# Tiptap Editor

The [module reference](src/mod_editor_tiptap.erl) contains the published reference
documentation and controlled `zotonic_keywords` metadata for zotonic.com.

Enable `mod_editor_tiptap` instead of `mod_editor_tinymce`. The module depends on
`mod_base` and `mod_admin`, but **does not depend on TinyMCE**. It supplies its own
templates, media-properties handler, CSS, and bundled JavaScript. Existing
`_editor.tpl` includes and `z_editor.init/add/save/remove` calls continue to work.
It has higher template priority than the TinyMCE module, but enabling only one
editor module is recommended.

```django
{% include "_editor.tpl" id=id %}
<textarea id="body" name="body" class="z_editor-init">{{ id.body|escape }}</textarea>
```

Use `do_zeditor` for the existing widget-based initialization. Both DOM elements
and jQuery collections work with `z_editor.add`, `save`, and `remove`. Wired form
submissions flush the editors to their original textareas. A plain form also
flushes on its submit event; code that serializes a form directly must first call
`z_editor.save(form)`.

## Editing

Includes headings, emphasis, lists, indentation, quotes, links, Zotonic internal
links and media, tables, undo/redo, and full-window editing. Additional toolbar
commands include alignment, text direction, subscript, superscript, code blocks,
horizontal rules, and line breaks. Select a media item and use the media button,
or double-click it, to edit its caption, alignment, size, cropping, and link.

Editors are initialized near the viewport, including when language tabs become
visible. Visited editors remain alive to retain undo history. Explicit removal
saves and destroys an editor; adding it again starts a new undo history. Removed
AJAX fragments are cleaned up automatically. Dynamic inserted textareas are
discovered automatically, and the existing explicit init hooks still work.

Initialization does not rewrite untouched textareas. Edited text is serialized
using the configured document schema. Existing Zotonic `z-media` comments are
converted to media nodes and back, preserving options including site-specific
keys. Standard HTML and table structure are supported, but arbitrary HTML,
embedded scripts/iframes, and every TinyMCE style/element option are not.
Review representative existing content before switching a site. Tiptap plugins
replace TinyMCE plugins; their configuration objects are not interchangeable.

Pasting cleans Office typography and reconstructs basic `mso-list` paragraphs.
This is not the commercial Tiptap Office paste handler. Complex Office numbering,
embedded images, footnotes, and exact document layout are not guaranteed. Add
site fixtures for the Word/Google Docs documents your editors actually use.

## Profiles and extensions

Add JavaScript to your site's `_editor_tiptap_overrides_js.tpl`. This template is
already included inside a `{% javascript %}` block. The default profile is merged
with the profile named by the textarea's `data-zeditor` configuration, or its name
without a language suffix (`body$nl` selects `body`).
The merge is shallow: arrays such as `toolbar` and `extensions` replace the
default profile's arrays. Profiles are read when an editor is created, so changing
a profile does not reconfigure already-mounted editors. There are no `m.config`
settings for this module.

```javascript
window.z_editor_tiptap_config = {
    default: {
        toolbar: ['undo', 'redo', '|', 'format', 'bold', 'italic', '|',
                  'bulletList', 'orderedList', '|', 'link', 'zlink', 'zmedia',
                  'table', '|', 'fullscreen']
    },
    summary: { toolbar: ['bold', 'italic', 'link', 'unlink'] },
    admin_frontend: { toolbar: ['format', 'bold', 'italic', 'zlink', 'zmedia', 'fullscreen'] }
};
```

```html
<textarea name="body$en" class="z_editor-init"
          data-zeditor='{"config":"admin_frontend"}'></textarea>
```

Changing the toolbar does not narrow the document schema, so reduced interfaces
can preserve content created in the full editor. Available toolbar names are in
`priv/lib-src/tiptap/toolbar.js`. A profile can also provide:

- `extensions`: an array of Tiptap extensions, or a function receiving
  `{ Editor, Extension, Node, Mark }` and returning an array. Use this to add
  site-specific nodes, marks, attributes, or ProseMirror plugins.
- `buttons`: custom controls keyed by toolbar name, with `label`, optional Lucide
  `icon` data, and `run(editor)`. Include each key in the toolbar array.
- `transformPastedHTML(html)`: replaces the default paste normalizer.
- `onCreate(editor, textarea)`: additional integration after mounting.

`z_editor_tiptap.get(textareaId)` returns a mounted Tiptap editor. The constructors
are also exposed on `z_editor_tiptap`. Keep additional extensions built against
the pinned Tiptap/ProseMirror versions to avoid duplicate plugin implementations.

The frontend admin's legacy `_admin_frontend_tinymce_init.tpl` hook is overridden
with a Tiptap-only hook. `tinyInit`, TinyMCE `overrides_tpl`, and TinyMCE plugin
settings are intentionally not evaluated; migrate them to the profiles above.
An `_editor.tpl` include can select `tiptap_overrides_tpl` instead.

The include also accepts `is_editor_include` to skip its initial
`z_editor_init()` call, and `zmedia_tabs_enabled`, `zmedia_tabs_disabled`,
`zlink_tabs_enabled`, and `zlink_tabs_disabled` to configure the shared chooser
dialogs. The media chooser's disabled list defaults to `["new"]`.

## Styling and dialogs

`priv/lib/css/editor-tiptap-body.css` defines generic Zotonic document typography,
tables, and media alignment. A site can override this asset, or include additional
CSS from `_editor_tiptap_head.tpl`, loaded after the defaults. Scope body rules
under `.z-tiptap-body`; toolbar/layout rules live in `editor-tiptap.css`.
These CSS files are source assets and do not require a CSS compiler.

The selection dialogs are shared **admin** templates. Their callbacks point
directly to `z_editor_tiptap.linkDone/mediaDone`; neither `admin-common.js`'s old
editor callbacks nor any TinyMCE JavaScript are needed. The properties dialog is
`_editor_tiptap_media_props.tpl`, with overridable caption, alignment, size, crop,
class, link, and edit-button blocks. A site-specific class control should be named
`class`. The preview uses the shared `admin_media_preview` dispatch.

Selection bookmarks are retained while dialogs are open and mapped through
transactions. Callbacks for destroyed editors are ignored. Full-window mode
preserves the editor instance and leaves Zotonic dialogs above it.

## Permissions

The `media_props` postback notification accepts a resource `id`, JSON `options`,
and a dialog `request_id`. It requires an authenticated user and a visible
resource; it opens the properties dialog without changing resource data.
Saving body text remains the responsibility of the normal ACL-checked and
HTML-sanitizing Zotonic save paths. Client-side URL checks and read-only state
are not authorization boundaries.

## Build and test

Generated CSS, the browser bundle and third-party license notices are checked in.
SCSS sources live in `priv/lib-src/scss/`; the JavaScript package, build script
and tests live in `priv/lib-src/tiptap/`.

The default build (also available as `make css`) only compiles stylesheets using
Dart Sass (`sass` on PATH, or override `SASS`). It does not run npm or the
JavaScript Makefile. Node.js/npm are needed only for the explicit `js` and
`test` targets:

```sh
make -C apps/zotonic_mod_editor_tiptap
make -C apps/zotonic_mod_editor_tiptap js
make -C apps/zotonic_mod_editor_tiptap test
./rebar3 compile
```

Dependencies are pinned in `priv/lib-src/tiptap/package-lock.json`. The JS build uses
esbuild and emits `priv/lib/js/zotonic-editor-tiptap.js` plus license notices for
the packages included in the bundle. Tests cover content conversion, paste,
multiple editors, profiles, dialogs, cleanup, saving, and full-window lifecycle.

Yjs, a Cotonic collaboration provider, and collaborative persistence are deferred.
