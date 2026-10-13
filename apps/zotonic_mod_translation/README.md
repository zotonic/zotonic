# Zotonic Translation

`mod_translation` manages multilingual content, language selection and recognition,
language-prefixed URLs, and translation administration. See the
[module reference](src/mod_translation.erl) for configuration, APIs, templates,
permissions and the full language-recognition profile list.

## Languages and rendering

Configure languages under **Structure > Translation**. URL language prefixes are
enabled by default; disable them with `mod_translation.rewrite_url = false`.
The site default language is the first fallback unless
`i18n.default_language_is_fallback = false`.

Use the `set_language` postback to switch languages and `_language_attrs.tpl` to
render language and text-direction attributes, including right-to-left content.
Resources without a language can use local n-gram recognition. Training texts
and profiles live in `priv/data/texts` and `priv/data/profiles`; the module reference
explains how to generate and install profiles. Recognition does not translate text.

## Editing and translation

- The page translation dialog can automatically translate, copy source texts,
  enable an empty language, or remove a translation. Review and save the edit form
  to persist these changes. Automatic translation requires a service module and
  may send text to a remote service.
- The translations overview offers click-to-edit dialogs for individual properties
  and named block fields. Rich text uses the enabled editor; plain text uses inputs
  or textareas. **Translate from** fills the draft from another saved language.
  Save updates only that property and language, with version checks protecting
  against concurrent changes. Open edit pages warn when they become stale.
- **Bulk update > Manage translations** processes selected pages without following
  descendants. **Translate all pages** processes a menu or collection tree,
  including nested menu entries and `haspart` connections. These background jobs
  save immediately, report progress and failures, and recheck edit permissions.
  Save or discard pending edits before starting them.

Copying and translation preserve existing destination text unless overwrite is
selected. Bulk and tree removal keep a page's last language. Structured survey
answer lists and unsafe fields are not editable in the single-field dialog;
slugs require **Customize page slug**.

## Integration

`m_translation:add_translation/5` translates and saves a resource;
`add_translation_map/5` works on an in-memory map. The shared resource translator
handles known text properties and nested blocks. `m_translation_tree` exposes
tree counts, job startup and progress; custom editors can use
`_translation_tree_button.tpl` and `_translation_tree_init.tpl`.

Normal ACL checks and HTML sanitization remain in force. Custom admin forms must
include `_admin_edit_version.tpl` for version-checked saves. Consult the module
reference before implementing custom translation workflows or progress listeners.
