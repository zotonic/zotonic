---
name: zotonic-elm-to-templates
description: Use when converting Elm-generated Zotonic pages to Zotonic template_compiler templates. Focuses on extracting Elm routes/views into templates, using resource page paths, category templates, catinclude, page headers, and avoiding language-specific dispatch.
---

# Zotonic Elm To Templates

## Goal

Convert Elm-rendered pages into Zotonic templates while preserving behavior and URLs through Zotonic resources, dispatch, and template inheritance.

## Discovery

- Read the Elm files first, especially route definitions, page/view modules, navigation, footer, forms, and any hard-coded page paths.
- Create a path inventory in `doc/` when migrating routes: list resource names and all language page paths found in Elm. Pad table columns for human review when requested.
- Use live or old-site HTML/CSS only as reference; implement the result in Zotonic templates.

## Routing

- Zotonic handles language path prefixes. Do not add `/en`, `/nl`, etc. dispatch variants.
- Keep dispatch names language-neutral, for example `activities_agenda`, not `activities_agenda_en`.
- Keep English route names in dispatch for now unless the user asks for localized route names.
- If a URL points to a named page resource, use `m.rsc.resource_name.page_url` instead of adding a dispatch rule.
- Use `{% url dispatch_name %}` for non-resource controller/dispatch routes.

## Template Structure

- Put templates in the app/site `priv/templates`.
- Use English as the source language for all static template text. Move Dutch or other old-site literals into PO translations instead of keeping them in templates.
- Wrap user-visible text with `{_ ... _}`. For translated include arguments, use `title=_"Text"`.
- Translate Elm view structure into semantic HTML, not a one-to-one div tree. Prefer `header`, `nav`, `main`, `section`, `article`, and `footer`.
- Preserve accessibility while converting: keep useful image alt text, add missing `aria-*` attributes where controls need labels, and use lowercase HTML tags/attributes.
- Treat URL/query values from `q` as unsafe in templates unless escaped or sanitized. `m.rsc` values are sanitized; values from other models should be escaped or otherwise checked.
- Use:

```django
{% extends "page.tpl" %}
```

for page variants, and let `page.tpl` extend `base.tpl`.

- Use category-specific filenames:
  - `page.event.tpl` for resources in category `event`
  - `page.building_private.tpl` for category `building_private`
  - `page.name.home.tpl` only for specific named resources when a normal `home.tpl` is not the intended template
- Remove one-use include templates by inlining their content.
- Move grouped partials into directories when useful, for example:
  - `form/_form_*.tpl`
  - `icon/_icon_*.tpl`
  - `page-header/_page_header*.tpl`
- If converted pages use images, check `priv/templates/mediaclass.config` for existing mediaclasses before inventing image sizes.

## Category Selection

- Prefer `{% catinclude %}` where the page/header/body varies by resource category.
- Typical pattern:

```django
{% catinclude "page-header/_page_header.tpl" id %}
```

- Implement category variants as files that match the category-aware catinclude pattern.
- Before creating category templates, inspect fixture categories and Elm page modules. If a category has a matching Elm page view, create the corresponding `page.category.tpl`.

## Base And Standard Includes

- Use Zotonic standard includes:
  - `{% all include "_html_head.tpl" %}`
  - standard body all-includes such as `_html_body` as seen in local Zotonic 1.x sites
- Remove head/body tags already provided by Zotonic modules.
- Remove Google Tag Manager snippets when `mod_seo` provides them.
- Check comparable Zotonic 1.x sites in the workspace for `html` and `body` attributes used by Cotonic and Zotonic.

## Navigation And Footer

- Translate all Elm navigation/footer elements, including ticket buttons, search, language switch, social links, and image/icon buttons.
- Preserve translations by converting Elm language-specific text into English source msgids plus PO entries.
- Avoid duplicating the language switch.
- Preserve visual affordances from the live site, such as image-based back-to-top/continue controls and arrow icons in footer buttons.
- Replace hard-coded internal links with `{% url ... %}` or `m.rsc.name.page_url`.

## Translation Workflow

- Do not regenerate or commit POT files during normal conversion work. Zotonic POT files are generated on the `master` branch with:

```sh
bin/zotonic pot zotonic
```

- The command uses the running Zotonic server. If a feature/test command creates POT diffs, restore or leave them out unless the user explicitly asks to update POT files.
- Merge existing language files with `msgmerge --backup=none --update`.
- Add missing languages with `msginit`, for example `de.po`.
- Fill translations for newly introduced English msgids, especially labels, email text, admin help text, and form copy.
- Validate with `msgfmt --check --output-file=/dev/null`.
- Scan templates for old Dutch/source strings after edits; keep frontend templates language-neutral and translatable.

## CSS And Layout Checks

- After template/CSS changes, check for horizontal scrolling, logo alignment, missing icons, and duplicated UI.
- Prefer fixing layout at the source: oversized width, viewport-width elements, negative margins, or uncontained images.
- Avoid inline styles from Elm output unless they are truly dynamic; prefer existing SCSS/CSS patterns in `priv/lib-src`.
