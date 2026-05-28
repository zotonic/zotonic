---
name: zotonic-template-creation
description: Use when creating, refactoring, or reviewing Zotonic template_compiler templates in priv/templates, including page/base templates, partials, category-specific templates, catinclude/catcompose usage, translations, template inheritance, template filters, scomps, and standard html/body/head setup for Zotonic sites and modules.
---

# Zotonic Template Creation

## First Pass

- Inspect nearby templates before editing. Preserve local structure, blocks, naming, and CSS class conventions.
- Put templates under the app or site `priv/templates` directory.
- Use Zotonic `template_compiler` syntax, which is Django-like but has Zotonic-specific tags, models, scomps, category-aware includes, and runtime compilation.
- Use semantic HTML and accessible attributes; avoid inline styles unless the value is truly dynamic.


## Security

- Treat template output as HTML unless proven otherwise. Escape data at trust boundaries.
- Properties from the `m.rsc` model are generally sanitized/pre-escaped and safe to render directly, for example `{{ id.title }}` or `{{ m.rsc[id].summary }}`.
- Do not assume all `m.rsc` values are safe: properties explicitly marked or documented as unsafe, such as names ending in `_unsafe`, must be escaped or sanitized before output.
- Output from other models must be escaped when rendered into templates unless that model explicitly documents the returned value as safe HTML. This includes request, identity, EXIF, medium metadata, and custom model output.
- Access query and request arguments with `q`, for example `{{ q.qs|escape }}`. Values from `q` always come from outside and must be considered unsafe.
- Values from `q` can be anything accepted by request/API handling, including strings, booleans, structured maps/lists, or upload records such as `#upload{}`; never bind an unsafe `q` value to a trusted variable name such as `id`.
- When a query id is needed as a resource, sanitize through the resource model: prefer `{% with m.rsc[q.id].id as id %}` over `{% with q.id as id %}`.
- Follow the Zotonic XSS guidance in `https://zotonic.com/cookbook/2325/security-templates-and-xss-prevention`: escape `q.*`, use sanitized controller/resource ids directly, and keep untrusted variables visibly named as untrusted, for example `qid`.

## Template Layout

- Make site page templates extend a common base, usually `{% extends "base.tpl" %}` or `{% extends "page.tpl" %}`.
- If a template uses `{% extends %}` or `{% overrules %}`, make that the first template tag. The template should then contain block overrides, not arbitrary top-level page markup.
- `extends`, `overrules`, `inherit`, `compose`, `catcompose`, `use`, and `useblock` accept extra arguments; the optional `with` keyword is accepted for compatibility.
- Use `{% inherit %}` inside an overridden block to render the inherited block body, optionally with extra variables.
- Use `{% compose %}`/`{% catcompose %}` when the caller needs to include a template and override named blocks inside it.
- Use fragments with `{% fragment %}`, `{% use %}`, and `{% useblock %}` for reusable pieces inside an inheritance tree.

## Base Templates

- A normal site base should include language, environment, Cotonic, module head/body hooks, JS collection, and script flush support.
- Put language on `<html>` using `z_language`, for example `lang="{{ z_language|default:"en"|escape }}"`; include `xml:lang` when the local site pattern does.
- Add an environment class on `<html>`: `class="environment-{{ m.site.environment }}"` or include it alongside other root classes.
- Include `{% all include "_html_head.tpl" %}` in `<head>` so modules can add Cotonic setup, SEO, theme initialization, feeds, and other head fragments.
- Include `{% all include "_html_body.tpl" %}` near the end of `<body>` so modules can add required body fragments.
- Include `{% include "_js_include.tpl" %}` when the site has a JS include partial.
- End the body with `{% script %}` so collected `{% javascript %}`, `{% wire %}`, and related scomp output is emitted.
- Add `data-cotonic-pathname-search="{% cotonic_pathname_search %}"` to `<body>` for Cotonic UI components.
- Use body classes like `page-{{ id.name }}` and `cat-{{ cat }}` for the current resource and its categories.
- Add `{% block html_attr %}`, `{% block body_class %}`, and `{% block body_attrs %}` or the local equivalents so pages can add attributes.
- For admin-like bases, follow `admin_base.tpl`: use `_language_attrs.tpl`, `zotonic-admin`, `_html_head_admin.tpl`, `_html_body_admin.tpl`, `data-cotonic-pathname-search`, admin JS includes, and `{% script %}`.

## Includes And Subfolders

- Prefix partial template names with `_` when they are not directly routed pages.
- Use subfolders to group related partials in larger sites, following patterns such as `cards/`, `header/`, `nav/`, `footer/`, `search/`, `page/`, `page-actions/`, `edit/`, `icons/`, `email/`, `pivot/`, or feature-specific folders.
- Keep path and category naming together for category-aware partials, for example `cards/card.tpl`, `cards/card.event.tpl`, `header/header.tpl`, `header/header.name.page_art_agenda.tpl`, `page/content-left.tpl`, `page/content-left.person.tpl`.
- Use `{% include "partial.tpl" arg=value %}` for a single known partial.
- Use `{% optional include "partial.tpl" %}` when the partial can be absent.
- Use `{% all include "_html_head.tpl" %}` or `{% all include "_admin_overview_filter_panel.tpl" %}` when all modules should contribute same-named templates in module priority order.
- Use include cache arguments (`max_age`, `vary`, `sudo`, `anondo`, `runtime`) only when needed; be explicit because they affect ACL/runtime behavior.

## Category Templates

- Use category-specific template filenames for resources and partials: `page.event.tpl`, `page.name.page_home.tpl`, `_admin_edit_content.person.tpl`, `cards/card.artwork.tpl`.
- Use `{% catinclude "base.tpl" id %}` when a resource category or unique resource name should choose the template.
- For a resource named `my_page_name` in category `news` under `article` under `text`, `{% catinclude "hello.tpl" id %}` searches `hello.name.my_page_name.tpl`, `hello.news.tpl`, `hello.article.tpl`, `hello.text.tpl`, then `hello.tpl`.
- When passing category names instead of a resource, put the most specific category last: ``{% catinclude "hello.tpl" [`text`, `article`] %}`` searches `hello.article.tpl`, then `hello.text.tpl`, then `hello.tpl`.
- Category names are mixed into the full basename before `.tpl`, preserving subfolders and prefixes. `catinclude "cards/card.tpl" event_id` looks for `cards/card.name.NAME.tpl`, `cards/card.event.tpl`, and `cards/card.tpl`.
- Use `{% all catinclude "partial.tpl" id %}` when all matching category templates from all modules must render, as in admin edit panels.
- Prefer `catinclude`/`catcompose` over large `if id.is_a...` switches when category-specific rendering is intended.

## Translations

- Keep template source strings in English.
- Wrap user-facing static text in `{_ Text _}`.
- Use `{% trans "Hello {name}" name=id.title %}` for translated strings with variable substitution.
- For translated tag/scomp/include arguments, use compiler-native translated literals: `text=_"Save"`, `title=_"Save this page."`, or `headline=_"Latest modified opportunities"`.
- In HTML attributes, embed normal translation tags, for example `placeholder="{_ Search _}"`.
- Use `z_language` for the current language. For language-specific resource URLs, use `{{ id.page_url with z_language = code }}`.
- Translation strings are extracted to POT files under `priv/translations/template/`, for example `priv/translations/template/sitename.pot`.
- Generate or refresh site/module translations with `bin/zotonic pot sitename`; this connects to the running Zotonic node.
- For Zotonic core translations, meaning everything under `apps`, generate the POT file with `bin/zotonic pot zotonic`.
- Never update Zotonic core `.po` files directly; they are managed through Crowdin at `crowdin.com`.
- Merge site/user-module PO files with `msgmerge --backup=none --update priv/translations/nl.po priv/translations/template/sitename.pot` and validate with `msgfmt --check --output-file=/dev/null priv/translations/nl.po`.
- Use `_translations.tpl` for strings that must be extracted but are not otherwise visible in templates.

## Tags And Filters

- Read tag documentation in `doc/template-tags/` when using unfamiliar tags. Important files include `extends.md`, `overrules.md`, `include.md`, `all_include.md`, `catinclude.md`, `all_catinclude.md`, `compose.md`, `catcompose.md`, `fragment.md`, `use.md`, `useblock.md`, `inherit.md`, `trans.md`, `trans_ext.md`, `url.md`, `lib.md`, `image.md`, and `media.md`.
- Built-in tags handled by Zotonic/template_compiler include control and inheritance tags such as `for`, `if`, `block`, `extends`, `overrules`, `include`, `catinclude`, `all include`, `comment`, `with`, `cache`, `filter`, `spaceless`, `javascript`, and `trans`.
- Zotonic runtime built-in tags include `url`, `lib`, `lib_url`, `image`, `image_url`, `image_data_url`, and `media`.
- Unknown non-built-in tags are custom Zotonic scomps. Find them under `src/scomps/` as `scomp_<module>_<tag>.erl`; examples include `cotonic_pathname_search`, `wire`, `button`, `pager`, `menu`, and `live` depending on enabled modules.
- Template filters are Erlang modules under `src/filters/` named `filter_name.erl`. The template filter `{{ value|summary:120 }}` maps to `filter_summary:summary(Value, 120, Context)`.
- Check `apps/zotonic_mod_base/src/filters/` first for core filters, then app/module `src/filters/` directories for module-specific filters.
- Filters should have a `-moduledoc` in their `src/filters/filter_*.erl` source file; use that source documentation as the reference.
- `default`, `default_if_none`, and `default_if_undefined` are inlined by `template_compiler`; many other filters call `filter_<name>` modules.
- Use escaping filters such as `escape`, `escapejs`, `escapejson`, `sanitize_html`, `sanitize_url`, and `urlencode` at trust boundaries.


## Scomps

- Scomps are screen components: server-side template tags that render HTML, JavaScript, or wire actions from Erlang.
- Use scomps as normal template tags, for example:
  ```django
  {% button text=_"Save" action={submit} %}
  {% wire id="form" type="submit" postback={save id=id} %}
  {% pager result=result dispatch="page" %}
  {% menu id=`main_menu` %}
  ```
- Unknown non-built-in tags are usually scomps. Find their implementation under `src/scomps/` as `scomp_<module>_<tag>.erl`.
- Scomps should have a `-moduledoc` that documents accepted arguments, generated markup, emitted JavaScript, postbacks, and security assumptions.
- Prefer existing scomps for common Zotonic behavior such as buttons, wires, validation, live updates, sortable lists, menus, tabs, pagers, and Cotonic integration.
- Keep scomp arguments explicit and translated where user-facing, for example `text=_"Delete"` and `title=_"Delete this page."`.
- If a scomp collects JavaScript or wires, ensure the base template ends with `{% script %}`.

## Forms

- Use normal HTML forms and wire them with Zotonic when the result should be handled by Erlang: ``{% wire id="contact-form" type="submit" postback={contact []} delegate=`mod_contact` %}``.
- Forms that submit by postback normally use `method="post" action="postback"` and have a stable `id`.
- Use `{% button %}` or action `{submit}` when the local template pattern uses Zotonic buttons instead of raw submit buttons.
- In the receiving `event(#submit{}, Context)`, fetch fields with `z_context:get_q/2`, `z_context:get_q_all/1`, or `z_context:get_q_validated/2`; never trust client-side validation alone.
- Use `z_context:get_q_all_noz/1` or `z_context:get_q_map_noz/1` when processing ordinary form fields and excluding Zotonic internal parameters.
- Add validation with `{% validate %}` next to the field being validated. The validation tag emits LiveValidation JavaScript and also enables server-side validation for submit/postback handling.

```django
{% wire id="contact-form" type="submit" postback={contact []} delegate=`mod_contact` %}
<form id="contact-form" method="post" action="postback">
    <input id="mail" name="mail" type="email">
    {% validate id="mail" type={presence} type={email} %}

    <textarea id="message" name="message"></textarea>
    {% validate id="message" type={presence failure_message=_"Please enter a message."} %}

    <button type="submit">{_ Send _}</button>
</form>
```

- The `id` argument of `{% validate %}` is the input element id. Use `name="field_name"` when the submitted field name differs from the element id.
- Multiple `type={...}` arguments can be used on the same field, for example `type={presence} type={email}`.
- Common validation options include `failure_message`, `valid_message`, `message_after`, `only_on_blur`, `only_on_submit`, `wait`, `trigger`, and `target`.
- Use generated ids such as `id=#email name="email"` when a partial can be rendered multiple times on one page.
- File inputs can use the `presence` validator; submitted uploads arrive as `#upload{}` values.
- Include `_js_include.tpl`/admin JS includes and end the page with `{% script %}` so LiveValidation, validators, wires, and form postbacks are initialized.

## Validators

- Template validators are implemented by Erlang modules in `src/validators/` named `validator_<module>_<name>.erl`.
- Validator type names in templates map to those modules. `{% validate id="email" type={email} %}` resolves to `validator_base_email`; module-specific validators include examples such as `validator_admin_identity_username_unique`.
- Core validators are in `apps/zotonic_mod_base/src/validators/`; inspect their `-moduledoc` before using unfamiliar arguments.
- Common core validators include `presence`, `email`, `length`, `numericality`, `format`, `date`, `json`, `acceptance`, `confirmation`, `postback`, `name_unique`, and `page_path_unique`.
- Validators generally implement `render_validator/5` for client-side LiveValidation setup and `validate/5` for server-side checks.
- A validator can return extra validation args from `render_validator/5`; those args are passed to `validate/5` during server validation.
- The `postback` validator performs custom server-side validation through a notification or delegate; use it for checks that need database lookups, ACL checks, or external state.

```django
<input id="username" name="username" type="text">
{% validate id="username" wait=400
            type={presence}
            type={postback event="validate_username"} %}
```

- For a custom validator, add `src/validators/validator_mymodule_slug.erl`, export `render_validator/5` and `validate/5`, include `zotonic.hrl`, and document all template arguments in `-moduledoc`.
- Client-side LiveValidation is for responsiveness; all security and data integrity checks must also happen server-side in the validator, `event/2`, model, or controller.

## Source Documentation

- Filters, scomps, validators, models, and modules should always include a `-moduledoc` in their Erlang source file.
- Use those `-moduledoc` entries as the authoritative local documentation for template-facing behavior.
- Do not rely on `doc/_build`; it was generated by older documentation systems and is not expected to exist.

## Models

- Access template models through `m`, using the model name without the `m_` prefix: `m.rsc`, `m.search`, `m.req`, `m.acl`, `m.config`, `m.category`, `m.media`, `m.edge`, and module-specific models.
- Model paths use dot/index notation. Examples: `{{ m.rsc[id].title }}`, `{{ m.acl.user }}`, `{{ m.req.host|escape }}`, `{{ m.config.site.title.value }}`.
- Use the shorthand `id.title`, `id.summary`, `id.o.haspart`, and `id.s.author` only when `id` is the sanitized page resource or came from `m.rsc`.
- Use search models for resource lists and prefer the `::` payload operator for new search calls: `{% for id in m.search.query::%{ cat: ["news"], pagelen: 10 } %}`.
- Use `{% with %}` to avoid repeating expensive model lookups, especially searches: `{% with m.search.query::%{ cat: ["news"], pagelen: 10 } as latest_news %}`.
- Models are implemented in `src/models/` as `m_name.erl` modules with the `zotonic_model` behaviour. Core models live in `apps/zotonic_core/src/models/`; module models live in each module's `src/models/`.
- Models should have a `-moduledoc` in their `src/models/m_*.erl` source file; use that source documentation as the reference. The old `doc/_build` generated docs are an obsolete artifact and should not be relied on.
- Before using output from a less familiar model, read its source documentation and check whether it performs ACL checks and whether returned values are safe for direct HTML output.

## Model Payloads

- Use the `::` operator to pass a structured payload to a model call. The expression after `::` is passed as the model payload instead of being part of the path.
- Prefer maps for payloads: `m.search.query::%{ cat: ["article"], is_published: true, sort: ["-created"], pagelen: 20, page: q.page }`.
- The path before `::` still selects the model and operation. In `m.search.paged.query::%{ text: q.qs }`, the path is `[paged, query]` and the payload is the map.
- For the search model, prefer `::` over the older tuple/list query syntax. It is clearer, easier to extend, and matches model API payload handling.
- Older tuple-style search syntax, such as `m.search[{latest cat="text" pagelen=10}]`, is still encountered in existing templates and docs but should not be copied for new code.
- Use tuple-style only when maintaining existing code that depends on the deprecated search representation or when the target model explicitly documents tuple input.
- Treat payload values from `q` as unsafe. A structured payload can safely pass `q.page` to the model for validation, but never render `q.*` directly without escaping.

## Resource And URL Patterns

- Prefer `{{ id.title }}`, `{{ id.summary }}`, `{{ id.body }}`, and `{{ id.depiction }}` when `id` is the sanitized page resource.
- Use `m.rsc[unsafe_id]` when the resource id comes from an untrusted source; Zotonic will sanitize through the model.
- Use `{{ m.rsc.unique_name.page_url }}` for named page resources.
- Use `{% url dispatch_name id=id %}` for dispatch/controller URLs.
- Do not hard-code internal URLs when a dispatch rule or resource `page_url` exists.
- Use `{% image id mediaclass="..." alt=id.title %}` or `{% media id %}` for media; define image sizes in `priv/templates/mediaclass.config`.


## CSS And Bootstrap

- Put source styles in `priv/lib-src/` and generated browser assets in `priv/lib/`; templates should include generated CSS with `{% lib "css/file.css" %}` or module/site include templates.
- Do not edit generated CSS in `priv/lib/` when a matching SCSS/LESS source exists. Edit the source file and rerun the relevant Makefile.
- Use the local build pattern. Most style builds are run from the source directory, for example `make -C apps/zotonic_mod_bootstrap/priv/lib-src`, `make -C apps/zotonic_mod_bootstrap/priv/lib-src/bootstrap`, `make -C apps/zotonic_mod_admin/priv/lib-src/admin-bootstrap5`, or `make -C apps_user/zotonicwww2/priv/lib-src`.
- SCSS builds use Dart Sass via the `sass` command; older icon/base styles may still use `lessc`. If a Makefile defines `SASS ?= sass`, override it with `make SASS=/path/to/sass` only when needed.
- `zotonic_mod_bootstrap` keeps Bootstrap sources under `priv/lib-src/bootstrap/scss` and Bootstrap 3 compatibility shims under `priv/lib-src/bootstrap/compat`. Its build emits `priv/lib/bootstrap/css/bootstrap.css` (Bootstrap 5 plus Bootstrap 3 compat), `bootstrap5.css` (Bootstrap 5 without compat), `bootstrap3-compat.css` (compat only), theme CSS, and minified variants.
- Use `bootstrap.css` when existing templates still use Bootstrap 3 class names and need the compatibility layer. Use `bootstrap5.css` only when templates are written for Bootstrap 5 classes or when a site includes compatibility separately.
- The admin has separate Bootstrap builds: `priv/lib-src/admin-bootstrap3` is kept for old Bootstrap 3 output, and `priv/lib-src/admin-bootstrap5` builds `priv/lib/css/admin-bootstrap5.css` and `.min.css` with Bootstrap 5, admin overrides, and the admin Bootstrap 3 compatibility file.
- Admin theme and component styles live in `apps/zotonic_mod_admin/priv/lib-src/zotonic-admin/scss`; prefer CSS custom properties from `_theme.scss`/`_variables.scss` for colors so light/dark themes keep working.
- Bootstrap 3 compatibility files should stay in separate `compat/` directories. Keep them SCSS-friendly with variables and nested selectors where that improves maintainability.
- When adding or changing template classes, first check whether the project is relying on Bootstrap 3 compatibility. Avoid changing large template class sets just to satisfy Bootstrap 5 if compat CSS is intended to preserve them.
- For dark/light theme support, prefer existing CSS variables and add new variables near the theme files rather than hard-coded colors in component partials.
- After SCSS/LESS edits, rerun the relevant Makefile and verify both normal and `.min.css` outputs if the build produces minified files.

## Cotonic

- Cotonic is Zotonic's browser-side JavaScript runtime for isolated components, workers, and MQTT-style publish/subscribe messaging.
- Cotonic exposes `cotonic` on the page, including `cotonic.ready`, `cotonic.broker.publish`, `cotonic.broker.subscribe`, `cotonic.broker.call`, `cotonic.spawn`, `cotonic.spawn_named`, and MQTT helpers such as `cotonic.mqtt.matches`, `fill`, and `extract`.
- Cotonic topics are slash-separated MQTT-style topics. Use `+` and `#` wildcards in subscriptions; named wildcards such as `+id` can be extracted by Cotonic helpers.
- Zotonic bridges the browser broker to the server over the origin MQTT bridge. Server topics are commonly addressed with `bridge/origin/...`, for example `bridge/origin/model/rsc/event/{{ id }}/delete`.
- Include Cotonic through Zotonic's standard head/JS includes. Do not hand-roll the low-level bridge setup unless working on `mod_base` itself.
- Ensure the base template includes `{% all include "_html_head.tpl" %}` or `_html_head_admin.tpl`; these include Cotonic head setup such as `_html_head_cotonic.tpl`.
- Ensure the JS includes load `cotonic/cotonic.js` and the Zotonic wired app when the site uses declarative data attributes or wires.
- Add `data-cotonic-pathname-search="{% cotonic_pathname_search %}"` to `<body>`; Zotonic's wired JS warns when it is missing and Cotonic UI/location handling depends on it.
- Use `cotonic.ready.then(function() { ... })` before calling Cotonic APIs from inline page JavaScript that must run after initialization.

## Cotonic Data Attributes

- Prefer Cotonic data attributes for simple declarative behavior instead of ad-hoc JavaScript event handlers.
- Use `data-onclick-topic="topic"`, `data-onsubmit-topic="topic"`, and `data-oninput-topic="topic"` to publish DOM events to Cotonic topics. Example: `<a href="#back" data-onclick-topic="model/location/post/redirect/back">`.
- For live search/filter forms, use `data-onsubmit-topic="model/location/post/qlist/submit"` and `data-oninput-topic="model/location/post/qlist/submit"` on a `GET` form.
- Event data attributes normally prevent default behavior and stop propagation. Use `data-onclick-cancel`, `data-onsubmit-cancel`, or `data-oninput-cancel` when local behavior needs different cancellation semantics.
- Use Cotonic location model topics for URL behavior: `model/location/post/redirect/back`, `model/location/post/qlist/submit`, `model/location/post/push`, `model/location/post/replace`, and their silent variants when appropriate.
- Use storage topics instead of direct storage access when cooperating with Cotonic components: `model/localStorage/get/+key`, `model/localStorage/post/+key`, `model/sessionStorage/get/+key`, and matching `event/+key` topics.
- Use UI topics such as `model/ui/insert/+key`, `model/ui/update/+key`, `model/ui/delete/+key`, and `model/ui/event/dom-updated/+key` for DOM updates managed by Cotonic UI code.
- Escape any template values placed inside data attributes, especially topic fragments or payload-like values that include `q.*` or non-resource model output.
- Keep data attributes readable and topic-oriented; if a behavior needs complex state, use a scomp, a Cotonic model, or a dedicated JavaScript module.

## JavaScript And Interactivity

- Put inline template-collected JavaScript inside `{% javascript %}...{% endjavascript %}` and ensure the base has `{% script %}`.
- Use Zotonic declarative behavior (`{% wire %}`, scomps, Cotonic widgets, `do_*` classes) when nearby templates use it.
- Include Cotonic assets through the site `_js_include.tpl` and module head/body hooks, not by duplicating low-level bridge snippets.
