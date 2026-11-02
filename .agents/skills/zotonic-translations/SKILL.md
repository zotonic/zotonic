---
name: zotonic-translations
description: Write, review, or audit Zotonic UI translations and action labels, including Dutch action labels, German formal address, language-specific register, and contextual Crowdin translation reviews. Apply Zotonic terminology and macOS-style imperative button labels.
---

# Zotonic Translations

## Project-Specific Preferences

These conventions are the defaults for Zotonic core. Individual non-core projects, such as sites and separately maintained modules, can define their own translation preferences, including terminology, register, and action-label style.

- Record a non-core project's preferences in a project-local skill file, such as `.agents/skills/project-translations/SKILL.md`, so they remain discoverable with that project.
- Read applicable project-local translation skills before translating or auditing that project. Explicit local preferences take precedence over the defaults in this skill within that project's scope; use these defaults where the local skill is silent.
- Keep project-specific exceptions in the local skill rather than changing Zotonic core conventions. Record newly agreed preferences there when asked to establish or update that project's translation policy.

## Dutch UI Style

Zotonic prefers concise, macOS-style **imperatives** for Dutch action buttons and menu commands. This is an explicit project preference. Do not standardize actions on infinitives such as “Opslaan”, “Annuleren”, or “Bewerken”.

Use the following preferred action labels when the meaning fits:

| English | Dutch |
|---|---|
| Save | Bewaar |
| Cancel | Annuleer |
| Close | Sluit |
| Delete | Verwijder |
| Edit | Bewerk |
| Create | Maak |
| Add | Voeg toe |
| Select | Selecteer |
| Confirm | Bevestig |
| Publish | Publiceer |
| Copy | Kopieer |
| Clear | Wis |
| Connect | Verbind |
| Disconnect | Ontkoppel |
| Duplicate | Dupliceer |
| View | Bekijk |
| Update | Werk bij |
| Search | Zoek |
| Upload | Upload |
| Download | Download |

- Save → “Bewaar” follows Dutch macOS wording. “Sla op” is also an imperative, but “Bewaar” is the preferred label here.
- Conjugate naturally when adding an object: “Bewaar wijzigingen”, “Voeg een pagina toe”, “Werk de pagina bij”. Do not concatenate a short label mechanically.
- Use sentence case, not English title case. Favor short, clear wording without sacrificing meaning.
- Apply imperatives to actions, not every UI string. Navigation labels remain “Terug” and “Volgende”; headings, properties, statuses, and explanatory prose use the grammatical form their context requires. For example, a completed action is “Taken verwijderd.”
- Address the user with “je” and “jij” in Dutch instructions. Use natural imperatives such as “Selecteer een categorie”.
- Treat this as a Dutch localization convention, not a rule to impose mechanically on other languages.

## German UI Style

- Use formal **Sie / Ihnen / Ihr** when addressing the user. This is the agreed Zotonic convention; correct informal “du” outliers when they are within the requested audit scope.
- Prefer natural, concise wording without direct address where practical. For example: “Soll dieser Eintrag wirklich gelöscht werden?” Avoid awkward circumlocutions solely to remove a pronoun.
- Use conventional, address-neutral German action labels such as **Speichern**, **Abbrechen**, **Löschen**, and **Bearbeiten**. Do not mechanically apply the Dutch imperative rule to German buttons or expand them into “Speichern Sie”.
- Capitalize formal address correctly. Distinguish formal “Sie” from third-person “sie” by context.

## Other Languages: Register

- Assess register per language and locale, separately from button grammar. A short action label alone does not establish whether prose uses formal or informal address.
- Preserve a clearly established register within the requested scope. For the languages listed below, use the approved Apple-aligned preferences. The explicit German Sie policy remains in force even though current Apple German documentation uses du.
- Do not equate grammatical third-person forms, plural forms, or the absence of pronouns with a universal level of formality. Some languages express politeness through vocabulary or phrasing rather than a du/Sie distinction.
- For the initial Crowdin findings and concrete examples, read [Language register audit](references/language-register-audit.md). Its observations are dated samples; the approved register choices below govern subsequent work. Recheck current translations when acting on findings.

### Approved Register by Language

The user chose Apple's localized Mac conventions for the languages below, alongside the explicit Dutch and German policies above. Apply these choices when translating or correcting strings within the requested scope; a style decision alone does not request a bulk Crowdin rewrite. Keep button grammar idiomatic for each language.

| Language / Crowdin locale | Preferred address | Apple usage reference |
|---|---|---|
| French `fr` | Formal **vous / votre**; remove informal tu outliers. | [Mac guide](https://support.apple.com/fr-fr/guide/mac-help/welcome/mac) |
| Spanish `es-ES` | Informal **tú / tu** and matching singular verbs; do not mix with usted conjugations. | [Mac guide](https://support.apple.com/es-es/guide/mac-help/welcome/mac) |
| Turkish `tr` | Polite/plural **siz** forms and matching suffixes; e.g. **Emin misiniz?**, not **Emin misin?** | [Mac support](https://support.apple.com/tr-tr/mac) |
| Indonesian `id` | **Anda**, capitalized, rather than kamu. | [Mac guide](https://support.apple.com/id-id/guide/mac-help/welcome/mac) |
| Estonian `et` | Polite/plural **teie** and matching verbs; avoid switching to sina/singular forms. | [Mac guide help](https://support.apple.com/et-ee/guide/mac-mini/apdfdad69354/mac) |
| Simplified Chinese `zh-CN` | Ordinary **你 / 你的**, rather than honorific 您; omit pronouns where natural. Use Simplified characters. | [Mac guide](https://support.apple.com/zh-cn/guide/mac-help/welcome/mac) |
| Traditional Chinese `zh-TW` | Ordinary **你 / 你的**, rather than honorific 您; omit pronouns where natural. Use Traditional characters and Taiwan terminology. | [Mac guide](https://support.apple.com/zh-tw/guide/mac-help/welcome/mac) |

| Italian `it` | Informal **tu / tuo** with singular verbs; omit the pronoun where natural. | [Mac guide](https://support.apple.com/it-it/guide/mac-help/welcome/27/mac/27) |
| Polish `pl` | Direct informal singular, e.g. **możesz / wybierz**, with **Twój / Twoja** where needed; avoid Pan/Pani address. | [Mac guide](https://support.apple.com/pl-pl/guide/mac-help/welcome/mac) |
| Swedish `sv-SE` | Informal **du / din / ditt**, not formal ni address. | [Mac guide](https://support.apple.com/sv-se/guide/mac-help/welcome/mac) |
| Russian `ru` | Formal **Вы / Ваш** and matching polite/plural verbs. Capitalize direct formal address; distinguish third-person references. | [Mac guide](https://support.apple.com/ru-ru/guide/mac-help/welcome/mac) |
| Ukrainian `uk` | Polite/plural **ви / ваш** with matching verbs; use lowercase in general UI prose except at sentence starts, following the cited Apple usage. | [Mac guide](https://support.apple.com/uk-ua/guide/mac-help/welcome/mac) |
| Portuguese, Portugal `pt-PT` | Polite third-person forms such as **pode / seu / clique**, usually without an explicit pronoun; avoid tu forms and unnecessary você. Use European Portuguese vocabulary. | [Mac guide](https://support.apple.com/pt-pt/guide/mac-help/welcome/mac) |
| Portuguese, Brazil `pt-BR` | Ordinary **você / seu** with matching third-person verbs; omit the pronoun where natural. Use Brazilian Portuguese vocabulary. Do not treat você as equivalent to formal German Sie. | [Mac guide](https://support.apple.com/pt-br/guide/mac-help/welcome/mac) |
| Arabic `ar` | Clear standard Arabic with natural direct singular or impersonal phrasing. Avoid invented honorific or plural address solely to sound formal; this is not a du/Sie distinction. | [Mac guide](https://support.apple.com/ar-sa/guide/mac-help/welcome/mac) |
| Hebrew `he` | Prefer natural impersonal phrasing where useful, or consistent direct singular address. Follow contextual gender-inclusive wording where needed; do not use plural as a substitute for formal address. Treat gender and number separately from formality. | [Mac guide](https://support.apple.com/he-il/guide/mac-help/welcome/mac) |
| Thai `th` | Polite neutral **คุณ** where a pronoun is needed, with clear polite instructions; omit pronouns where natural. Avoid colloquial register shifts or adding gendered polite particles mechanically. | [Mac guide](https://support.apple.com/th-th/guide/mac-help/welcome/mac) |
| English `en` / source `en-US` | Direct, neutral **you / your**, concise action labels and instructions. No formal/informal pronoun distinction. | No language-specific address distinction. |

These references establish observed Apple usage, not a claim of an exhaustive published macOS localization policy. The non-binary register guidance for Arabic, Hebrew, Thai, and Portuguese is a practical Zotonic preference informed by these examples. Regional vocabulary, script, gender, and number still require contextual review; do not infer them from formality alone. For languages not listed, inspect the target locale before establishing a new policy.

## Crowdin Glossary

- Consult the live [Zotonic Crowdin glossary](https://crowdin.com/profile/mworrell/resources/glossaries/1971) (glossary `1971`, project `3405`) through the Crowdin integration when translating or auditing terminology. Use its preferred target-language terms where their definitions match the UI context; retain natural grammar and the project's language-specific style.
- Read glossary definitions and notes, and associate language variants by concept ID. Source-only entries do not establish a preferred target-language translation. For example, the glossary currently maps “passcode” to Dutch “eenmalige code”, distinguishes dependency graphs from charts, and identifies `Content-Security-Policy` as a literal HTTP header. Recheck live entries rather than treating these examples as a fixed glossary snapshot.
- If glossary access is unavailable or an entry conflicts with the intended meaning or explicit project preferences, report the limitation or conflict. Apply terms within the requested scope; glossary consultation does not authorize glossary edits or unrelated translation changes.

## Contextual Translation Review

- Read the actual template or Erlang usage before proposing a translation. Determine whether the string is a button, field label, heading, instruction, or notification. For example, “First” on a `name_first` field means “Voornaam”.
- Check shared usages before changing a shared source string. Flag incompatible meanings that need distinct English source strings rather than forcing one translation into both contexts.
- Zotonic core translations are managed in Crowdin. When reviewing current translations or submitting changes, use the live project as the source of truth; local PO files provide a useful snapshot and source references.
- Present the existing translation, proposed wording, and contextual reason when the user requests an audit or examples. Distinguish meaning or grammar corrections from stylistic improvements.
- Preserve placeholders, plural forms, markup, and links. Keep source strings in English and use Zotonic translation tags or Erlang translation helpers.
- A request for examples or a skill update does not authorize a bulk translation rewrite or Crowdin submission. Apply changes within the user's requested scope; do not infer an automatic approval policy.

## Placeholders and Technical Literals

- Never translate placeholder contents. Preserve the complete token exactly, including case, delimiters and internal whitespace: `{client}`, `{username}`, `{ url }`, and `{{ predicate }}` must remain unchanged. Translate only the surrounding prose.
- Preserve each token's occurrence count. Named placeholders may move with the target language's grammar; do not rename, omit, duplicate, split, or substitute them with an example value. Keep placeholders intact inside HTML attributes and links as well as visible text.
- Preserve template syntax such as `{% cache %}` and formatting directives such as Erlang `~p`. Keep positional arguments in their original order unless the actual formatter explicitly supports indexed reordering. Check each plural form separately when present.
- Preserve executable technical literals, including configuration keys, module/function names, OAuth scope and enum values, MIME types, and relative-date expressions. For example, keep `User.Read`, `organizations`, `image/png`, `manage_schema/2`, and `-5 year` exactly as written. A code span is not sufficient evidence by itself: distinguish required syntax from an illustrative URL path that may legitimately be localized.
- Before submitting or approving a translation, compare its placeholders against the source, including multiplicities and delimiter structure, and inspect the surrounding markup. Compare positional directives in order. Do not approve a translation with an unresolved mismatch.
- For an audit across languages, use current Crowdin source strings and target translations, paginate completely, and record coverage by file and locale. Detect both missing source tokens and unexpected target tokens, including in strings without source placeholders. Report untranslated or missing entries separately. State whether the check covers current selected translations, approved translations, or all historical suggestions; do not imply that a selected-translation scan covers every suggestion.

## Reference

Apple's [button guidance](https://developer.apple.com/design/human-interface-guidelines/buttons) recommends action-oriented labels. Dutch macOS uses “Bewaar” and “Annuleer”, as shown in [Apple's Dutch Preview documentation](https://support.apple.com/nl-nl/guide/preview/prvw1509/11.0/mac/27). The Dutch imperative convention above records Zotonic's chosen style; it is not a claim that the general HIG mandates Dutch grammar.
