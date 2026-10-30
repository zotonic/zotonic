---
name: zotonic-translations
description: Write, review, or audit Zotonic UI translations and action labels, including Dutch action labels, German formal address, language-specific register, and contextual Crowdin translation reviews. Apply Zotonic terminology and macOS-style imperative button labels.
---

# Zotonic Translations

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

## Contextual Translation Review

- Read the actual template or Erlang usage before proposing a translation. Determine whether the string is a button, field label, heading, instruction, or notification. For example, “First” on a `name_first` field means “Voornaam”.
- Check shared usages before changing a shared source string. Flag incompatible meanings that need distinct English source strings rather than forcing one translation into both contexts.
- Zotonic core translations are managed in Crowdin. When reviewing current translations or submitting changes, use the live project as the source of truth; local PO files provide a useful snapshot and source references.
- Present the existing translation, proposed wording, and contextual reason when the user requests an audit or examples. Distinguish meaning or grammar corrections from stylistic improvements.
- Preserve placeholders, plural forms, markup, and links. Keep source strings in English and use Zotonic translation tags or Erlang translation helpers.
- A request for examples or a skill update does not authorize a bulk translation rewrite or Crowdin submission. Apply changes within the user's requested scope; do not infer an automatic approval policy.

## Reference

Apple's [button guidance](https://developer.apple.com/design/human-interface-guidelines/buttons) recommends action-oriented labels. Dutch macOS uses “Bewaar” and “Annuleer”, as shown in [Apple's Dutch Preview documentation](https://support.apple.com/nl-nl/guide/preview/prvw1509/11.0/mac/27). The Dutch imperative convention above records Zotonic's chosen style; it is not a claim that the general HIG mandates Dutch grammar.
