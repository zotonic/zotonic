# Zotonic interface translation context

## Purpose and audience

This catalog combines strings from Zotonic's core and bundled modules. It covers a web CMS: content and media editing, administration, access control, configuration, surveys, mailing lists, and public-facing flows such as sign-in and registration. It is not a single screen or exclusively an administrator interface.

Readers include content editors, administrators, developers, and website visitors. Use clear, concise, helpful language. Explain errors and next steps directly. Preserve technical precision in configuration and diagnostic screens without introducing technical jargon into visitor-facing flows. Use individual string context and source references to determine the audience and function.

## Terminology and grammar

- Follow the project's Crowdin glossary when the definition matches the actual meaning. A shared English word can have different meanings across modules; consult string context rather than assuming one translation fits every use.
- Distinguish action buttons, field labels, headings, and completed-action statuses. Keep action labels concise and use sentence case where appropriate in the target language.
- Dutch actions use macOS-style imperatives: Save → Bewaar, Cancel → Annuleer, Close → Sluit, Delete → Verwijder, Edit → Bewerk, Add → Voeg toe. Conjugate naturally with objects. Do not apply this grammar mechanically to other languages or to non-action labels.
- German action labels use conventional neutral forms such as Speichern, Abbrechen, Löschen, and Bearbeiten.

## Language-specific address

These are Zotonic core conventions, not a universal formal or informal tone for all languages. Keep address consistent within each locale and omit pronouns where natural.

| Locale | Preferred address |
|---|---|
| Dutch `nl` | Informal je/jij. |
| German `de` | Formal Sie/Ihnen/Ihr, correctly capitalized. |
| French `fr` | Formal vous/votre. |
| Spanish `es-ES` | Informal tú/tu and singular verbs. |
| Italian `it` | Informal tu/tuo and singular verbs. |
| Polish `pl` | Informal singular, e.g. możesz/wybierz and Twój/Twoja. |
| Swedish `sv-SE` | Informal du/din/ditt. |
| Russian `ru` | Polite Вы/Ваш with matching plural verbs. |
| Ukrainian `uk` | Polite ви/ваш with matching plural verbs; lowercase except at sentence starts. |
| Turkish `tr` | Polite/plural siz forms and matching suffixes. |
| Estonian `et` | Polite/plural teie forms and matching verbs. |
| Indonesian `id` | Anda, capitalized. |
| Portuguese `pt-PT` | Polite third-person forms, usually without an explicit pronoun; avoid tu and unnecessary você. Use European Portuguese. |
| Portuguese `pt-BR` | Ordinary você/seu with third-person verbs; use Brazilian Portuguese. |
| Simplified Chinese `zh-CN` | Ordinary 你/你的; use Simplified characters. |
| Traditional Chinese `zh-TW` | Ordinary 你/你的; use Traditional characters and Taiwan terminology. |
| Arabic `ar` | Clear standard Arabic, natural singular or impersonal phrasing; no artificial honorifics. |
| Hebrew `he` | Natural impersonal or consistent singular phrasing, with context-sensitive inclusive wording. |
| Thai `th` | Polite neutral คุณ where needed; avoid mechanically adding gendered polite particles. |
| Japanese `ja` | Polite です/ます prose, with concise conventional action labels such as 保存 and 削除. Omit unnecessary pronouns. |
| Czech `cs` | Polite plural instructions, e.g. vyberte/otevřete; lowercase váš. Use conventional infinitive action labels. |
| Korean `ko` | Polite 합니다/하십시오 prose, with concise action labels such as 저장 and 삭제. Omit unnecessary pronouns. |
| Vietnamese `vi` | Neutral bạn where needed; omit pronouns in concise instructions and action labels, e.g. Lưu/Xóa. |
| Romanian `ro` | Polite plural instructions, e.g. faceți/puteți, and dvs. where needed. Keep action labels concise and idiomatic. |
| English `en` | Direct, neutral you/your. |

Separately maintained sites and modules may define their own preferences in project-local translation skills. Those exceptions apply to that project's strings, not automatically to this core catalog.

## Web interface constraints

- Preserve variable names and placeholders exactly, including placeholders inside HTML. Preserve markup, links, escape sequences, and plural-form requirements while translating the surrounding text.
- Preserve survey example tokens such as `[age]`, `[color]`, and `[icecream=vanilla|strawberry|chocolate|other]` in full, including names and option values inside the brackets.
- Keep literal identifiers, protocol names, code, and URLs intact when they are not natural-language labels.
- Labels appear in buttons, dialogs, tables, forms, and responsive layouts. Prefer concise natural wording; there is no universal character limit. Follow any limit supplied for an individual string.
- For right-to-left languages, write natural RTL text and preserve embedded technical tokens and placeholders in their original order. Do not manually reverse text or add layout markup to compensate for direction.
- File context supplements individual string context. Flag incompatible shared meanings or missing context rather than guessing.
