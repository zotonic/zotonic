# Language Register Audit — 2026-10-30

## Scope and Evidence

Read-only sample of current translations from Crowdin project 3405, file 8692. Checked nine shared strings per locale in 18 other target locales (162 returned translations), including admin instructions and account/confirmation messages. This is not a full-catalog audit or a statement of each language's industry standard. Interpretations below concern the sampled text. No translations were changed.

Source IDs: 2797364 (Save), 2797526 (Are you sure you want to delete the page), 2797558 (Choose a media item from this page to insert in the body text.), 2797668 (Enter a URL or embed code…), 2798104 (You are not allowed to edit this page.), 2802167 (Your search query is invalid…), 2802791 (Your username is:), 2803512 (You are logged in), 2803940 (Are you sure?).

## Observations, Not Approved Policies

| Locale | Pattern in sample | Evidence / audit concern |
|---|---|---|
| French `fr` | Mostly formal vous; informal outlier | “Vous n’êtes pas autorisé…” versus “Es-tu sûr?” |
| Spanish `es-ES` | Mixed usted and tú | “No tiene permitido…” versus “Tu nombre de usuario es:”. One message mixes “Tu consulta…” with “Corríjalo”. |
| Italian `it` | Informal singular | “Sei sicuro…”, “Non ti è permesso…”, “Il tuo nome utente…”. Button: “Salva”. |
| Polish `pl` | Direct informal singular | “Czy na pewno chcesz…”, “Nie możesz…”, “Twoja nazwa użytkownika…”. Button: “Zapisz”. |
| Swedish `sv-SE` | Informal du | “Är du säker…”, “Din sökfråga…”. Button: “Spara”. |
| Russian `ru` | Polite/plural вы forms | “Вы уверены…”, “Введите…”, “Ваше имя пользователя…”. Button: “Сохранить”. |
| Ukrainian `uk` | Polite/plural ви forms | “Ви впевнені…”, “Виберіть…”, “Ваше ім'я користувача…”. Button: “Зберегти”. |
| Turkish `tr` | Mostly polite/plural forms; informal outlier | “emin misiniz” and “yetkiniz” versus “Emin misin?”. Do not infer prose register from the short button “Kaydet”. |
| Indonesian `id` | Mostly Anda; informal kamu outliers | “Anda tidak diizinkan…” versus “Kamu sudah masuk” and “Apa kamu yakin?”. Also inconsistent capitalization of Anda. |
| Estonian `et` | Mixed informal singular and polite/plural forms | “Oled kindel…” and “Vali…” versus “Teil pole lubatud…”, “Teie…” and “Sisestage…”. |
| Simplified Chinese `zh-CN` | Mixed polite 您 and ordinary 你 | “您不能编辑本页面。” versus “你确定吗？” and “你可以…”. Some messages omit direct address. |
| Traditional Chinese `zh-TW` | Mixed 您 and 你; script inconsistency | “您的用戶名是：” versus “你確定嗎？”. Several sampled strings contain Simplified Chinese, e.g. “您不能编辑本页面。”. Audit locale/script as well as register. |
| Portuguese, Portugal `pt-PT` | Third-person/você wording; locale concerns | “Você não está autorizado…” and “Seu nome de usuário…”. Sample includes “Salvar”, “mídia”, “usuário”, and “logado”; review Brazilian influence before setting a Portugal-specific standard. Do not equate você with German Sie. |
| Portuguese, Brazil `pt-BR` | Você / third-person wording | “Você não tem permissão…”, “Seu nome de usuário…”. This is not evidence of a German-style formal address policy. |
| Arabic `ar` | Direct singular address; politeness expressed in phrasing | “هل أنت متأكد؟”, “يرجى…”. Do not classify mechanically as du/Sie; review gender and polite phrasing separately. |
| Hebrew `he` | Direct address, largely masculine singular; some number mixing | “אתה מחובר” versus plural wording in the URL instruction. Review gender/number separately from register. “Save” is “להציל” (rescue), a separate meaning issue to audit. |
| Thai `th` | คุณ and polite instruction wording, with conversational confirmations | “คุณไม่ได้รับอนุญาต…”, “โปรดแก้ไข…”, “คุณแน่ใจมั้ย?”. Register needs a language-specific review, not a binary du/Sie conversion. |
| English `en` | You; no du/Sie distinction | Determine tone from phrasing, not the pronoun. |

## Approved Decisions

Dutch uses informal address with imperative actions; German retains formal address or natural neutral wording with conventional German button labels. Following the Apple comparison, the user also approved Apple-aligned address for the mixed catalogs: French vous, Spanish tú, Turkish siz forms, Indonesian Anda, Estonian teie forms, and Chinese 你 in both scripts. The user subsequently extended the preferences to the remaining checked languages: informal Italian, Polish, and Swedish; polite Russian and Ukrainian; locale-specific Portuguese address; and natural language-specific address for Arabic, Hebrew, Thai, and English. See the parent skill for authoritative rules and Apple references. The table above preserves the original observations; it does not override these approved choices.

Apply the approved register when auditing the listed mixed languages; inspect both admin and end-user contexts before changing shared strings. A shared string can affect both surfaces. Keep meaning, regional vocabulary, script, gender, and grammatical number issues distinct from formality.
