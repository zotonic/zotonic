---
keywords:
  - reference
  - backend_developer
  - localization_and_translation
  - translated_text
---

# Erlang translation API: z_trans

`z_trans` is part of `zotonic_core`. It resolves gettext interface messages and
language alternatives. For concepts and catalog authoring, see the
[translation guide](guide#translations). For template calls, see
[the extended trans tag](tag#trans_ext).

Supply literal English source text and literal message contexts for POT
extraction. The functions below accept binaries or strings for these arguments;
selected translations are binaries. They do not substitute placeholders.

## Singular messages

```erlang
z_trans:trans(<<"Open">>, Context).
z_trans:trans(<<"Open">>, nl, Context).
z_trans:translations(<<"Open">>, Context).
```

`trans/2` selects text using the request context's languages. `trans/3` accepts
an explicit language. `translations/2` returns a `#trans{}` record containing
language alternatives, including an English fallback.

The macros in `zotonic.hrl` provide the same operations:

```erlang
?__(<<"Open">>, Context).   % Selected text: trans/2.
?___(<<"Open">>, Context).  % Language alternatives: translations/2.
```

## Message context

```erlang
z_trans:trans_msgctxt(<<"Open">>, <<"action">>, Context).
z_trans:trans_msgctxt(<<"Open">>, <<"action">>, nl, Context).
z_trans:translations(<<"Open">>, <<"action">>, Context).
?__(<<"Open">>, <<"action">>, Context).
?___(<<"Open">>, <<"action">>, Context).
```

These APIs are strict: they try the requested message context in the selected
languages, then fall back to the English source. They do not borrow contextless
translations. The module's `-mod_msgctxt` attribute applies to templates only.

Use `lookup/2,3` or `lookup_fallback/2,3` to select a language from a resolved
singular `#trans{}` record. Passing that record to `trans/3` can re-fetch
translations using only its English text, losing the message context.

## Plural messages

```erlang
z_trans:trans_plural(<<"{n} hour">>, <<"{n} hours">>, N, Context).
z_trans:trans_plural(
    <<"{n} hour">>, <<"{n} hours">>, N, <<"relative time: past">>, Context).
```

`N` must be a non-negative integer. These functions return the selected text;
callers substitute `{n}` and other placeholders separately. The contextual form
is strict, just like `trans_msgctxt`. Its source pair is extracted into `msgid`
and `msgid_plural`, with `msgctxt` for the five-argument form.

For all language alternatives, use:

```erlang
z_trans:plural_translations(Singular, Plural, Context).
z_trans:plural_translations(Singular, Plural, MsgCtxt, Context).
```

The returned `#trans{}` holds plural values, not selected text. Each language's
value pairs a parsed plural rule with a tuple of forms. Keep the rule and forms
together: indices from one catalog must not select forms from another catalog.

## Contextless fallback

Opt into contextual-then-contextless lookup with:

```erlang
z_trans:translations(Text, MsgCtxt, true, Context).
z_trans:plural_translations(Singular, Plural, MsgCtxt, true, Context).
```

For plural messages, a language can contain ordered candidates: the contextual
entry first, followed by the contextless entry. For example:

```erlang
Trans = z_trans:plural_translations(
    <<"{n} item">>, <<"{n} items">>, <<"basket">>, true, Context).
```

The Dutch entry in `Trans#trans.tr` might be:

```erlang
{nl, {plural_candidates, [
    %% Contextual entry, with its plural form still untranslated.
    {{2, {'!=', n, 1}}, {<<"{n} artikel">>, <<>>}},
    %% Contextless entry, with its own plural rule.
    {{2, {'!=', n, 1}}, {<<"{n} item">>, <<"{n} items">>}}
]}}
```

For `N = 1`, this selects `<<"{n} artikel">>`. For `N = 2`, the contextual
form is empty, so it selects `<<"{n} items">>` from the contextless entry.
Candidates can have different rules; each evaluates the count independently.

Resolve the count before selecting the language, so missing forms still allow
fallback to another language:

```erlang
Source = {template_compiler_plural:english(),
          {<<"{n} item">>, <<"{n} items">>}},
Resolved = template_compiler_plural:resolve(Trans, N, Source),
Text = z_trans:lookup_fallback(Resolved, Context).
```

`Resolved` contains selected text per available language. If all candidates for
a language lack a usable form, that language is omitted. English source fallback
uses the English plural rule. Placeholder substitution remains the caller's task.
