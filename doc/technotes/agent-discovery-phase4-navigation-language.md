# Phase 4: relationship navigation and multilingual retrieval

Date: 11 November 2026, Europe/Amsterdam. Local site: `zotonicwww2`.
Repository revision at the start: `d09962f64`.
This continues the [recency repeat](agent-discovery-phase4-recency-repeat.md) and
[agent discovery plan](agent-discovery-and-access.md).

## Findings

**Both access modes successfully navigated explicit cross-references and retrieved
the Dutch content.** They also correctly recognized English fallback at the
German URL. The HTTP language agent independently used the advertised existing
JSON resource export to verify the resource's published translation set. No
participant used the new concise relationship or language endpoints.

The relationship task was already well served by ordinary article references and
“Referred by” navigation. Those are human documentation links, not agent discovery
links. API use was unnecessary for this task, so non-use is not a failure.

No application code, site content, ACL rules, templates or discovery configuration
was changed. Discovery remains in headers, HTML head metadata and dedicated
documents; no visible or hidden body/footer discovery links were added.

## Trial design

Four primary trials crossed two tasks with two access modes. Each participant
was a fresh isolated instance of the current Codex agent, with no conversation
history or model override. The exact model/build was not exposed by the tooling.
One earlier HTTP relationship run was repeated after an instrumentation collision;
it is retained as ancillary evidence rather than included in primary comparisons.

As before, prompts supplied only the task, an ordinary page URL and access rules:
no API paths, protocol names or preferred navigation strategy. Each agent had an
eight-fetch budget, used only the logged read-only helper, and could not inspect
repository files, other participants or investigator snapshots. HTTP participants
could see headers and response bodies and parse their own saved responses.
Extracted-text participants saw body text, anchor destinations and image alt text,
but not head metadata, headers, scripts, styles or raw HTML files. This is a text
extraction simulation, not a fully rendered browser. Non-HTML documents remained
readable if discovered. The display cap remained 80,000 characters.

Expected answers were established before participant runs using anonymous public
resource/relationship reads, then checked independently with ordinary site-context
`m_edge` and `m_rsc` calls. Evaluator fetches are excluded from participant metrics.
The existing fixtures were used without creating or modifying test resources.

## Relationship task

Starting page:
`https://zotonicwww2.test:8443/en/docs/13301/catinclude`

> Which other documentation pages does this page directly refer to, and which
> documentation pages refer back to it? Give TWO outgoing targets and THREE
> incoming sources if available, distinguish direction, and cite ordinary human
> page URLs. Use explicit cross-references, not merely shared topics. Explain how
> you verified the directions; do not guess if evidence is insufficient.

Ground truth for the public `refers` predicate (13927):

- Outgoing from `catinclude` (13301): `all_catinclude` (13297) and `include` (13307).
- Fifteen incoming public sources were available. Any three verified sources
  were acceptable; no exhaustive inventory was requested.
- The graph also has subject/topic and other relationship edges. Merely appearing
  under “Related documentation” was not sufficient evidence of a direct reference.

Both primary participants returned the two correct outgoing targets and these
three correct incoming sources:

| Incoming source | Ordinary page | Verified reference to catinclude |
| --- | --- | --- |
| update | `/en/docs/13253/update` | Argument description links to the tag |
| live | `/en/docs/12822/live` | Category-based inclusion instructions link to the tag |
| all_catinclude | `/en/docs/13297/all_catinclude` | See-also/explanatory text links back to the tag |

The HTTP participant read the starting article, followed “Referred by” candidates,
checked source-page body links and resolved named `/id/…` references to ordinary
URLs. The text participant followed the same kinds of body references, including
resolving the common target back to `catinclude`. Both explicitly excluded
shared-topic recommendations and limited their claims to the requested examples.
Neither fetched API help, OpenAPI, `llms.txt`, the catalog or a data API.

This verifies direction-sensitive answers and citation quality on the selected
fixture. It does **not** demonstrate unaided adoption of `/api/agent/related`, nor
behavior on a large graph, an absent backlink UI or hidden endpoints.

## Multilingual task

Starting page:
`https://zotonicwww2.test:8443/en/page/14188/test-of-the-translation-module`

> Retrieve the site's published DUTCH version: give its Dutch title, briefly
> summarize its content in English, and cite the Dutch human page URL. Determine
> whether a published GERMAN translation exists; distinguish a translated page
> from fallback content and identify the actual fallback language if applicable.
> Do not translate the source yourself or infer availability merely from a
> language selector. State any uncertainty.

This existing translation-test resource is a useful controlled fixture, not a
representative sample of all production multilingual content. Its title/body
translations and language list contain English and Dutch, not German.
Independent public facade checks before the trials returned:

| Requested | Actual | Fallback? | Title |
| --- | --- | --- | --- |
| Dutch (`nl`) | Dutch | false | Test van de vertaalmodule |
| German (`de`) | English | true | Test of the translation module |

Both participants cited the correct Dutch human page:
`https://zotonicwww2.test:8443/nl/page/14188/test-van-de-vertaalmodule`.
Both accurately summarized the published Dutch paragraph: it recalls the early
promise of a reusable Space Shuttle within a larger transport system, making
frequent trips to low Earth orbit. Neither substituted a self-generated Dutch
translation for published content.

### HTTP participant

Four fetches: the starting English page, its Dutch alternate, the advertised
`/api/model/rsc_export/get/14188` JSON export, and the export's German URL.
The export showed a published resource with languages `en` and `nl`, and only
those title/body translations. The agent used that evidence to state that this
resource has no published German translation.

It also checked the German URL itself: the heading and article body were English;
the canonical link and article metadata identified English. The outer HTML and
some UI chrome were German. It correctly distinguished the UI/request language
from the article's language and did not mistake a German URL for translated text.

This is unaided use of an **existing compatibility export discovered on the
ordinary page**. It is not a trial of the new facade's language response fields.
Existing page JSON-LD was inspected as evidence; no new semantic representation
support was added.

### Extracted-text participant

Three fetches: the English page, the same resource under `/nl/`, then `/de/`.
The Dutch request redirected to its Dutch title slug. The German route returned
the same English title and body as the starting page.

The agent correctly identified observed English fallback and cited the Dutch
page. It appropriately limited its availability claim: body text proves what the
route serves, but cannot rule out unpublished content or a translation stored
elsewhere. It did not infer availability from a language selector alone and did
not claim access to metadata that its tools did not expose.

The retrieval and fallback-identification subtasks therefore passed in both
modes. Only the HTTP participant established the published translation set from
explicit resource metadata. That evidence distinction is retained rather than
scoring the qualified text answer as an identical availability determination.

## Primary metrics

| Task / mode | Logical fetches | Final-body bytes | Summed fetch time | API use | Outcome |
| --- | ---: | ---: | ---: | --- | --- |
| Relationships / HTTP, clean repeat | 7 | 295,792 | 0.2234 s | None | Correct 2 outgoing + 3 incoming; correct citations |
| Relationships / extracted text | 6 | 256,823 | 0.2440 s | None | Correct 2 outgoing + 3 incoming; correct citations |
| Languages / HTTP | 4 | 82,339 | 0.1258 s | Existing JSON export | Correct Dutch retrieval, availability and fallback |
| Languages / extracted text | 3 | 78,345 | 0.0750 s | None | Correct Dutch retrieval and observed fallback; qualified availability |

All final responses returned HTTP 200. Counts are **logical helper fetches**:
automatic redirects can add HTTP transactions. Requested and final URLs are both
recorded. Bytes count complete final response bodies before extraction/truncation,
not intermediate redirects or headers. Summed fetch times exclude model reasoning
and are not end-to-end latency measurements. Generation tokens and costs were not
measured. No credentials were supplied.

## Instrumentation incident and audit trail

The first HTTP relationship participant fetched two pages concurrently. The helper
allocated both saved responses number 2, so one body file was overwritten. Both
URL/status/byte log entries survived. The agent noticed the collision, repeated
one fetch, and produced the correct answer after five logical fetches (208,691
bytes). These counts include its recovery and are not used as the primary result.

A per-run file lock was added to the temporary helper to serialize budget checks,
request numbering and response saves. A fresh participant then repeated the same
relationship prompt successfully with seven collision-free fetches. The final
text-language participant also used the locked helper; the other two primary
runs had no concurrent-write collision. This instrumentation fix did not change
what content each access mode exposed or any Zotonic behavior.

The [machine-readable record](agent-discovery-phase4-navigation-language.json)
contains prompts, expected graph/language results, all five request traces,
metrics, response hashes and answer assessments. Ambiguous hashes for the two
colliding response numbers are explicitly null; the original trace is retained,
not silently replaced. The primary four runs contain 20 logical fetches; the
ancillary collision run adds five.

## Interpretation and next trials

- Good ordinary cross-reference navigation can satisfy relationship tasks without
  API discovery, including for text-only agents. Do not remove or replace that UI
  to make API adoption appear higher.
- Header/head advertisements can lead an agent to the existing JSON export for
  useful language evidence, without adding human-facing discovery links.
- A language-prefixed URL, page-shell language and article language can differ.
  The participants handled that distinction correctly on this fixture; broader
  testing should preserve it as an explicit scoring criterion.
- These runs do not establish adoption or efficiency of the new related-resource
  and multilingual facade endpoints. A future unaided task with larger or less
  visibly linked graphs can test that; separately labeled API-guided tests can
  assess endpoint usability without being counted as discovery successes.
- Broaden multilingual trials to independently authored multi-paragraph content,
  partially translated fields and other language pairs. Keep metadata availability
  separate from observed fallback when evaluating text-only answers.

These are single-task instances of one inherited model, reviewed by the same
assistant family. There is no randomized discovery ablation, multi-provider
comparison, or statistical adoption/cost claim. The logged helper collision also
limits comparison with the discarded first relationship run.

Discovery remains out of human-facing bodies and footers, including hidden body
links. Survey blocks remain skipped in Markdown. Additional JSON-LD and Turtle
support remains deferred until the SPARQL merge.
