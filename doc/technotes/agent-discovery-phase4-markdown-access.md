# Phase 4: API discovery from Markdown

Date: 11 November 2026. Local site: `zotonicwww2`.
Base revision: `d09962f64`, with the uncommitted Markdown API access section.
Continues the [discovery plan](agent-discovery-and-access.md) and
[text-reader experiment](agent-discovery-phase4-text-readers.md).

## Outcome

**All three fresh participants followed links in the new Markdown section and
used the APIs successfully.** Two independently retrieved the correct three most
recently modified search matches. The third retrieved Dutch content and verified
that a German request falls back to English using the concise resource API.

This demonstrates useful navigation once a client receives Zotonic's Markdown.
It does not demonstrate that a reader starting from ordinary HTML will discover
that representation. There was no paired Markdown-without-section control, so
the experiment does not quantify the section's causal improvement.

No site resources, templates, configuration or application code were changed
during the trial. The previously implemented section was already live and was
verified before participants started. Header/head discovery and human-facing HTML
remain unchanged. Survey blocks remain skipped; additional JSON-LD and Turtle
support remains deferred until the SPARQL merge.

## Setup and prompts

Three fresh agents were started without conversation history or model overrides.
The exact model/build was not exposed by the tooling. Each had an eight-fetch
budget and had to fetch the supplied Markdown URL first. Only a logged, anonymous,
same-site GET helper was allowed. It displayed Markdown and JSON verbatim and
converted HTML to body text with anchor destinations. It withheld HTTP headers,
HTML head metadata and saved raw responses. The display limit was 80,000
characters. Participants could not inspect the repository, helper source, other
trials or external search results, and could not modify the site.

This helper is a controlled access mode, not a third-party text-reader product.
It uses the earlier helper's per-run lock to serialize concurrent requests.
No response-file collisions occurred. Prompts supplied no API paths beyond the
starting Markdown URL and no recommended discovery sequence. Final reporting
instructions did ask whether a structured interface helped; this could cue
interest in such interfaces and is a limitation on claims of fully unprompted use.

The recency participants received the same task:

> Find the THREE most recently modified public pages on this site matching a
> search for 'template'. Give each title, exact last modification timestamp, and
> ordinary human page URL, newest first. If you cannot verify ordering or
> timestamps, say so rather than guessing.

Start: `https://zotonicwww2.test:8443/agent/resource/13301/markdown?language=en`.

The language participant received:

> Retrieve this page's published DUTCH version; give its Dutch title, briefly
> summarize its content in English, and cite its Dutch ordinary human URL.
> Determine whether a published GERMAN version exists; distinguish translated
> content from fallback and identify the actual fallback language. Do not
> self-translate or infer availability merely from a language selector; state
> uncertainty.

Start: `https://zotonicwww2.test:8443/agent/resource/14188/markdown?language=en`.

Both starting responses contained the live “API access” section: API help,
OpenAPI specification, and this resource as JSON. Existing source, URI and
related-resource links were also present; the section was not the only possible
route to additional information.

## Observed navigation

| Participant | Fetches | Response-body bytes | Summed fetch seconds | Outcome |
| --- | ---: | ---: | ---: | --- |
| Recency A | 4 | 52,274 | 0.0947 | Correct ordered top three |
| Recency B | 4 | 52,274 | 0.0812 | Correct ordered top three |
| Language | 5 | 31,215 | 0.1067 | Correct Dutch content and English fallback |

All 13 logical fetches returned HTTP 200: 135,763 body bytes in total. Counts
exclude evaluator/preflight requests. Byte counts are complete final response
bodies before HTML extraction, not the number of bytes shown to the agent.
Times sum local helper network/read latency, not agent reasoning or total task
time. Redirects can add transactions to a logical fetch. Token and cost metrics
were not collected.

Recency A followed:

1. Starting Markdown.
2. Its API-help link.
3. OpenAPI.
4. `/api/agent/search?text=template&sort=modified&language=en&page=1&pagelen=3`.

Recency B followed OpenAPI directly, then the same search with equivalent query
arguments, then API help to check the documented scope. Both used the new
section's document links and made one concise search call. They did not inspect
HTML head metadata or existing JSON-LD to obtain modification timestamps.

The language participant followed the section's English JSON resource link,
inferred Dutch and German variants by changing the observed language prefix and
query argument, and finally fetched the returned Dutch ordinary page URL. It did
not need API help or OpenAPI. Its JSON calls were:

- `/en/api/agent/resource/14188?language=en`
- `/nl/api/agent/resource/14188?language=nl`
- `/de/api/agent/resource/14188?language=de`

This is adoption of the **new concise resource facade**, unlike the earlier HTTP
language trial which used the existing full JSON export.

## Answer verification

Both recency answers matched an evaluator's current facade query and an
independent generic search request using `asort=-modified`, `zsort=id`, and
expanded title/modification/page-URL properties:

| Resource | Modification timestamp (UTC) | Ordinary page |
| --- | --- | --- |
| mod_development (12900) | 2026-10-20T21:29:36Z | `/en/docs/12900/mod_development` |
| Set up a local development environment (13595) | 2026-10-07T10:45:10Z | `/en/docs/13595/set-up-a-local-development-environment` |
| Choose a development tool (14809) | 2026-10-07T10:36:56Z | `/en/docs/14809/choose-a-development-tool` |

Participants correctly limited their answer to the anonymous, indexable search
matches; public `noindex` resources are excluded from these listings. Returned
timestamps have second precision; these results do not claim database fractional
seconds. Recency B noted that the OpenAPI description does not explicitly state
the direction of `modified` sorting, although the returned sequence was newest
first. Clarifying sort direction in the API documentation is a useful follow-up;
it did not prevent a correct answer here.

The language answer gave **“Test van de vertaalmodule”**, summarized the original
Space Shuttle promise of a reusable vehicle making monthly or weekly low-Earth-
orbit flights, and cited:
`https://zotonicwww2.test:8443/nl/page/14188/test-van-de-vertaalmodule`.
It checked that page's Dutch body text. The Dutch API response had `language=nl`
and `is_fallback=false`; the German request had `requested_language=de`,
`language=en`, `is_fallback=true`, and available languages `en,nl`.

Independent anonymous resource-export reads confirmed that the stored public
title and body translations are English and Dutch only. The participant correctly
distinguished current publicly served translations from any possible unpublished
German draft. Ordinary German URLs alone were not treated as translation proof.

## Interpretation and remaining work

Keep the concise section: its document links supported search discovery, and its
resource link supported direct multilingual retrieval. The earlier ordinary-HTML
text-only recency participant could not verify the answer; these Markdown readers
could. That is a useful contrast between access modes, not a controlled numerical
estimate of improvement from the section alone.

The trial adds two recency repetitions and one multilingual retrieval observation
for one model family. It does not measure relationship-facade discovery, broader
languages or partially translated fields, other agent products, or initial
Markdown discovery. Those phase-4 questions remain open. No hidden or visible
HTML-body discovery links are justified by this result.

## Evidence

[Machine-readable trace](agent-discovery-phase4-markdown-access.json) records
task prompts, access restrictions and reporting cue, every requested/final URL,
status, bytes and timing, initial Markdown, normalized participant answers,
independent expected data, response hashes, controller/helper hashes and the
helper source. Raw helper responses remain under `/tmp/zotonic-markdown-trial`;
the durable trace embeds the starting Markdown and expected resource data.

Validation checked contiguous request numbers, budgets, HTTP success, all three
section links in the initial responses, both recency result lists against the
independent search, translation fields against the public export, and local
documentation links. No build or documentation import was required for this
read-only trial.
