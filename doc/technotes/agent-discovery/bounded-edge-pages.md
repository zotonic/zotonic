# Bounded reads for high-degree relationships

Implemented and checked on 11 November 2026 on `zotonicwww2`.

## Problem and change

The agent relationship API already used SQL `LIMIT pagelen + 1` and optional
predicate filtering. However, its ordering by `predicate_id, seq, id` could force
PostgreSQL to scan and sort all incoming edges before returning a small page.
This matters for resources such as keywords with many incoming `subject` edges.
Loading `m_edge:subjects/3` and slicing in Erlang would also load/cache the entire
adjacency list, so it is not an appropriate substitute.

`m_edge:get_page(Direction, Id, Predicate, Offset, Limit, Context)` now owns the
bounded selection. It resolves resource/predicate references, checks the root and
selected predicate ACLs, and queries only `Limit + 1` candidates. Returned rows
contain only endpoint/predicate IDs, with invisible endpoints/predicates removed.
The extra row determines `has_more`; it is not returned. No count is performed.
The agent facade calls this function and retains its additional anonymous,
publication and noindex rules and existing continuation fields.

New model access points are:

```text
/api/model/edge/get/o_page/{id}/{predicate}?offset=0&limit=20
/api/model/edge/get/s_page/{id}/{predicate}?offset=0&limit=20
```

The predicate segment is optional. Unknown payload keys, malformed numbers,
negative offsets, limits outside 1–100, and offset plus limit above 10000 are
rejected. These are candidate pages: ACL filtering can leave an empty result
while `has_more` is true. Offset advances by the requested limit, not the number
of returned items. The core paths use caller ACLs, not the facade's anonymous-only
policy; no private row identifiers are returned. Candidate continuation does not
provide a visible-resource total and can reflect filtered candidates.

## Ordering and bounds

Outgoing pages order by `predicate_id, object_id`, supported by
`edge_spo_key (subject_id, predicate_id, object_id)`. Incoming pages order by
`predicate_id, subject_id`, supported by
`edge_ops_key (object_id, predicate_id, subject_id)`. Both unique indexes already
exist in the schema; no migration or new index is needed. The optimizer chooses
the actual access path based on data and statistics.

The agent facade uses endpoint-ID order, not editorial sequence order. Existing
full-list/sequence-oriented `m_edge` functions are unchanged. A future ordering
change would require clients to begin traversal anew.

The returned database row set is bounded, but SQL OFFSET still visits skipped
index entries: work is roughly offset plus limit, not constant-time for deep
pages. The existing 10,000-candidate cap remains. These are not snapshot pages;
concurrent inserts/deletes can shift offsets. A cursor API for traversals beyond
that bound is a separate future extension, not implied by this implementation.

## Validation

- Core tests cover both query directions, SQL-side predicate selection,
  lookahead exclusion, bounds, root/predicate ACLs, endpoint filtering, empty
  candidate pages and model-path decoding. Agent tests exercise the real edge
  function and add anonymous publication/noindex checks. See the consolidated
  [pilot findings](pilot-findings.md) for acceptance coverage.
- Live generic `s_page` reads at limit 3 traversed all fifteen `refers` sources of
  `catinclude`; `o_page` returned both targets. Concatenated IDs were sorted,
  unique and equal to the existing full-list edge accessors. Following the agent
  facade's `next_url` returned exactly the same IDs in both directions.

An isolated temporary table contained 100,000 incoming edges for one object and
predicate, with both equivalent unique indexes and fresh statistics. It existed
only within a transaction and was dropped automatically on commit; no persistent
resources or edges were created. `EXPLAIN (ANALYZE, BUFFERS)` compared:

| Query | Plan behavior | Execution time |
| --- | --- | ---: |
| Old order, first 21 candidates | Scan 100,000 rows and top-N sort | 15.511 ms |
| New order, first 21 candidates | Index-only scan, 21 heap fetches | 0.039 ms |
| New order, offset 9,900, next 21 | Index-only scan, 9,921 heap fetches | 1.318 ms |

These are single local synthetic measurements, not production capacity claims.
The homogeneous fixture let the planner use the outgoing unique index for the
incoming query as well; the important observed difference is elimination of the
full-set sort, not a forced index choice. The deep-page measurement explicitly
shows OFFSET's remaining cost. Raw plans are saved in
[bounded-edge-page-plans.txt](bounded-edge-page-plans.txt).

The benchmark's table shape was `(id, subject_id, predicate_id, object_id, seq)`;
rows used IDs/subjects 1–100000, predicate 7, object 1, and reverse sequence.
The two comparisons used `WHERE object_id=1 AND predicate_id=7`, with old
`ORDER BY predicate_id,seq,id` versus new `ORDER BY predicate_id,subject_id`,
`LIMIT 21`, and offsets 0/9900. Temporary-table heap fetches reflect this fixture,
not production visibility-map behavior.

Human-facing HTML and header/head discovery remain unchanged. Survey blocks stay
excluded, and additional JSON-LD/Turtle support remains deferred until the SPARQL
merge.
