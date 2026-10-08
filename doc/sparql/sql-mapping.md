# Mapping SPARQL to SQL

The module maps a supported SPARQL `SELECT` query to Zotonic SQL search terms.
`z_search_terms` combines those terms into a `#search_sql{}` query, after which
the normal Zotonic search code adds final SQL and executes it.

The processing stages are:

1. Parse SPARQL into an AST.
2. Normalize the AST into a SPARQL query plan.
3. Map the plan to typed Zotonic SQL terms.
4. Combine nested SQL terms and optimize the combined query.
5. Add access-control and category restrictions for every resource alias.
6. Generate and execute parameterized PostgreSQL SQL.

Predicate mappings can target:

- Zotonic edges, including reversed predicates
- Columns in `rsc`, pivot or facet tables
- JSONB selectors in `rsc.props_json`
- Resource category and category hierarchy mappings
- Full-text or trigram search columns

Namespaces are normalized using the `#rdf_ns{}` notification. Predicate
mappings are supplied through `#sparql_mapping{}`. Unknown namespaces remain
expanded; an unknown or variable predicate cannot currently be converted to
SQL.

## Deferred: variable predicates

Variable-predicate support is deferred (2026-10-05). The proposed first step is
to support predicates restricted to an explicit `VALUES` list, returning one
property/value pair per row:

```sparql
PREFIX zotonic: <http://zotonic.net/predicate/>
SELECT ?property ?value WHERE {
    VALUES ?property { zotonic:title zotonic:summary }
    <https://example.org/id/123> ?property ?value .
}
```

This query is not currently supported. The parser and planner represent variable
predicates, but SQL generation rejects them. Returning property/value rows would
avoid the combinations produced by independently matching several multivalued
properties into separate columns.

Implementation needs predicate-IRI handling distinct from resource-ID bindings,
support for a fixed subject, and SQL branches that produce separate rows while
preserving heterogeneous value types and RDF metadata. The existing `UNION`
mapping combines search conditions and cannot directly provide these result
branches. Each branch must retain resource and property ACL checks; filters,
ordering and pagination need regression coverage.

The initial estimate is 3–5 developer days including tests and documentation.
Unrestricted discovery (`?subject ?property ?value` without a finite predicate
list) is a separate, larger scope: define discoverable predicates and canonical
IRIs across columns, JSON properties, edges, facets and module mappings. The
current `sparql_mapping` notification resolves supplied predicates rather than
enumerating them. The rough estimate for that broader scope is 2–4 developer
weeks total, with lower confidence.

These estimates exclude expanding `#trans{}` records into multiple bindings.
The endpoint's existing language-fallback serialization would remain in place.

## Nesting

- A graph group is a conjunction of search terms.
- `UNION` becomes an `anyof` nested search term.
- `OPTIONAL` becomes a correlated `LEFT JOIN LATERAL`; bindings produced by its
  right-hand graph pattern are projected as nullable columns.
- Standalone `FILTER EXISTS` and `FILTER NOT EXISTS` become correlated
  `EXISTS` and `NOT EXISTS` subqueries. Inner bindings remain private.
- Joins local to a nested alternative are compiled into correlated `EXISTS`
  subqueries.
- Shared/projected aliases remain in the outer query when required there.

The complete right-hand side of an `OPTIONAL`, including its filters, tables,
category restrictions and nested alternatives, stays inside the lateral
subquery. This prevents a right-hand restriction from leaking into the outer
`WHERE` and changing the left join into an inner join.

`BIND` and `GRAPH` are represented by the parser and plan but are currently
rejected by SQL generation.

## Resource IRIs

A fixed resource IRI is resolved with `m_rsc` and represented in SQL by the
resource id. Resource variables are represented by `rsc` aliases and projected
as ids, matching Zotonic search result conventions. Prefixed and relative query
IRIs are expanded during planning.

## Expressions and aggregates

Expressions carry their logical type and storage source. JSONB values remain
JSONB until an operator or function requires text, numeric, boolean or datetime
coercion. See `builtins-type-mapping.txt` and `builtins-mapping.txt`.

The generated search terms include projection, `GROUP BY`, `HAVING` and
`ORDER BY` clauses. Supported aggregates are mapped by
`z_sparql_sql_aggregate`.

## Access control

`z_search_acl` applies Zotonic ACL and category checks to resource aliases.
Checks for resource aliases local to a nested branch or graph-pattern filter are
added before that branch is compiled into an `EXISTS`, `NOT EXISTS`, or OPTIONAL
lateral subquery. Checks for
outer aliases are added when the combined search query is reformatted. This
prevents a subquery from bypassing content-group or other resource visibility
restrictions without making a failed OPTIONAL remove its left-hand row.

Predicate mapping itself also prevents access to protected resource properties:
properties which are not exposed by the mapping cannot be queried.

## API

Queries are executed with:

    z_sparql:search(Sparql, Context)
    z_sparql:search(Sparql, Arguments, Context)
    z_sparql:search(Sparql, OffsetLimit, Context)
    z_sparql:search(Sparql, Arguments, OffsetLimit, Context)

These functions return a Zotonic `#search_result{}` and retain their existing
projection and paging contract. The separate `/sparql` endpoint uses
`z_sparql_sql:result_plan_to_sql/2` to project the requested variables with RDF
metadata. `z_sparql_results` builds normalized bindings; `z_sparql_results_encode`
serializes JSON, XML, CSV or TSV according to HTTP content negotiation. Endpoint
pagination consumes the outer query LIMIT/OFFSET before SQL compilation unless
explicit HTTP paging overrides it. See the module README for request formats
and the supported SELECT subset.

## Language values

Language tags travel alongside SQL values as RDF metadata.
`zotonic:translation(value, language)` performs exact selection;
`zotonic:translationFallback(value, language)` uses Zotonic's canonical language
and fallback chain, then site default, English and any available translation.
Both accept translation records, JSONB strings, text/varchar values and nulls.
They produce one value with its actual language tag, without expanding rows.

The compiler calls versioned PostgreSQL helpers installed by `mod_sparql` schema
version 2. It projects the returned text and language separately, so `COALESCE`,
ordering and `DISTINCT` operate on the selected translation. Directly selected
`#trans{}` values retain the endpoint's context-language fallback at serialization.
See [language functions and examples](builtins-lang-support.txt) for semantics
and the helper upgrade procedure.
