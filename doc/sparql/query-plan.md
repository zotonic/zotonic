# SPARQL query plan

`z_sparql_plan` normalizes a parsed query into the representation consumed by
`z_sparql_sql`. PostgreSQL performs the final relational query optimization.

The current pipeline is:

1. Parse the query with `z_sparql_scanner` and `z_sparql_parser`.
2. Accept a `SELECT` query; other parsed query forms return
   `unsupported_query`.
3. Resolve `BASE` declarations and expand prefixed names.
4. Normalize RDF literals, IRIs and query blank-node identifiers.
5. Normalize pre-bound Erlang arguments to typed query variables.
6. Map graph groups to plan nodes: joins, unions, filters, optional joins,
   bindings, values and graph nodes.
7. Resolve namespaces through the `#rdf_ns{}` notification.
8. Resolve predicates through the `#sparql_mapping{}` notification to resource
   columns, pivot/facet columns, JSONB selectors, categories, subclasses or
   edges.
9. Map expressions, XSD constructors, aggregates and the Zotonic full-text
   extension functions.
10. Find the root resource variable used by Zotonic search and collect the
    projection, grouping, having and ordering expressions.
11. Pass the plan to `z_sparql_sql`, which emits nested Zotonic SQL search terms.

## SQL-supported plan nodes

- Triple patterns with a fixed mapped predicate
- Nested graph groups
- `UNION`
- `OPTIONAL`
- `FILTER` expressions using supported operators and built-ins
- `VALUES`, including `UNDEF`
- Projection expressions and `DISTINCT`
- `GROUP BY`, `HAVING` and supported aggregates
- `ORDER BY`
- A single inverse predicate path (`^predicate`)
- Blank-node property-list syntax as query-local resource bindings

`UNION` branches become `anyof` search terms. Joins used only inside a branch
are compiled to correlated SQL `EXISTS` subqueries instead of being lifted into
the outer query.

`OPTIONAL` becomes a correlated SQL `LEFT JOIN LATERAL`. Its right-hand graph
pattern is compiled in a separate scope. Local tables, filters, categories and
ACL checks stay inside the lateral subquery, and bindings exported from it are
nullable in the outer query. Multiple matching right-hand solutions retain the
normal SPARQL row-multiplication behavior.

## Parsed or planned but rejected by SQL generation

- `CONSTRUCT`, `DESCRIBE` and `ASK` (the planner accepts only `SELECT`)
- Dataset clauses (`FROM` and `FROM NAMED`)
- Query `LIMIT` and `OFFSET`; pagination is supplied separately to
  `z_sparql:search`
- `BIND`
- `GRAPH`
- Variable predicates
- Queries without a root resource variable

The parser covers some constructs that are intentionally rejected later. This
keeps syntax recognition separate from the currently implemented SQL subset.
`REDUCED` is parsed and currently emitted like the default projection, which is
permitted because SPARQL allows but does not require duplicate elimination for
that modifier.

SPARQL `EXISTS` and `NOT EXISTS` filter expressions are not parsed. The SQL
`EXISTS` generated for local nested search terms is an implementation detail and
does not provide those SPARQL built-ins.
