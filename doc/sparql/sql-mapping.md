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

## Nesting

- A graph group is a conjunction of search terms.
- `UNION` becomes an `anyof` nested search term.
- Joins local to a nested alternative are compiled into correlated `EXISTS`
  subqueries.
- Shared/projected aliases remain in the outer query when required there.

`OPTIONAL`, `BIND` and `GRAPH` are represented by the parser and plan but are
currently rejected by SQL generation.

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
Checks for resource aliases local to a nested branch are added before that
branch is compiled into an `EXISTS` subquery. Checks for outer aliases are added
when the combined search query is reformatted. This prevents a subquery from
bypassing content-group or other resource visibility restrictions.

Predicate mapping itself also prevents access to protected resource properties:
properties which are not exposed by the mapping cannot be queried.

## API

Queries are executed with:

    z_sparql:search(Sparql, Context)
    z_sparql:search(Sparql, Arguments, Context)
    z_sparql:search(Sparql, OffsetLimit, Context)
    z_sparql:search(Sparql, Arguments, OffsetLimit, Context)

The result is a Zotonic `#search_result{}`. There is no complete SPARQL Protocol
endpoint or SPARQL Results JSON/XML serializer in this module.

## Language values

Language-tagged literals and Zotonic `#trans{}` values are not yet represented
by the SQL expression layer. See `builtins-lang-support.txt`.
