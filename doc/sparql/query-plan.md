SPARQL query plan
=================

How to map a parsed SPARQL query to a query plan, that can be used for the
SQL generation.

We let PostgreSQL do the basic optimizations for the generated SQL queries.

After parsing:

1. Check if the query is a SELECT
2. Collect a namespace mapping, find all namespaces from the query
   These namespaces will be normalized later for the SQL term generation.
3. Normalize RDF terms
    - URI/IRI mappings (maybe relative to BASE)
    - Expand FQNs
    - Maybe map values
4. Blank nodes: map blank nodes internal identifiers
5. Map groups / unions to tuples with 'left_join', 'union', 'filter', etc.
   (Keep tree structure of graphql)
6. Build expression trees
7. Map namespaces using the `#rdf_ns{}` notification (if unknown, keep the full namespace)
8. Map all triples to Zotonic props/columns/facet/pivots
   (if not defined, then decide: error or 'null'?)
9. Variable scoping: check if/where variables are bound and if they
   are used by filters or in expressions
   (check type usage?)
   Special attention to 'optional', as the variables introduced there
   are optionally bound (left join)
10. Map all triples to access functions: rsc/property/tables/where
11. Resolve found resource references using m_rsc
12. Find the main resource we will select from, this is used when building
    the SQL query
13. Find all expressions/variables used for the main select.
14. Same for the order by, distinct, limit etc.
15. Combine into SQL terms, using anyof/allof/noneof and left join.

We then have a nested SQL term definitions with arguments, which we can
flatten to a SQL query and execute.
