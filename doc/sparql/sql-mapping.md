Mapping to SQL
==============

We will map the SPARQL query to Zotonic SQL terms.
The SQL terms are then combined into a SQL query using the usual Zotonic
query builders.

**NOTA BENE** there is a problem with nested queries and table references. Right now the
table definitions are "lifted" to the top-level, where it should be lifted to the highest
level (anyof/noneof) where the table is needed. For that we need to emit nested queries.

Steps:

1. Parse the SPARQL query into an AST
2. Make a SPARQL query plan for the given AST
3. Map the query plan to Zotonic SQL terms
4. Merge Zotonic SQL terms for optimization
5. Generate the SQL query from the SQL terms

For the terms we can map all predicates to either:

- edges with a certain predicate (note that the direction can be reversed in Zotonic)
- direct columns, in the rsc, pivot or facet tables
- jsonb selector in the props_json column of a rsc

Namespaces are normalized using the `#rdf_ns{}` notification, so that mapping
routines can recognize compact namespace prefixes. Unknown namespaces are kept
in full.

Nesting
-------

The SPARQL nesting can be mapped:

- Graph group: 'allof'
- Union: 'anyof'
- Negation: 'noneof'
- Optional: left join

Resource URIs
-------------

When encountering fixed resource URI in a query we can map it to the correct 
resource id using the m_rsc routines.

Translations and language handling
----------------------------------

TBD

Tricky, as the #trans{} tuples are not compatible with SPARQL, and we allow
a mix of binaries and/or #trans{} tuples as values.


SPARQL API
----------

First we will concentrate on mapping SPARQL queries to SQL queries, and running those
SQL queries. If that works then we will check if we can implement a SPARQL API which will
then need to map the returned values to SQPARL results.

The first API will just accept a SPARQL query and then return a JSON with the results.
(m_get with query in payload).

ACL
---

The predicate mapping can restrict access to certain properties, to prevent searches on private
properties (like email, address etc.)

The Zotonic SQL query generator adds the needed access control on a per-resource
basis.
