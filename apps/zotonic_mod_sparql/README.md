SPARQL
======

This module parses SPARQL queries and translates them into SQL queries for the
Zotonic datamodel.

It maps Zotonic predicates directly. For mapping to properties the module uses notifiers,
zotonic_rdf, and m_rdf.

The generated SQL joins all needed resources and returns the found resources and properties.

To be able to query the resource properties, we are using PostgreSQL JSONB selectors.
If a sparql predicate maps to a Zotonic predicate then we use joins via the edge table.

Because a notifier is used to map to the properties, this module can use pivot and facet tables
for the queries.


Default BASE and prefixes
-------------------------

Every SPARQL query has a site-aware implicit prologue:

- `BASE` is the language-neutral base URL of the site.
- The default `:` prefix is the language-neutral resource URI namespace from
  the site's `id` dispatch rule.
- `site:` is an alias for that same local resource namespace. Local namespaces
  are normalized to this prefix in query plans.
- `zotonic:` is `http://zotonic.net/predicate/`.

Explicit `BASE` and `PREFIX` declarations override these defaults. Both local
`site:` IRIs and generic `zotonic:` IRIs are resolved to local resources when
possible. This allows concise site queries such as:

```sparql
SELECT ?article WHERE {
    ?article zotonic:is_published true .
    ?article :relation :123
}
```

Local resources after `:` can be identified by either their numeric id or their
unique name, for example `:1` and `:administrator` identify the same resource.


EXISTS expressions
------------------

`EXISTS` and `NOT EXISTS` can be used in standalone filters, compound expressions,
and result expressions:

```sparql
SELECT ?article (EXISTS { ?article :relation ?related } AS ?hasRelation)
WHERE {
    ?article zotonic:is_published true .
    FILTER (EXISTS { ?article :author ?author } || NOT EXISTS { ?article :relation ?related })
}
```

The inner pattern uses the current outer bindings and returns a boolean. Variables
introduced inside the pattern remain local. Projecting an existence check preserves
outer rows, including rows without a match; multiple matches do not duplicate a row.
The same expressions can be used in functions such as `IF`, `ORDER BY`, and `HAVING`,
subject to the normal grouping rules. `EXISTS {}` is true and `NOT EXISTS {}` is false.


Full-text search
----------------

The Zotonic extension functions `zotonic:fullText` and `zotonic:fullTextRank` search a resource:

```sparql
# Two parameter searches, using default pivot
FILTER(zotonic:fullText(?r, "search text"))
zotonic:fullTextRank(?r, "search text")
```

Or search a fulltext or fts column:

```sparql
# Three parameter searches on columns or text values
FILTER(zotonic:fullText(?r, zotonic:facet.sometextindex, "search text"))
zotonic:fullTextRank(?r, zotonic:pivot.name.texts, "search text")
```

The two-argument function uses `rsc.pivot_tsv`. The three-argument function selects a mapped facet or
pivot column, for example `zotonic:facet.transcript` or `zotonic:pivot.articles.search_text`.
The match text must be a string literal.

`fts` searches use `mod_search` to_tsquery function and rank_weight/rank_behaviour configurations.

`fulltext` and normal text columns use the trigram word-similarity operator for fuzzy searching.
For fulltext the GIN trigram index is used and ranking is done by `word_similarity`.
For efficiency you must ensure that a GIN index is placed on the pivot text column, the facets
will handle this automatically (block name `..._fulltext`).

In the future, a GiST index might be useful for better ranking.


Parametrized queries
--------------------

SPARQL queries can be parametrized. The parameters are passed as pre-bound 'VALUES' to the query and
can be accessed by name `?name` in the query.

The parameters are passed as a map to the query:

```
Args = #{
   count => 1
   myrsc => {rsc, 1}
},
Query = <<"
      SELECT ?r WHERE {
         ?r zotonic:relation ?myrsc .
         ?r zotonic:count ?count .
      }
">>,
z_sparql:search(Query, Args, {1,10}, Context)
```

The value of an argument is one of:

- Resource: {rsc, ResourceReference}
- IRI: {iri, Iri}
- Text: binary, unicode data or an atom.
- Boolean: true or false
- Integer
- Float
- Datetime: {{Y,M,D}, {H,I,S}}
- Date: {Y,M,D}, converted to {{Y,M,D}, {0,0,0}}
- Undefined: use the atom `undefined`

JSON callers can represent values that need an explicit RDF interpretation as:

```json
{
  "related": { "type": "resource", "value": 123 },
  "homepage": { "type": "iri", "value": "https://example.test/" },
  "since": { "type": "datetime", "value": "2026-08-20T10:00:00Z" },
  "day": { "type": "date", "value": "2026-08-20" }
}
```

Model API
---------

`m.sparql` and `m.search.sparql` both run SPARQL through Zotonic's normal
search pipeline. Consequently paging, result formatting and resource ACL
restrictions are the same as for other searches.

The arguments are a map containing `query` and, optionally, a nested `args`
map of named pre-bound SPARQL variables:

```erlang
SearchArgs = #{
    <<"query">> => Sparql,
    <<"args">> => #{
        <<"related">> => #{ <<"type">> => <<"resource">>, <<"value">> => 123 }
    },
    <<"page">> => 1,
    <<"pagelen">> => 20
},
{ok, Result} = m_sparql:search(SearchArgs, Context),
{ok, Result} = m_search:search(<<"sparql">>, SearchArgs, Context).
```

The model paths `/`, `/paged` and `/count` are available through the normal
Zotonic model API. SPARQL can project multiple values when called directly;
the result rows then use the normal SPARQL result shape.

Query resources
---------------

The admin query editor automatically classifies search-term, JSON search and
SPARQL text, shows its classification and reports parser errors before saving.
The detected classifier name is stored in the `query_type` property, but it is
always checked again on the server.

A SPARQL query stored in a `query` resource can be invoked by its resource
name through `m.search`, like any other named search. Named pre-bound variables
are passed in the nested `args` map. Stored SPARQL queries must select exactly
one variable, and that variable must be the root resource. This preserves the
resource-id result contract of query resources.

Live query notifications remain limited to mod_search's search-term and JSON
formats. SPARQL query resources are never registered as live watches, even if
the old `is_query_live` property was set.

Additional modules can provide future query formats by observing the
`search_query_parse` notification and returning a classified, compiled search
descriptor.


RDF expression metadata
-----------------------

SQL expressions carry RDF term kind (`iri`, `literal`, or `bnode`), datatype
IRI, and language tag separately from their SQL storage and coercion types.
Literal and constructor datatypes are preserved exactly, including custom
datatype IRIs. Language tags are normalized to lowercase. Resource bindings
remain IRIs even when represented by integer database IDs.

Metadata follows VALUES rows, OPTIONAL bindings, projected aliases, IF and
COALESCE branches, string operations, and numeric promotion. It is represented
internally as SQL text expressions so it can vary per row. VALUES and OPTIONAL
use hidden metadata columns only for variables whose metadata is needed by
DATATYPE, including dependencies through projected aliases. Demand is tracked
per component and SQL scope: a local filter does not export metadata, direct
DATATYPE does not carry language tags, and aggregates that discard dynamic
metadata do not request it. CONCAT still requests the language tags needed to
determine its result datatype. Unrelated bindings do not add metadata columns.
The public result shape is unchanged. Boundness
is tracked separately: metadata alone does not mean that a value is bound.

Mapped SQL scalars and external arguments use canonical RDF types. JSONB
scalars use their JSON shape, refined by compatible mapping hints; numbers
default to `xsd:decimal`. Integer hints use `xsd:integer` only for integral
values. Arrays and tagged objects, including translation objects, have unknown
metadata. SQL NULL represents unknown or inapplicable metadata. Aggregates
preserve known static metadata; metadata for heterogeneous aggregate inputs
is currently unknown.

DATATYPE returns the datatype IRI for literals, including `xsd:string` for
plain strings and `rdf:langString` for language-tagged literals. It works in
result expressions and filters. Non-literals, unbound values, and unknown
metadata produce SQL NULL (an unbound result or a filter that does not match).
This is basic support; heterogeneous aggregate metadata remains unknown.
LANG, translation selection, RDF-aware equality, and RDF result serialization
are not yet implemented.


Translations
------------

TBD

Quite tricky, as the `#trans{}` tuples are not compatible with SPARQL, and we allow a mix of
binaries and/or `#trans{}` tuples as values.


TODO
----

 - [x] Basic SPARQL parser
 - [x] Basic mapping of SPARQL to SQL
 - [x] Simple test for complete SPARQL query to database
 - [x] Add the 'reversed' option to the predicate lookup edge result
 - [x] Handle 'is' (needs list of categories from nesting) also `rdf:type` and `rdfs:subClassOf`
 - [x] Add psql json selector for property on root level
 - [x] Add psql json selector fot property on nested level, allow dot syntax?
 - [x] Add basic mappings to properties using the z_rdf_props
 - [x] Add mappings to facet and pivot tables (use dot-syntax: facet.col, pivot.name.col)
 - [x] Mapping of SPARQL functions to (very) similar SQL functions (see doc/sparql/builtins.txt)
 - [x] Check extraction of JSON terms, do we need to use `#>>` ?? --> Makes type casting easier?
 - [x] Add type hints for the column and jsonb value mapping, use those for type casting
       when using '=' and other operators -- use these for type coercion in operators
 - [x] Add support for dates (from JSONB, literals, etc.) -- xsd:dateTime
 - [x] Support for aggregate function (count/min/max/etc.)
 - [x] Fix joins generated by z_search_terms, allow subqueries for anyof/noneof/left-join without joining at top level
 - [x] Add fulltext search support for fts and fulltext columns
 - [x] Figure out how parametrized queries can be implemented in SPARQL, especially considering types.
 - [x] Accept multi value VALUES in parser and planner (currently only single value is accepted)
 - [x] Optimize sort for generated SQL, remove all non-one-column sub-sorts after 'id' / 'created' / 'modified'
 - [x] With the new joins, fix the z_search checks on cat/cat_exact and acl for the subqueries
 - [x] Add isIRI/isURI/isBLANK RDF term tests
 - [x] Support OPTIONAL using isolated correlated lateral joins
 - [x] Support standalone FILTER EXISTS / FILTER NOT EXISTS
 - [x] Set default PREFIX (`:`) to the base URL of the `id` dispatch (no language)
 - [x] Set BASE to the base URL of the site (no language)
 - [x] Add default Zotonic PREFIX (`zotonic:`) set to `http://zotonic.net/predicate/`
 - [x] Add Zotonic m_sparql model for querying
 - [x] Allow SPARQL query in search_query resources
 - [x] Support EXISTS / NOT EXISTS inside compound and result expressions
 - [x] Carry RDF term kind, datatype and language metadata through expressions
 - [x] Add basic DATATYPE support
 - [x] ACL checks for private properties, only allow 'administrator users' to query on private properties
 - [ ] Import and export of Turtle, JSON-LD, and other formats (TBD)
 - [ ] Support language handling, using the JSON objects: { _type: "trans", tr = { "en":"..." } }, including LANG etc.
 - [ ] Add SPARQL endpoint, with expected results (for use with 3rd parties -- check API standards)
 - [ ] Check usage of rsc props_json vs props, migrate to props_json (accept both for now)
 - [ ] Ensure the types of z_props, mod_rdf and search_facet are the same (bool -> boolean, int -> integer)

After merge:
 - [ ] Endpoint with: Turtle, JSON-LD, and other formats as output
 - [ ] Fulltext query ranking, add options for trigram operator, thresholds and sorting (maybe named combos?)
