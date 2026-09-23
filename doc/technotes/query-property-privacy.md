# Property privacy in SQL queries

`z_search_acl_props` is the shared policy and SQL guard API for search compilers.
It complements resource visibility checks; it does not replace them. Standard
search and SPARQL record dependencies when resolving properties. The shared term
compiler emits their guards within each query scope; it does not parse SQL text.

## Policy

`policy(Property, Context)` takes a canonical binary resource property name.
Admins receive `allow`. Other callers use the first `acl_query_prop` observer:

- `allow`: no extra condition.
- `deny`: the property cannot participate in this query scope.
- `{privacy, MaxLevel}`: allow `rsc.privacy` from zero through this nonnegative integer level.

Undefined observer responses delegate; no response allows access, so ACL modules
without a private-property concept need no observer. Explicit denials and invalid
policies deny access. The existing per-resource `acl_is_allowed_prop` check is unchanged.

`mod_acl_user_groups` reuses its private-property lists. Anonymous users may query
public values (0); members may query levels from 0 through 10. Always-private
billing fields are denied to both. Owner, editor and shared-group exceptions are
intentionally excluded. Persons default to 30; other resources default to 0.

## SQL guard

```erlang
{Guard, Args1} = z_search_acl_props:sql(
    <<"rsc">>, [<<"email">>, <<"phone">>], Args, Context).
```

Append `Guard` to this scope's conditions and use `Args1`. Empty SQL means no
additional restriction; `false` denies the scope. Equivalent policies are
combined once, and parameters follow the existing arguments. The alias identifies
an `rsc` row with a materialized `privacy` column.

Guards must be applied where values enter the query, before filtering, sorting,
aggregation and pagination. Keep guards inside OPTIONAL, UNION and subquery
branches. Derived columns, facets and expressions must declare their canonical
source properties. Never apply this as a post-result filter.

The guard is `rsc.privacy BETWEEN 0 AND $n::integer`. Pending conversion uses `-1`,
which no audience policy accepts. Do not cache generated guards across ACL contexts.

## Stored defaults and conversion

`rsc.privacy` starts at `-1`. Nullable `privacy_is_default` distinguishes legacy
rows (NULL), derived defaults (true), and explicit privacy (false).
`z_rsc_defaults` runs after modules are ready and converts 100 rows per batch.
It merges legacy properties with JSON taking precedence, applies the same
`rsc_get` fold as normal reads, and stores privacy and a missing content group.
Only these defaults are persisted; unrelated computed fetch properties are not.
The normal `z_db` update serializes JSON and clears legacy `props` atomically.

The persistent task advances by resource ID, retries unresolved rows on subsequent
sweeps, and flushes changed resource caches after commit. A partial index restricts
these sweeps to pending rows. Failed or malformed privacy remains closed.

Resource writes resolve defaults inside their transaction. Category-tree and
module-policy changes queue a gradual rebuild: existing privacy remains in place
until each resource is processed, including changes to a more restrictive policy.
There is no bulk reset to -1 and resource writes do not take a shared advisory lock.
Only never-migrated or unresolved rows remain closed with privacy -1.

Rebuilds page through at most 100 resource IDs per transaction and preserve explicit
privacy. Their cursor is persisted by the pivot task queue. Replacing a rebuild
allocates a new task ID so a running old task cannot overwrite the new cursor.
A brief schema-scoped advisory lock serializes only those queue replacements.

Startup compares stable `rsc_get` observer identities with a stored fingerprint.
The same observers resume existing tasks without resetting their cursors or
rebuilding resolved rows. A changed observer set queues a rebuild. Observer process
IDs are excluded from the fingerprint. Missing content-group defaults are filled;
existing content-group assignments are never overwritten.

Modules changing defaults through configuration (without changing observers) should
call `z_rsc_defaults:invalidate/1` to queue a rebuild. Custom direct SQL writes must
maintain privacy or set it to -1 for recomputation. Default-resolution observers
must remain independent of the requesting user, as for cached `rsc_get`.

## Compiler integration and trusted indexes

`#search_sql_term.property_sources` contains `{ResourceAlias, Source}` pairs.
Sources are `{column, Table, Column}` or `{jsonb, Table, Column, Path}`. Attach them
with `add_source/3` where the compiler resolves the expression. The owning resource
alias is essential when the value comes from a joined table.

The term compiler combines direct dependencies within a conjunction, emits guards
before aggregation, and keeps OPTIONAL, UNION and EXISTS branches independent.
Ordinary search negation also requires permission outside the negated expression,
so denying a property cannot itself make a negated filter match.

Custom pivot tables (`pivot_*`), `search_facet`, and full-text indexes
`rsc.pivot_tsv` and `rsc.pivot_rtsv` are trusted. Their indexers must exclude content
which is not safe to search. No property guards are added for these sources.
Non-full-text pivot columns **on the rsc table** are different: known address,
location and date pivots inherit their source-property protection. Unknown rsc
pivots and whole JSON property containers are denied for non-admins.

Modules can observe `#acl_query_source{source = Source}` and return
`{ok, [CanonicalProperty]}` to declare dependencies for other mappings. An explicit
`{ok, []}` declares a public source. `deny` blocks it; `undefined` delegates to the
next observer and then the built-in mappings. These are trusted module declarations,
not options accepted from the query author.

The compilers generate guards using the current context on every compilation; they
do not cache authorization decisions. Callers caching complete results must still
vary by the complete resource ACL context and invalidate on category/privacy changes.

## Standard search property selectors

Use `prop:email` to filter a value in `rsc.props_json`, or `prop:contact.city` for
nested objects. `filter:email` also selects the JSON property when there is no
physical `rsc.email` column. Physical columns take precedence for `filter:`.
The explicit `prop:` form avoids a database-column lookup.

For example, a query term is:

```json
{"term": "prop:email", "operator": "=", "value": "person@example.com"}
```

Sorting uses `sort=prop:email` or `sort=-prop:email` (also available for `asort`
and `zsort`). Nested selectors authorize the top-level property: selecting
`prop:address_city.label` requires access to `address_city`.

Paths and values are SQL parameters. Comparisons retain JSON types, so numbers
and booleans should be passed as JSON numbers and booleans. Ordering comparisons
require the same JSON type; `~` does text-prefix matching. `null` tests both an
absent path and JSON null. Multiple values are OR alternatives. To compare an
array itself, use a value/operator map, for example:

```json
{"term": "prop:tags", "value": {"operator": "@>", "value": ["news"]}}
```

The same scoped ACL guards apply to filtering, sorting, negation and count queries.

## Trusted Erlang searches

An explicit Erlang options map can omit property privacy guards:

```erlang
z_search:search(<<"query">>,
    #{<<"q">> => [#{<<"term">> => <<"prop:email">>,
                     <<"value">> => <<"person@example.com">>}]},
    1, 20, #{no_privacy_check => true}, Context).
```

This skips property guards in all nested query scopes, including SPARQL terms.
Resource visibility, publication, content-group ACLs, and returned-property ACL
filtering still apply. The option is also accepted by the Erlang term compiler
`z_search_terms:combine/3`.

Request/model options and options embedded in search arguments cannot enable it:
`map_to_options/1` deliberately excludes both atom and binary spellings. Only the
literal atom option with value `true` in the explicit Erlang argument is accepted.

## Insert defaults and migration status

`mod_acl_user_groups` supplies privacy and the default content group in `rsc_insert`,
before the initial database insert. Its category privacy policy is shared with
`rsc_get`, so new resources and migrated resources use the same defaults. Explicit
values remain explicit; the protected provenance flag preserves module defaults.

The ACL module contributes the Privacy and content groups item to the shared
migration panel on the admin dashboard and admin/status. Core reports whether
privacy, JSON properties, or content groups need conversion and whether a rebuild
is queued or a conversion sweep is active. A partial pending-row index supports
`EXISTS`; neither table-wide counts nor scans are needed.

An admin can start the combined migration if no migration is queued/running.
The backend repeats this check while holding a site-scoped advisory lock; migration
batches use its shared counterpart. Simultaneous manual starts cannot queue duplicate
work. This lock is only for migrations, not ordinary resource updates.

Modules extend `#migration_status{}` with items containing `id`, `title`,
`description`, `is_needed`, `is_running`, `can_start`, and optionally `url`.
They handle `#migration_start{id=Id}` to queue their work in the start transaction.
Module migrations must report queued work as running and call
`z_migration:batch_lock/1` inside each batch transaction. Starts go through
`z_migration:start/2` (admin-only), also exposed as the admin_status model's
`migration/start/<id>` POST endpoint. All start buttons are suppressed while
any migration reports queued/active work.
