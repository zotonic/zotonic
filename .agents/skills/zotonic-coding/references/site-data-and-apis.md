# Zotonic Site Data and APIs

Use the least invasive interface that exposes the required data. Prefer public,
read-only HTTP model calls for a remote site and direct model calls for a local
running node. Use SQL only when the model layer cannot answer the inspection
question.

## Inspect a Running Site over HTTP

Zotonic exposes models through this URL shape:

```text
/api/model/<model>/<get|post|delete>/<model-path...>
```

The path after the method is passed to `m_<model>:m_get/3`, `m_post/3`, or
`m_delete/3`. GET query arguments are supplied as the model-call payload. Read
the model's `-moduledoc` and its callback clauses before assuming that a path or
argument is supported.

Useful read-only calls include:

```shell
# Resource properties visible to the caller.
curl --fail --silent --show-error \
  --header 'Accept: application/json' \
  'https://example.com/api/model/rsc/get/1234'

# Full portable representation, including visible medium and outgoing edges.
curl --fail --silent --show-error \
  --header 'Accept: application/json' \
  'https://example.com/api/model/rsc_export/get/1234'

# Search and expand selected properties instead of returning only resource ids.
curl --fail --silent --show-error --get \
  --header 'Accept: application/json' \
  --data-urlencode 'cat=article' \
  --data-urlencode 'page=1' \
  --data-urlencode 'pagelen=20' \
  --data-urlencode 'options.properties=title,summary,publication_start' \
  'https://example.com/api/model/search/get/paged'
```

API responses normally wrap the value as `{"status":"ok","result":...}`.
Use a resource id, unique name, or URI where the called model accepts an
`m_rsc:resource()`. For a local `.test` site with a self-signed certificate,
add `--insecure` (`-k`) to `curl`; do not use it for normal production HTTPS.

Calls run with the request's ACL context. Anonymous calls only expose public
data. For protected data, use an authorized browser session or an OAuth2 Bearer
token with the minimum necessary permissions:

```shell
curl --fail --silent --show-error \
  --header 'Accept: application/json' \
  --header 'Authorization: Bearer <token>' \
  'https://example.com/api/model/rsc_export/get/1234'
```

Do not print, commit, or persist bearer tokens. HTTP `POST` and `DELETE` model
calls can mutate the site; only issue them when the user requested that change.
In browser JavaScript, the equivalent server-model topic is
`bridge/origin/model/<model>/<get|post|delete>/<model-path...>`.

## Inspect a Site from the Erlang Shell

Create the context for the exact site before reading data:

```erlang
C = z:c(example_site).
Id = m_rsc:rid(<<"page_home">>, C).
m_rsc:get(Id, C).
m_rsc_export:full(Id, C).
m_search:search(<<"query">>, #{ <<"cat">> => <<"article">> }, C).
```

`z:c/1` creates an ordinary site context; it does not grant administrator
rights. Use `z_acl:sudo(C)` only when privileged inspection is in scope. The
export model is especially useful for understanding a resource because it
returns its identity, resource properties, category ancestry, visible medium,
page URLs, and visible outgoing edges in one map.

For database-level diagnosis, keep SQL parameterized and read-only:

```erlang
z_db:qmap(
    "select id, name, category_id from rsc where id = $1",
    [Id],
    C).
```

Use `m_rsc`, `m_edge`, `m_media`, `m_search`, and other models before querying
their backing tables directly. They encode ACL, translation, pivot, and data
shape behavior that raw SQL omits.

## Import a Remote Resource for Local Testing

`m_rsc_import` consumes the representation produced by `m_rsc_export`. Confirm
that the target is the intended local development site before importing:

```erlang
Local = z:c(local_test_site),
development = m_site:environment(Local),
LocalAdmin = z_acl:sudo(Local),
Uri = <<"https://source.example/api/model/rsc_export/get/1234">>,
m_rsc_import:import_uri(Uri, [], LocalAdmin).
```

The default creates a non-authoritative resource linked to its source URI, so
it can be reimported. For an editable, independent test copy, pass
`is_authoritative`. To skip media downloads, pass `is_no_medium_download`:

```erlang
m_rsc_import:import_uri(
    Uri,
    [is_authoritative, is_no_medium_download],
    LocalAdmin).
```

When the source is another local `.test` site with a self-signed certificate,
the development context normally enables insecure fetching automatically. It
can be explicit when needed:

```erlang
m_rsc_import:import_uri(
    <<"https://source.test:8443/api/model/rsc_export/get/1234">>,
    [{fetch_options, [insecure]}],
    LocalAdmin).
```

Use `import_uri_recursive/3` only when connected resources should also be
copied; inspect the export's `edges` first because a recursive import can pull
in a much larger graph. If both sites run in the same Erlang node, the HTTP
round trip can be avoided:

```erlang
Source = z:c(source_site),
{ok, Export} = m_rsc_export:full(1234, Source),
m_rsc_import:import(Export, [is_authoritative], LocalAdmin).
```

Imports mutate the target site. Do not run these examples against production,
and do not use a sudo source context to copy private data unless that data is
explicitly in scope.
