---
keywords:
  - how_to_guide
  - backend_developer
  - http
module: mod_agent_discovery
---

# Help agents discover a site's public APIs

Enable `mod_agent_discovery` on a public publishing site, using the Modules
page in the admin. It depends on `mod_base` and `mod_search`; it does not require
`mod_seo`. Activation adds these routes:

| URL | Purpose |
| --- | --- |
| `/api-help` | Readable API explanation with a clickable public search example |
| `/openapi.json` | OpenAPI 3.1.0 description of existing synchronous search and resource export |
| `/.well-known/api-catalog` | Linkset catalog pointing to the public API and its descriptions |
| `/llms.txt` | Concise site guidance and links for agents |

Set `mod_agent_discovery.description` to an optional public description of the
site's information. The site title comes from the existing site configuration.
No API key is needed to read public content. The existing APIs continue to apply
the caller's permissions; this module does not grant access or change query limits.

## Link from pages

Sites using the normal head hook automatically get discovery links:

```django
{% all include "_html_head.tpl" %}
```

Also include a visible link in the footer or resource page so clients that only
read extracted page text can discover the API:

```django
{% optional include "_agent_discovery_link.tpl" %}
```

The optional include keeps the template working if the module is disabled.
Override `agent_api_help.tpl` to customize the explanation for the site.
Its `discovery` variable contains public metadata and documented operations.
The sitemap is linked when `mod_seo_sitemap` is active.

Standard resource responses receive HTTP discovery links as well. Custom
controllers must call `z_context:set_resource_headers/2` to receive them.
Existing Link values, including WebSub advertisements, are preserved when the
discovery observer runs. Later custom observers must also merge Link values.

A JSON resource alternate is advertised only when the resource is visible to an
anonymous visitor and is not excluded by site, resource or category noindex
settings. Visibility is checked even when the visitor is an administrator.
These exclusions affect discovery, not the permissions of existing API routes.

## Verify the site

Visit an ordinary public resource while signed out. Check its HTML links and
HTTP `Link` header, follow the API-help link, run the search example, and use an
ID from the result to retrieve a full export. Check the export's language-specific
`page_url` map when citing it. Compare a private and a noindex resource to confirm
that no JSON alternate is advertised for either.

Check site-specific routes and proxy rules if a discovery URL is unavailable.
The module generates URLs through dispatch and uses anonymous metadata. Discovery
documents currently use no-cache headers; existing APIs keep their cache and
`X-Robots-Tag: noindex,nofollow` behavior. No robots.txt or CORS settings are changed.
Do not infer reuse licenses from public accessibility; retain the site's stated
content and media attribution and licensing terms.

This is the first discovery implementation. Concise bounded read APIs, schema and
relationship discovery, Markdown representations, and live agent trials remain
in the [implementation plan](../technotes/agent-discovery-and-access.md).
Additional JSON-LD and Turtle work waits for the SPARQL merge. No MCP endpoint is
advertised.
