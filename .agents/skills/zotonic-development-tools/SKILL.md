---
name: zotonic-development-tools
description: Inspect and validate running Zotonic sites via CLI, HTTP, and MQTT, including remote sites accessed through an authorized browser session or OAuth2 administrator token. Use for dispatch and template inspection, lazy-render tracing, console capture, and checking site changes. SSH runs the CLI remotely.
---

# Zotonic Development Tools

Use the running site's indexes and traces to identify the code actually used by
that site. Source filenames alone do not establish which module override wins.

## Access and environment

Choose the interface available for the site:

- **CLI:** run commands locally on the server, or use SSH to run that same CLI
  remotely. SSH is a transport for CLI access, not a separate development API.
- **HTTP:** use the site's model endpoints with an authorized browser session
  or an OAuth2 bearer token belonging to an administrator with the required access.
- **MQTT:** use the authenticated browser's Cotonic bridge to call the model
  topics shown below.

Remote inspection does not require SSH or a local checkout when an authorized
browser session or OAuth2 administrator token is available. Use the site's
origin for HTTP calls and its authenticated browser tab for Cotonic calls.

The `inspect` and `capture` commands require a running Zotonic node and the
updated `mod_development` implementation. Their model APIs require administrator
and `use mod_development` access. All operations are limited to `development`,
`test`, and `acceptance`; production and other environments return `eacces`, even
over trusted SSH RPC. Do not change the environment or bypass these checks to
inspect production. Use an authorized non-production counterpart.

Run CLI commands from the Zotonic umbrella checkout as the normal server user
with access to its Erlang cookie/configuration. The SSH host, checkout and site
are separate identifiers; confirm the intended site before changing anything.
Use configured SSH access rather than copying keys or printing credentials.

```sh
# Local checkout
bin/zotonic status
bin/zotonic inspect mysite modules

# Same command on the server; replace the example host/path/site.
ssh site-host 'cd /srv/zotonic && bin/zotonic inspect mysite modules'
```

`inspect` and `capture` output JSON `{ok, result}` or `{ok: false, error}` and
return a nonzero exit status for model errors. Older installations may lack the
commands; inspect their source/version rather than guessing alternate APIs.

## Find the code behind a page

```sh
bin/zotonic inspect mysite dispatch
bin/zotonic inspect mysite dispatch-trace /nl/news/example
bin/zotonic inspect mysite template page.tpl
bin/zotonic inspect mysite template page.tpl article_resource_name
bin/zotonic inspect mysite templates _article
bin/zotonic inspect mysite modules
```

- Dispatch tracing accepts a local path, stays within the selected site, and
  does not render the controller. Pass the language prefix as seen in the URL.
- Template lookup returns the resolved name, winning filepath, and all matches
  for that name in module priority order. Supplying a resource id/name uses the
  real category selection rules (`catinclude`). Lower module priorities win.
- Template listing is a prefix search, not a recursive filesystem search. The
  CLI returns up to 50 entries; use the API's offset/limit for larger results.
- Modules lists active modules and priorities. Trace an actual render when
  conditions or runtime includes determine the final composition.

## Attach to an open page and capture lazy rendering

Prefer JavaScript in the intended page's browser console or browser execution
tool to attach and obtain the ID. There is no need to locate or click the button:

```javascript
const capture = await window.z_development_capture.attach();
capture.id; // Use this ID with CLI, HTTP, or MQTT capture commands.

await window.z_development_capture.status();
// Current capture state, or null if no attachment exists or it expired.

// After interacting with the page and fetching the evidence:
await window.z_development_capture.stop();
```

`attach()` preserves page state, reuses a valid attachment, restarts it if stopped,
and creates a replacement if it expired. Concurrent calls share one attachment.
All three functions return promises; permission and transport failures reject.
`status()` checks the server rather than trusting a possibly expired ID in
sessionStorage. `stop()` flushes the browser's buffered logs before stopping.

Use browser tools or normal interaction to scroll, open tabs, and trigger the
component under investigation. Fetch the trace and console evidence using the
returned ID, then stop the capture. Reload only when the initial render needs
to be traced; renders before attachment cannot be reconstructed.

The floating debug button's **Attach capture** and **Stop capture** controls use
the same functions and remain a human-friendly alternative; its panel shows the
ID. `bin/zotonic capture mysite list` also lists attachments. If the JavaScript
object is absent, check that the page has loaded the development tools, that the
trace button is enabled for the site, and that the browser has the required
administrator access on a supported non-production environment.

```sh
bin/zotonic capture mysite list
bin/zotonic capture mysite status CAPTURE_ID
bin/zotonic capture mysite start CAPTURE_ID
bin/zotonic capture mysite trace CAPTURE_ID
bin/zotonic capture mysite console CAPTURE_ID
bin/zotonic capture mysite stop CAPTURE_ID
```

Run these CLI commands locally or remotely through SSH, or use the HTTP/MQTT
equivalents below to control capture and fetch evidence. Capture control does
not execute browser interactions or take screenshots; use browser tools for
those tasks. If no page is attached, arrange an
attachment instead of treating an empty trace as evidence that no template ran.

Capture facts that affect interpretation:

- Template traces are session-scoped; other tabs in the same session can add
  renders. Console entries come from the attached page. Duplicating a tab can
  copy its sessionStorage capture ID; use a fresh attachment for isolation.
- Only subsequent `console.log` calls are collected. This is not the browser's
  historical console, network log, or an automatic capture of errors/warnings.
- Start is idempotent and retains accumulated evidence. A fresh API attachment
  gives a fresh graph; stop freezes the existing evidence.
- Captures expire ten minutes after attachment. Logs and graphs are bounded;
  inspect `truncated`. Browser log delivery is best effort, approximately once
  a second; disconnects, reloads and buffer overflow can lose entries.
- Stopping from the page flushes its current console batch. Stopping through the CLI or API
  freezes ingestion immediately and can lose the remaining browser buffer.
- Attachment survives reload through sessionStorage. It uses the logged-in
  browser identity; an admin rendering does not validate anonymous access.

## HTTP and MQTT equivalents

Use an existing authorized browser session or an OAuth2 administrator token
with the required permissions. For token-authenticated HTTP requests, send
`Authorization: Bearer <token>` over HTTPS. Keep credentials out of logs, source
files, and captured console output. A token can inspect the site and control/read
an existing capture; capturing browser logs still requires an attached page.
Do not enable the older unauthenticated recompile API to gain inspection access.

```text
GET  /api/model/development_tools/get/dispatch/list
GET  /api/model/development_tools/get/dispatch/trace?path=%2Fnl%2Fnews
GET  /api/model/development_tools/get/template/lookup?name=page.tpl&id=article_resource_name
GET  /api/model/development_tools/get/templates?prefix=_article&offset=0&limit=50
GET  /api/model/development_tools/get/modules

GET  /api/model/development_capture/get/list
GET  /api/model/development_capture/get/status/CAPTURE_ID
GET  /api/model/development_capture/get/trace/CAPTURE_ID
GET  /api/model/development_capture/get/console/CAPTURE_ID
POST /api/model/development_capture/post/start/CAPTURE_ID
POST /api/model/development_capture/post/stop/CAPTURE_ID
```

HTTP responses wrap data as `{status: "ok", result: ...}`. Query parameters are
model payload fields. Template listing accepts limit 1–200 and a nonnegative
offset. Dispatch tuples are JSON arrays, not Erlang terms.

In the attached browser, Cotonic calls the same model paths:

```javascript
const response = await cotonic.broker.call(
    "bridge/origin/model/development_tools/get/template/lookup",
    {name: "page.tpl", id: "article_resource_name"}
);
const result = response.payload.result;
```

For bridge implementation details, the capture model additionally offers
`POST attach {path}` (creates a stopped capture) and `POST poll/ID {entries: []}`.
Poll binds to the attaching browser session; an unrelated API token cannot
impersonate that session. The browser `attach()` function and floating debug button handle this handshake.

## Validate changes

Use inspection to choose the relevant source, then run the project's normal
build/tests. For Erlang compilation in this checkout use `./rebar3 compile`;
for frontend assets follow the site's Makefile. Re-run the page interaction and
compare trace/console evidence. Successful compilation is not proof that lazy
components or anonymous views work.

Deployment changes the running site. Use it when deployment is part of the
user's request. The existing `bin/zotonic deploy [git-working-tree]` performs a
fast-forward pull, build and activation under the deployment barrier and restores
`rebar.lock`; it is not a read-only validation command or a generic command for
publishing uncommitted edits. Confirm behavior in the installed command's source.
Do not assume editing a watched live checkout waits for an explicit deploy.

Read API details in `apps/zotonic_mod_development/src/models/m_development_tools.erl`
and `m_development_capture.erl` in the checkout. For deeper local inspection,
`bin/zotonic shell` attaches to the running node; obtain a site context with
`z:c(mysite)` and exit with Ctrl-D. Never call `q().`, which stops that node.
