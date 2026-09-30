---
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - messaging_and_pubsub
---

# Zotonic Cotonic models

These browser-side models extend Cotonic with Zotonic functionality. Each model
has its own reference page:

| Model | Purpose |
| --- | --- |
| [model/auth](auth.md) | Authentication state and operations. |
| [model/auth-ui](auth-ui.md) | Login and account recovery interfaces. |
| [model/oauth](oauth.md) | OAuth authorization and redirect flows. |
| [model/fileuploader](fileuploader.md) | Chunked uploads and upload queues. |
| [model/loadmore](loadmore.md) | Replace a loader with server-rendered content. |
| [model/alert](alert.md) | Alert dialogs. |
| [model/clipboard](clipboard.md) | Copy text to the clipboard. |
| [model/wires](wires.md) | Trigger named Zotonic events. |
| [model/console](console.md) | Browser console logging and ping replies. |
| [model/activity](activity.md) | Browser activity events; no request topics. |

## Calling conventions

Use `cotonic.broker.publish(topic, payload)` for commands and
`cotonic.broker.subscribe(topic, callback)` for events. The callback receives a
message whose `payload` contains the data. Within a worker, use `self.publish`
and `self.subscribe`.

Use `cotonic.broker.call` or `self.call` only where the operation documents a
response. Many commands publish events or update the interface without replying
to the request's response topic.

Client topics start with `model/`. A topic such as
`bridge/origin/model/fileuploader/post/new` addresses a server-side Zotonic model;
it is a different API from `model/fileuploader/post/new` in the browser.

Availability depends on the scripts and workers loaded by the page. Cotonic's
own models, including `cotonic#sessionId`, `cotonic#ui`, and `cotonic#location`, are
documented in the Cotonic reference maintained in its repository.
