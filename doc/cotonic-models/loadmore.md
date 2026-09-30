---
module: mod_base
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - render
  - user_interface_and_interaction
  - browser_navigation
---

# model/loadmore

Replaces a loader element with a server-rendered template. Include
`js/models/loadmore.js` from `mod_base` on pages that use this model.

## post/replace

Topic: `model/loadmore/post/replace`.

| Payload field | Meaning |
| --- | --- |
| `id` | ID of the element to replace. |
| `template` | Template path to render on the server. |
| `url` | Optional URL whose query parameters become render arguments. |
| `qargs` | Render arguments when no URL is supplied. |
| `replace_location` | Whether to silently replace the browser URL when `url` is supplied; defaults to true. |

Cotonic DOM event payloads can instead provide `message.id`,
`message["data-template"]`, `message["data-url"]`, and
`message["data-replace-location"]`. If neither URL nor `qargs` is provided, the
handler uses `message`, or otherwise the payload itself, as render arguments.
String values `""`, `"0"`, `"false"`, and `"no"` disable URL replacement.

```javascript
cotonic.broker.publish("model/loadmore/post/replace", {
    id: "next-page",
    template: "_results_next.tpl",
    qargs: { page: 2 },
    replace_location: false
});
```

The handler marks the target `loading` and `aria-busy="true"`, then requests
`bridge/origin/model/template/get/render/<template>` with response topic
`model/ui/replace/<id>`. The rendered template receives its arguments through
`q`, for example `q.page`. The template response replaces the loader element;
include another loader in the response when more pages remain.

Requests for missing or already-loading elements are ignored. The command does
not reply to its caller and has no model-specific completion event.

## Initial visibility loading

When the script starts, it observes existing elements with
`data-onvisible-topic="model/loadmore/post/replace"`. Each is triggered once when
it intersects the viewport, using its `id`, `data-template`, `data-url`, and
`data-replace-location` attributes. This initialization scan does not register
new loaders inserted later.

## Implementation

[JavaScript source](../../apps/zotonic_mod_base/priv/lib/js/models/loadmore.js).
