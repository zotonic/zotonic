---
module: mod_wires
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - user_interface_and_interaction
---

# model/clipboard

Copies text using the browser Clipboard API. Implemented in the wires script.
Browser clipboard availability and permission rules still apply.

## post/copy

Topic: `model/clipboard/post/copy`.

Supply `{ text: "Text to copy" }` directly, or let a Cotonic DOM event provide
`payload.message["data-text"]`. The `data-text` value takes precedence over
`payload.text` when it is defined.

```javascript
cotonic.broker.publish("model/clipboard/post/copy", { text: "Example" });
```

A button can publish the command directly:

```html
<button type="button"
        data-onclick-topic="model/clipboard/post/copy"
        data-text="Example">Copy</button>
```

The handler does not return a reply or publish a success or failure event. A
published command is therefore not confirmation that the copy succeeded. The
direct `text` branch ignores empty strings.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).
