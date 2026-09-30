---
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - user_interface_and_interaction
---

# model/alert

Displays a Zotonic alert dialog through the wires script and `z_dialog_alert`.
The page must have the normal Zotonic dialog support loaded.

## post

Topic: `model/alert/post`.

The payload is an options object:

| Field | Meaning |
| --- | --- |
| `text` | Dialog body, inserted as HTML. Escape untrusted text before supplying it. |
| `title` | Dialog title; defaults to the translated “Alert”. |
| `ok` | Confirmation button label; defaults to the translated “OK”. |
| `width` | Optional dialog width. |

```javascript
cotonic.broker.publish("model/alert/post", {
    title: "Upload complete",
    text: "The document is ready.",
    ok: "Close"
});
```

The command does not reply and does not publish a model event. Use `publish`,
not `call`. JavaScript function callbacks cannot be sent through worker messages.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).
