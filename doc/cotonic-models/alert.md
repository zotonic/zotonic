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

# model/alert

Displays a Zotonic alert dialog through the wires script and `z_dialog_alert`.
The page must have the normal Zotonic dialog support loaded.

## post

Topic: `model/alert/post`.

The payload is an options object:

| Field | Meaning |
| --- | --- |
| `text` | Dialog body, inserted as HTML. |
| `title` | Dialog title, inserted as HTML; defaults to the translated “Alert”. |
| `ok` | Confirmation button label, inserted as HTML; defaults to the translated “OK”. |
| `width` | Optional dialog width. |

All three fields, `text`, `title`, and `ok`, are rendered as HTML without
escaping or sanitization by this handler. HTML-escape untrusted plain text before
supplying it. If markup is intended, sanitize it before sending the command.

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
