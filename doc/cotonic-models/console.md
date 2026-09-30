---
module: mod_wires
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - development_and_debugging
  - logging_and_monitoring
---

# model/console

Writes structured messages to the browser console and answers diagnostic pings.
Implemented in the wires script.

## post/log

Topic: `model/console/post/log`.

| Payload field | Meaning |
| --- | --- |
| `message` | Message string. |
| `severity` | Defaults to `info`; `critical`, `fatal`, and `error` are red, `warning` is orange, and `notice` is blue. |
| `fields` | Optional object of additional values to log. |
| `meta` | Optional metadata object; `file` and `line` provide a source location. |

```javascript
cotonic.broker.publish("model/console/post/log", {
    message: "Preview updated",
    severity: "info",
    fields: { resource_id: 123 }
});
```

This command does not reply. On pages whose HTML element has the
`environment-development` class, the same handler also subscribes to
`bridge/origin/model/log/event/console` for server log messages.

## post/ping

Topic: `model/console/post/ping`.

Replies with the string `"pong"` when the request supplies a response topic. The
reply uses the request's QoS.

```javascript
cotonic.broker.call("model/console/post/ping", {}).then(function(msg) {
    console.log(msg.payload); // "pong"
});
```

There is no retained `model/console/event/ping` announcement.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).
