---
module: mod_wires
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - user_interface_and_interaction
  - publish_and_subscribe
---

# model/activity

Publishes browser activity observed by Zotonic's wires script. This is an
event-only model: it has no `get`, `post`, or `delete` operations and no
availability ping.

## event

Topic: `model/activity/event`.

The payload is `{ type: "scroll" }`, with `type` taken from one of these DOM
events: `scroll`, `mousemove`, `keyup`, or `touchstart`. There are no coordinates,
key values, or DOM targets in the payload.

```javascript
cotonic.broker.subscribe("model/activity/event", function(msg) {
    console.log("Activity:", msg.payload.type);
});
```

The listeners run on `window` with capture and passive options. Publication is
limited to one message per 100 ms time bucket, shared across all four event
types. Suppressed events are not queued for later delivery. Messages are not
retained, so a new subscriber only sees subsequent activity.

This topic is distinct from Cotonic's `model/ui/event/recent-activity`, which
carries an `is_active` state and is used by the authentication worker.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).
