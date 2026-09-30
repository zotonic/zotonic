---
module: mod_wires
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - wire_action
  - user_interface_and_interaction
---

# model/wires

Connects Cotonic messages to named Zotonic events through the wires script.

## post/event/+name

Topic: `model/wires/post/event/+name`.

The final topic segment names an event registered on the page. The handler calls
`z_event(name, msg.payload)`, passing the payload as the event's parameters.

For a named event created by a template wire:

```django
{% wire name="refresh-preview" action={reload} %}
```

Publish its name from JavaScript:

```javascript
cotonic.broker.publish("model/wires/post/event/refresh-preview", {});
```

The resulting behavior depends on the named wire: it may execute browser actions
or send a postback to the server. The client command itself has no response and
publishes no `model/wires/event` message. Server-side authorization remains the
responsibility of the receiving action or delegate.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).
