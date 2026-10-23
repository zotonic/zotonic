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

# model/password

Toggles password visibility in the browser. Implemented in the wires script,
loaded by the standard Zotonic JavaScript includes. It works with dynamically
rendered forms through Cotonic's delegated DOM events, without inline JavaScript
or rebinding click handlers after a template update.

## Template usage

With `mod_authentication` enabled, use its shared button template after a password
input, inside an `input-group` container. Pass the input's HTML ID as `target_id`:

```django
<div class="input-group">
    <input class="form-control" type="password" id="new-password" name="password"
           autocomplete="new-password" required>
    {% include "_password_visibility.tpl" target_id="new-password" %}
</div>
```

The partial provides an attached eye-icon button with a translated **Show password**
accessible label and tooltip, a generated ID,
`type="button"`, `aria-controls`, `aria-pressed`, and `data-onclick-topic`.
The eye is crossed out while the password is visible.
Each password field can have its own button. Include buttons only for fields
that the user can interact with, not hidden password fields in intermediate
login or two-factor steps.

For a custom button, use the same attributes and give both elements unique IDs:

```django
<input type="password" id="account-password" name="password"
       autocomplete="current-password">
<button type="button" id="account-password-toggle"
        aria-controls="account-password" aria-pressed="false"
        data-onclick-topic="model/password/post/toggle">
    {_ Show password _}
</button>
```

Keep the label stable: `aria-pressed="true"` indicates that showing the password
is enabled. Native buttons support mouse and keyboard activation without
submitting the form.

## post/toggle

Topic: `model/password/post/toggle`.

The payload must contain `message.id`, the ID of the **button**, not the input.
Cotonic's `data-onclick-topic` handling supplies this from the button attributes.
A direct publish from an already loaded script has the same shape:

```javascript
cotonic.broker.publish("model/password/post/toggle", {
    message: { id: "account-password-toggle" }
});
```

The handler requires a button with the matching `data-onclick-topic`. It reads
`aria-controls` and searches for the input inside the button's form, or the
document if the button has no form. Inputs must have type `password` or `text`.
Missing buttons, missing inputs, and disabled inputs are ignored.

The input type switches between `password` and `text`, and the button's
`aria-pressed` becomes `false` or `true` respectively. The existing input element,
value, name, and autocomplete attributes are preserved.

This is a local browser topic. The toggle handler does not read or publish the
password value, contact the server, return a reply, or publish a result event.
Do not include password values in toggle messages.

## Template updates

The model also subscribes to `model/ui/event/dom-updated/+key`. The event payload's
`id` identifies the updated DOM container. Its toggle buttons and associated
inputs are reset to `aria-pressed="false"` and `type="password"`, including when
Incremental DOM has reused an input that was previously revealed. The input value
is preserved. The container itself is also handled if it is a toggle button.

Visibility is not persisted across these updates. New templates should render
password inputs masked and their toggle buttons unpressed. Replacing a form does
not require registering new click handlers.

## Implementation

[JavaScript source](../../apps/zotonic_mod_wires/priv/lib/js/apps/zotonic-wired.js).

[Shared button template](../../apps/zotonic_mod_authentication/priv/templates/_password_visibility.tpl).
