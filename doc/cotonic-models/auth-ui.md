---
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - authentication
  - identity_and_accounts
  - user_interface_and_interaction
---

# model/auth-ui

Manages Zotonic's login, reminder, verification, reset, and password-change
views. The standard `_logon.tpl` starts the worker and supplies the
`signup_logon_box` UI target. The worker renders `_logon_box.tpl` through the
server template model and reacts to [authentication events](auth.md).

## Commands

All topics below start with `model/auth-ui/`. Commands update the view or start
an asynchronous flow; they do not reply to a response topic.

| Topic suffix | Payload |
| --- | --- |
| `post/view/+view` | Select the view named by the final topic segment, such as `logon` or `reminder`. Payload is ignored. |
| `post/form/reminder` | Form payload with `value.email`. |
| `post/form/send_verification_message` | Form payload with `value.token`. |
| `post/form/reset` | Form payload with `value.password_reset1`, `value.password_reset2`, and optional `value.passcode`, `value.rememberme`, `value["code-new"]`, and `value.test_passcode`. |
| `post/form/change` | Form payload with `value.password`, `value.password_reset1`, `value.password_reset2`, and optional passcode fields. The current handler reads `code-new` from the outer payload. |
| `post/confirm-authuser` | Object with `username` and `authuser` to enter account confirmation. |

```javascript
cotonic.broker.publish("model/auth-ui/post/view/reminder", {});
```

For example, a reminder request can be sent with:

```javascript
cotonic.broker.publish("model/auth-ui/post/form/reminder", {
    value: { email: "reader@example.com" }
});
```

## State and results

The initial view comes from the location query's `logon_view`, defaulting to
`logon`. Query arguments `secret`, `u`, and `email` supply reset and login context.
The worker checks repeated passwords before submitting reset or change commands
to `model/auth`.

Authentication errors, user changes, and password-change results are consumed
from `model/auth/event/auth-error`, `model/auth/event/auth-user-id`, and
`model/auth/event/auth-change-result`. Results appear in the rendered login
box; there is no separate `model/auth-ui/event/...` stream.

## Implementation

[JavaScript source](../../apps/zotonic_mod_authentication/priv/lib/js/zotonic.auth-ui.worker.js).
