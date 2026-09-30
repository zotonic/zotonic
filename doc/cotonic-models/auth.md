---
module: mod_authentication
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - authentication
  - identity_and_accounts
---

# model/auth

Owns browser authentication state and communicates with `/zotonic-auth`.
It checks and refreshes authentication, manages login/logout and password
operations, and synchronizes with other tabs. Use the authentication worker
loaded by Zotonic's normal page setup.

## Commands

All topics below start with `model/auth/`. Unless stated otherwise, use
`publish` and observe the events below; commands do not return a direct reply.

| Topic suffix | Payload and behavior |
| --- | --- |
| `post/check` | Recheck authentication; payload is ignored. |
| `post/refresh` | Refresh authentication, passing the payload as refresh options. |
| `post/logon` | `username`, `password`, optional `passcode`, `rememberme`, `code-new`, and `test_passcode`. |
| `post/form/logon` | Cotonic form payload: credentials under `value`; `message.username` overrides `value.username` when non-empty. Also accepts `value.is_username_check`, `value.authuser`, and `value.onauth`. |
| `post/logoff` | Log out; payload is ignored. |
| `post/switch-user` | `{ user_id: ... }`; the server determines whether the switch is authorized. |
| `post/onetime-token` | `{ token: ..., url: ... }` for an authentication handoff. |
| `post/reset-code-check` | `username`, `secret`, and optional `passcode`. Supports a response topic and returns the server's reset-check response. |
| `post/reset` | `username`, `secret`, new `password`, optional `passcode`, `rememberme`, `onauth`, `code-new`, and `test_passcode`. |
| `post/change` | Current `password`, new `password_reset`, optional `passcode`, `onauth`, `code-new`, and `test_passcode`. |

```javascript
cotonic.broker.publish("model/auth/post/check", {});
```

Only the reset-code check explicitly supports a direct call:

```javascript
cotonic.broker.call("model/auth/post/reset-code-check", {
    username: "reader",
    secret: resetSecret
}).then(function(msg) {
    console.log("Reset check:", msg.payload);
});
```

A fetch failure for that operation replies with
`{ result: "error", error: "fetch" }`.

## Events

All event topics start with `model/auth/`.

| Topic suffix | Payload | Retained |
| --- | --- | --- |
| `event/auth` | Authentication state, including `status`, `is_authenticated`, `user_id`, `username`, and `preferences`. | Yes |
| `event/auth-user-id` | Current user ID when the worker enters its known-authentication state. | No |
| `event/auth-changing` | `{ onauth, auth }` during an authentication transition. | No |
| `event/auth-error` | `{ error, data }`. | No |
| `event/auth-change-result` | Server result of a password-change request. | No |
| `event/ui-status` | `{ classes, status: { auth: "user" or "anonymous" } }`. | No |
| `event/ping` | `"pong"` after subscriptions are installed. | Yes |

```javascript
cotonic.broker.subscribe("model/auth/event/auth", function(msg) {
    const auth = msg.payload;
    console.log("Authenticated:", auth.is_authenticated);
});
```

The worker listens to `model/ui/event/recent-activity` for keep-alive decisions,
`model/sessionStorage/event/auth-user-id` for user changes, and
`model/serviceWorker/event/broadcast/auth-sync` for cross-tab checks. Its
advertised dependencies are `cotonic#sessionStorage`, `cotonic#localStorage`, and
`cotonic#sessionId`.

Client state describes the interface's current view of authentication. Server
models and actions still enforce their own access control.

## Implementation

[JavaScript source](../../apps/zotonic_mod_authentication/priv/lib/js/zotonic.auth.worker.js).
