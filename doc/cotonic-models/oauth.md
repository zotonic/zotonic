---
module: mod_oauth2
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - oauth_2_0
  - authentication
  - browser_storage
---

# model/oauth

Coordinates an OAuth authorization or redirect page. The worker is provided by
`mod_oauth2` and advertises `model/oauth` as a dependency capability. It has no
public `model/oauth/get/...` or `model/oauth/post/...` subscriptions.

## Initialization

The standard `logon_service_oauth.tpl` and `logon_service_oauth_done.tpl` templates
start `js/zotonic.oauth.worker.js` with server-generated `worker_args`.

| Argument | Meaning |
| --- | --- |
| `oauth_step` | `"authorize"` or `"redirect"`. |
| `authorize_url` | Provider authorization URL for the authorize step. |
| `oauth_state` | State data stored for the authorize step. |
| `oauth_state_id` | Identifier associated with that state. |

Use the standard OAuth service login flow to obtain these arguments. The worker
depends on `bridge/origin`, `model/auth`, `model/sessionId`, `model/location`,
`model/sessionStorage`, and `model/localStorage`.

## Flow and messages

The worker waits for a stable `model/auth/event/auth` state. During authorization
it stores temporary `oauth-data` in localStorage and redirects to the provider.
During the redirect step it coordinates a page reload, reads the callback query
parameters, and calls
`bridge/origin/model/oauth2_service/post/oauth-redirect`.

Depending on the server response, it can submit an onetime token to
`model/auth/post/onetime-token`, request account confirmation through
`bridge/opener/model/auth-ui/post/confirm-authuser`, redirect the browser, close
the window, or render an error view. It also listens to
`model/auth/event/auth-user-id` and `model/auth/event/service-confirm`.

This worker does not expose a general request/reply API or publish its own
`model/oauth/event/...` status stream. Its UI and authentication effects are
handled through the other models.

## Implementation

[JavaScript source](../../apps/zotonic_mod_oauth2/priv/lib/js/zotonic.oauth.worker.js).
