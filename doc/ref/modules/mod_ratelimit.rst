
.. include:: meta-mod_ratelimit.rst

Implements rate limiting for various resources.

After activation rate limiting will be added to the login and password reset flow.

The rate limiting is done on username or e-mail address. After five attempts the username (or e-mail address) will be blocked for an hour.

If an username is used for a successful login, then a special *device-id* cookie is placed on the user-agent.
This device-id ensures that that particular user-agent is allowed its own five tries. This prevents an username to be blocked
on known devices by tries on other devices. The device-id cookie is only valid for a single username.
