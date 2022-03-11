.. include:: meta-security_headers.rst

This is called when the *security headers* are set for the request, which is done at
the start of the request handling, before any controller callback is called.

That is done so early to ensure that any returned payload has all required security headers.

The default security header list is:

.. code-block:: erlang

    [
        % Content-Security-Policy is not added by default
        % {<<"content-security-policy">>, <<"script-src 'self' 'nonce-'">>}
        {<<"x-xss-protection">>, <<"1">>},
        {<<"x-content-type-options">>, <<"nosniff">>},
        {<<"x-permitted-cross-domain-policies">>, <<"none">>},
        {<<"referrer-policy">>, <<"origin-when-cross-origin">>},
        {<<"x-frame-options">>, <<"sameorigin">>}
    ]

If the controller option ``allow_frame`` is set to `true` then the ``x-frame-options`` header
is not added.

The ``security_headers`` notification does a *first* to fetch the security headers. The default
headers are passed in the ``headers`` field of the notification.

If the notification returns a list with ``<<"content-security-policy">>`` then in the value of
the Content-Security-Policy header the string ``'nonce-'`` is replaced with the unique nonce for the
request.

The nonce can de added to script tags:

.. code-block:: django

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
        // Inline javascript here
    </script>

It can be requested in Erlang code using:

.. code-block:: erlang

    CSPNonce = z_context:csp_nonce(Context).

Or via the ``m_req`` model:

.. code-block:: erlang

    CSPNonce = m_req:get(csp_nonce, Context).

Note that the nonce is only set iff the Context is a HTTP request context. It is *not* set for
MQTT contexts.
