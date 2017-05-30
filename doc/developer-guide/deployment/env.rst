.. _guide-deployment-env:

Useful environment variables
============================

The following environment variables influence how Zotonic starts up.


``ZOTONIC_IP``
  The IPv4 address to bind the web server to. By default, it binds to
  any IP address. If Zotonic runs behind a proxy like nginx or
  Varnish, it is wise to put ``127.0.0.1`` or ``localhost`` here.
  Use ``any`` to bind to all IPv4 addresses, ``none`` to disable the IPv4
  web server.

``ZOTONIC_IP6``
  The IPv6 address to bind the web server to. By default it binds to
  the ZOTONIC_IP address. If Zotonic runs behind a proxy like nginx or
  Varnish, it is wise to put `::1`` here.
  Use ``any`` to bind to all IPv6 addresses, ``none`` to disable the IPv6
  web server.

``ZOTONIC_PORT``
  Outside port that clients send HTTP requests to. Defaults to
  ``ZOTONIC_LISTEN_PORT``. See :ref:`ref-port-ssl-configuration`.

``ZOTONIC_SSL_PORT``
  Outside port that clients send HTTPS requests to. Defaults to
  ``ZOTONIC_SSL_LISTEN_PORT``.
  Use ``none`` to disable the ssl web server.
  See :ref:`ref-port-ssl-configuration`.

``ZOTONIC_LISTEN_PORT``
  Port on which Zotonic will listen for HTTP requests. Defaults to port 8000.
  See :ref:`ref-port-ssl-configuration`.

``ZOTONIC_SSL_LISTEN_PORT``
  Port on which Zotonic will listen for HTTPS requests.
  See :ref:`ref-port-ssl-configuration`.

``ZOTONIC_SMTP_LISTEN_IP``
  The IPv4 address to bind the SMTP server to. Binds to any IP address
  by default. Use ``none`` to disable the SMTP server.
  Use ``any`` to bind to all IP addresses.

``ZOTONIC_SMTP_LISTEN_PORT``
  The port number to bind the SMTP server to. Defaults to port 2525.
  Use ``none`` to disable the SMTP server.

``ZOTONIC_SMTP_LISTEN_DOMAIN``
  The domain to bind the SMTP server to, if any.

``TMP``
  Where Zotonic puts temporary files, like temporary files for image
  resizing or URL downloading.


.. note:: If the variables do not seem to have effect, then check
          ``~/.zotonic/zotonic.config``. The above variables are
          overridden by the ones in that file.
