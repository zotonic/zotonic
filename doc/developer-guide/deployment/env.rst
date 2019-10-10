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

``ZOTONIC_CONFIG_DIR``
  The directory with the configuration files. For Zotonic major version 1, this defaults to ``~/.zotonic/1/``.
  Default locations (assuming the version of zotonic is 1.0 and the node is called ``zotonic@foobar``) are:

   * ``~/.zotonic/zotonic@foobar/``
   * ``~/.zotonic/1.0/``
   * ``~/.zotonic/1/``
   * ``~/.zotonic/``
   * ``/etc/zotonic/zotonic@foobar/``
   * ``/etc/zotonic/1.0/``
   * ``/etc/zotonic/1/``
   * ``/etc/zotonic/``

``SNAME``
  The *short name* of the Zotonic Erlang node. This defaults to ``zotonic``. If a
  short name is defined then the Erlang node is started with ``-sname``. The name can
  be like ``zotonic@foobar``, but the part after the ``@`` may not have a
  dot (``.``), as then it is a long name.

``LNAME``
  The *long name* of the Zotonic Erlang node. This defaults to ``zotonic``. If a
  long name is defined then the Erlang node is started with ``-name``. The name can
  have the domain defined, for example: ``zotonic@foo.bar.com``. The part after the
  ``@`` **must** be a fully qualified domain name. Zotonic will use the OS's domain name
  if no domain is defined in the LNAME.

``TMP``
  Where Zotonic puts temporary files. Examples are temporary files for image
  resizing or URL downloading.
