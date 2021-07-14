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
  The directory with the configuration files. Possible locations are:

   * The init argument ``zotonic_config_dir``
   * The environment variable ``ZOTONIC_CONFIG_DIR``
   * The directory :file:`$HOME/.zotonic`
   * The directory :file:`/etc/zotonic` (only on Unix)
   * The OS specific directory for application config files

  The OS specific directories are:

   * On Unix: :file:`~/.config/zotonic/config/`
   * On macOS: :file:`~/Library/Application Support/zotonic/config/`

  In those directories the system searches for a ``zotonic*`` file in the following subdirectories (assuming the version of Zotonic is 1.2.3 and the node is called ``zotonic001@foobar``):

   * ``zotonic001@foobar/``
   * ``zotonic001/``
   * ``1.2.3``
   * ``1.2``
   * ``1``
   * ``.``

  The default is the OS specific directory, with as subdirectory the major version number of Zotonic (in this case ``1``).
  For Linux this would be :file:`~/.config/zotonic/config/1/`

``ZOTONIC_SECURITY_DIR``
  The directory to store the certificates and other security related data.
  Possible locations are:

   * The environment variable ``ZOTONIC_SECURITY_DIR``
   * The :file:`~/.zotonic/security` directory
   * The :file:`/etc/zotonic/security` directory (only on Linux)
   * The OS specific directory for application data files

  The OS specific directories are:

   * On Unix: :file:`~/.config/zotonic/security/`
   * On macOS: :file:`~/Library/Application Support/zotonic/security/`

  The default is the OS specific directory.

``ZOTONIC_DATA_DIR``
  The directory to store the data files.
  Possible locations are:

   * The environment variable ``ZOTONIC_DATA_DIR``
   * The :file:`data` directory in the Zotonic directory
   * The OS specific directory for application data files

  The OS specific directories are:

   * On Unix: :file:`~/.local/share/zotonic`
   * On macOS: :file:`~/Library/Application Support/zotonic/`

  The default is the OS specific directory.

``ZOTONIC_LOG_DIR``
  The directory to store the log files.
  Possible locations are:

   * The environment variable ``ZOTONIC_LOG_DIR``
   * The :file:`logs` directory in the Zotonic directory
   * The OS specific directory for application log files

  The OS specific directories are:

   * On Unix: :file:`~/.cache/zotonic/log/`
   * On macOS: :file:`~/Library/Logs/zotonic/`

  The default is the OS specific directory.

``ZOTONIC_APPS``
  The directory used for sites, modules and additional OTP applications. This defaults to ``apps_user``
  in the Zotonic umbrella application.

  If a separate checkouts directory is used, then this environment variable must be:

   * Defined when building Zotonic with ``make compile`` or ``./rebar3 compile``
   * Defined when starting Zotonic

``ZOTONIC_PIDFILE``
  Path to zotonic PID file. If set, Zotonic will create it at start and remove it before exit.
  Note however that Erlang VM is not stopped at this moment and can last for several seconds.
  See ``ZOTONIC_WAIT_VM`` to get rid of this.

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

The following environment variables influence how Zotonic stops.

``ZOTONIC_WAIT_VM``
  If set to 1, ask launcher script to wait for total stop of Zotonic Erlang VM before exit.
  This can be used to ensure all resources are freed before trying a new start.
