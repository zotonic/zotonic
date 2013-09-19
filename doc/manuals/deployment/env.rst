.. _manual-deployment-env:

Useful environment variables
============================

The following environment variables influence how Zotonic starts up.


``ZOTONIC_IP``
  Which IP address to bind the web server to. By default, it binds to
  any IP address. When running Zotonic behind a proxy like nginx or
  varnish, it is wise to put ``127.0.0.1`` here.

``ZOTONIC_PORT``
  The port number to bind the web server to. Defaults to port 8000.


``ZOTONIC_SMTP_LISTEN_IP``
  The IP address to bind the SMTP server to. Binds to any IP address
  by default.

``ZOTONIC_SMTP_LISTEN_PORT``
  The port number to bind the SMTP server to. Defaults to port 2525.
  
``ZOTONIC_SMTP_LISTEN_DOMAIN``
  The domain to bind the SMTP server to, if any.

``TMP``
  Where Zotonic puts temporary files, like temporary files for image
  resizing or URL downloading.

  
.. note:: When the variables do not seem to have effect, check
          Zotonic’s ``priv/config`` file. The above variables are
          overridden by the ones in that file, at the moment.
