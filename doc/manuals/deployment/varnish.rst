.. highlight:: none

.. _manual-deployment-varnish:

Using Varnish as frontend for Zotonic
=====================================

Using the `Varnish HTTP frontend <https://www.varnish-cache.org/>`_,
you can speed up your Zotonic even more as this web server caches
static files intelligently.

Your Varnish ``config.vcl`` needs to define a `backend` for Zotonic::

  backend zotonic {
    .host = "127.0.0.1";
    .port = "8000";
    .first_byte_timeout = 300s;
    .connect_timeout = 300s;
    .between_bytes_timeout = 300s;
  }

Then, in ``vcl_recv``, specify the Zotonic backend as the default backend::

  sub vcl_recv {
    set req.http.X-Forwarded-Host = req.http.host;
    set req.backend   = zotonic;

    ...


Full varnish example configuration file
---------------------------------------

Please see the `Varnish documentation
<https://www.varnish-cache.org/docs>`_ for more information.

.. literalinclude:: varnish.zotonic.vcl


Auto-starting Varnish on Mac OSX
--------------------------------

.. highlight:: xml
               
To automatically start Varnish on Max OSX, add the following ``plist`` file to launchd.

.. literalinclude:: varnish.launchd.plist
