.. _manual-deployment:
==========
Deployment
==========

So you have built your Zotonic site, and now you want to show it to
the world. This page tells you how to configure your zotonic
environment so that it is ready for real-world website visitors.

As per the Installation Instructions, up until now you probably have
always started Zotonic using the ``zotonic debug`` command. This is
fine for debugging purposes, because it gives you an Erlang shell in
which you can view the output of the server, the ``?DEBUG`` messages that
are triggered, and try out Erlang expressions.

However for a production system, you don't need this shell, you want
Zotonic running in the background, and started at startup.

Creating an init script
=======================

The :ref:`Zotonic shell command <manual-cli>` can start Zotonic in the
background and stop it again.

.. highlight:: bash

An init script will just need to call the zotonic command with either
`start` or `stop`. On debian systems it might look like this::

  #!/bin/sh -e

  ### BEGIN INIT INFO
  # Provides:             zotonic
  # Required-Start:       $postgres $local_fs $remote_fs $network $time
  # Required-Stop:        $postgres $local_fs $remote_fs $network $time
  # Should-Start:         
  # Should-Stop:          
  # Default-Start:        2 3 4 5
  # Default-Stop:         0 1 6
  # Short-Description:    Zotonic
  ### END INIT INFO

  /usr/bin/sudo -u zotonic -i /home/zotonic/zotonic/bin/zotonic $@



Using Varnish as frontend for Zotonic
=====================================

Using the Varnish HTTP frontend, you can speed up your Zotonic even
more as this web server caches static files intelligently.

Excerpt from a ``config.vcl`` file for Varnish::

  #
  # Varnish configuration for a Zotonic site
  #
  
  backend zotonic {
    .host = "127.0.0.1";
    .port = "8000";
    .first_byte_timeout = 300s;
    .connect_timeout = 300s;
    .between_bytes_timeout = 300s;
  }

  sub vcl_recv {
    set req.http.X-Forwarded-Host = req.http.host;
    set req.backend   = zotonic;

    ######################################################################

    # Add a unique header containing the client address
    unset req.http.X-Forwarded-For;
    set   req.http.X-Forwarded-For = client.ip;

    # We only deal with GET and HEAD by default
    if (req.request != "GET" && req.request != "HEAD") {
      return (pass);
    }

    # Cache served css and media files
    if (req.url ~ "^/(lib|image|media|favicon.ico)/") {
      unset req.http.Cookie;
      unset req.http.Authenticate;
      set req.grace = 30m;
      return (lookup);
    }

    return (pass);
  }
