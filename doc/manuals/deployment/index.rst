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

This manual describes the various ways how Zotonic can be run and how
it works in combination with widely used HTTP frontends like Varnish
and nginx.

Table of contents
-----------------

.. toctree::
   :maxdepth: 1
              
   startup
   privilegedports
   varnish
   nginx


