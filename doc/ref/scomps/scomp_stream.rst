.. include:: meta-stream.rst

stream
======

Enable WebSockets or Comet communication.

This scomp starts the WebSockets or Comet communication.

When available then a WebSockets connections is opened, otherwise a long polling Comet connection is started.  The WebSockets connection will also be used for sending Ajax requests to the server.

Comet connections are not enabled by default to prevent problems with certain browsers that support only a limited number of open connections.

Example::

  {% stream %}

This will add javascript to the output of the :ref:`scomp-script`. The added javascript starts the connection.

