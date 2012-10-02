.. highlight:: django
.. include:: meta-sha1.rst

Translate a string to a sha1 hex value.

This filter creates a SHA-1 checksum of the input string. It is output
as a hex digest::

  {{ "Hello world"|sha1 }}

Outputs the value: "7B502C3A1F48C8609AE212CDFB639DEE39673F5E"

