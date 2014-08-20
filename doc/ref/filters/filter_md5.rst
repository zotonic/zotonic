.. highlight:: django
.. include:: meta-md5.rst

Translates a string to a `md5 <http://en.wikipedia.org/wiki/MD5>`_ hex value.

For example::

  {{ "The quick brown fox jumps over the lazy dog"|md5 }}

Creates::

  9E107D9D372BB6826BD81D3542A419D6

Note that MD5 is not considered to be a very safe encryption algorithm.

