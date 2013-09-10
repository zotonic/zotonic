.. highlight:: django
.. include:: meta-tokens.rst

Returns a list of tokens from input string, separated by the characters in the filter argument.

This is filter maps directly onto the ``string:tokens/2`` function in ``stdlib`` from the Erlang/OTP distribution.

Example::

  {{ "abc defxxghix jkl"|tokens:"x " }}

Will give the list of tokens: ``["abc", "def", "ghi", "jkl"]``.
