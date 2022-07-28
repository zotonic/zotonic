.. highlight:: django
.. include:: meta-normalize_email.rst

Normalize an email address, used in the identity management.

The email address is lowercased and trimmed.

For example::

  {{ "ME@Example.Com "|normalize_email }}

Evaluates to the value ``me@example.com``.
