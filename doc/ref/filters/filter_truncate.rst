.. highlight:: django
.. include:: meta-truncate.rst

Truncate a text to a maximum length.

The text gets truncated to the maximum length specified with the
argument. The text will alway truncated at a word boundary. When the
truncation is not after punctuation then the unicode "…" character is
appended.

For example::

  {{ value|truncate:8 }}

When value is “hello world.” then the output is “hello…”.

Entities like "&amp;" are recognized as a single character.

This filter is multibyte aware: Multi-byte UTF-8 characters are
assumed to be non-breaking characters.

