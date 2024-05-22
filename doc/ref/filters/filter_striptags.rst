.. highlight:: django
.. include:: meta-striptags.rst

Removes all HTML tags from the value.

Useful as part of filtering input from external sources.

For example::

  {{ value|striptags }}

When value is “<b>Hello</b>world" then the output will be “Helloworld”.

Striptags optionally allows a maximum length of characters to be returned::

  {{ value|striptags:80 }}

This returns a string truncated to max 80 characters, including whitespaces etc.
