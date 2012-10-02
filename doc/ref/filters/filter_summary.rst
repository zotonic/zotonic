.. highlight:: django
.. include:: meta-summary.rst

Extract a summary from a resource.

The value of the filter is the resource's id.

This uses the `summary` field if available, but if it is empty, uses
the first paragraph of the body text to show a summary.

Useful for displaying excerpts of texts that do not always have a
separate summary provided.

Example::

  {{ id|summary }}

