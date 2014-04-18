
.. include:: meta-mod_export.rst

Provides a generic framework to export :term:`resources <resource>`.

Currently, when installed, the module exposes a url
``/export/csv/:id`` which exports the resource with the given ID to a
CSV (Comma Separated Value) format. Only the following resource fields
are currently exported: title, summary, created, modified,
page_url_abs.

Currently, CSV is the only supported export format, but the module is
set up in such a way that other export formats are easy to add.
