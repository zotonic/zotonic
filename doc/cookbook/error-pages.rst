.. highlight:: django

.. _cookbook-error-pages:

How to customise error pages
============================

Specific error pages
--------------------

Zotonic’s :ref:`controller-http_error` first tries to find an error page
template that is specific for the HTTP status code, named
:file:`templates/error.{status_code}.tpl`. So, to override the 404 template,
create a file :file:`templates/error.404.tpl`.

Generic error page
------------------

If no specific error page template exists, Zotonic falls back to the generic
:file:`templates/error.tpl`.
