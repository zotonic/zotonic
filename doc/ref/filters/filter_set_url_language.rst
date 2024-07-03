.. highlight:: django
.. include:: meta-set_url_language.rst

Change the language of an URL to another language. Useful to generate alternative URLs for a page.

Example, provide an URL to the German version of the current page::

    {{ m.req.raw_path|set_url_language:'de' }}

The URL is sanitized before the language is added. The returned URL is always an absolute URL with
the correct hostname, port and protocol.

Note that it is possible to get an alternative language version of a resource's page by providing
a ``z_language`` argument::

    {{ id.page_url_abs with z_language = 'de' }}
