.. highlight:: django
.. include:: meta-is_site_url.rst

.. seealso:: :ref:`filter-sanitize_url`, :ref:`filter-url_abs`, :ref:`filter-url`, :ref:`filter-urlencode`

Test if the given URL is a url for the current site.

If the current site handles requests for the hostname ``example.com``, then all of the
following expressions will echo ``true``::

    {{ "https://example.com"|is_site_url }}
    {{ "#foo"|is_site_url }}
    {{ "/page/path"|is_site_url }}
    {{ "//example.com"|is_site_url }}

The following will echo ``false``::

    {{ "https://foo.test"|is_site_url }}
    {{ "example.com"|is_site_url }}
    {{ "//foo.test"|is_site_url }}

