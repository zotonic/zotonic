
.. include:: meta-dispatch.rst

Dispatch or generate URLs or page paths. Useful to check dispatch rules or for client side code
to dispatch page paths.

Dispatching URLs
----------------

The ``path`` returns the matched dispatch rule, controller, controller options,
language, bindings and other dispatch information.

If no dispatch rule matches then 404 Not Found is returned.

Example:

.. code-block:: bash

    curl -k 'https://example.test:8443/api/model/dispatch/get/path/nl/page/1234/hello'

Returns:

.. code-block:: json

    {
        "result": {
            "bindings": {
                "id": "1234",
                "z_language": "nl",
                "zotonic_site": "example",
                "zotonic_dispatch_path": [
                    "page",
                    "1234",
                    "hello"
                ],
                "zotonic_dispatch": "page",
                "slug": "hello"
            },
            "controller": "controller_page",
            "site": "example",
            "language": "nl",
            "dispatch_rule": "page",
            "path_tokens": [
                "page",
                "1234",
                "hello"
            ],
            "controller_options": {
                "template": {
                    "template": "page.tpl",
                    "is_catinclude": true
                },
                "zotonic_dispatch_file": "dispatch",
                "zotonic_dispatch_module": "mod_site_whatwebwhat"
            }
        },
        "status": "ok"
    }


Creating URLs
-------------

URLs or page paths can also be requested using the ``url_for`` method. Here the name
of the dispatch rule is padded, optionally with any extra arguments.

Example:

.. code-block:: bash

    curl -k 'https://example.test:8443/api/model/dispatch/get/url_for/admin'

Returns:

.. code-block:: json

    {"result":"/en/admin","status":"ok"}

Or, use ``abs_url_for`` if a complete URL with domain is needed:

.. code-block:: bash

    curl -k 'https://example.test:8443/api/model/dispatch/get/abs_url_for/admin'

Returns:

.. code-block:: json

    {"result":"https://example.com/en/admin","status":"ok"}


And for more complex URLs:

.. code-block:: bash

    curl -k 'https://example.test:8443/api/model/dispatch/get/url_for/page?id=1234&slug=foo&a=1&bar=baz'

Returns:

.. code-block:: json

    {"result":"/en/page/1234/foo?a=1&bar=baz","status":"ok"}

Here the selected dispatch rule was something like:

.. code-block:: erlang

    {page, [ "page", id, slug ], controller_page, [ {template, "page.tpl"} ]}

Because ``id`` and ``slug`` are part of the path, they are included in the generated URL path, the
other arguments are added as query parameters.
