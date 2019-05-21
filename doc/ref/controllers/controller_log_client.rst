
.. include:: meta-log_client.rst

Controller for logging User Interface errors.

Receives an JSON like:

.. code-block:: json

    {
        "type": "error",
        "message": "The error message",
        "file": "http://example.com/lib/file.js",
        "line": 123,
        "col": 12,
        "stack": "foo@http://example.com/lib/file.js:123:12\nbar@...",
        "user_agent": ""Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit...",
        "url": "http://example.com/en/page/271828"
    }

The type must be one of: ``"error"``, ``"warning"``, ``"info"``, or ``"debug"``.

The JSON payload is logged in the info log and the database UI log.
Only the fields above are logged in the database, other fields are dropped.

All database log entries are rate limited to a maximum of 1 per second. The log can be viewed at ``/admin/log/ui``.

See :ref:`mod_logging
