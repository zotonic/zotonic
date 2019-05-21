
.. include:: meta-log_format_stack.rst

Formats a Javascript stack trace string for readability.

Used in the UI log of the admin (``/admin/log/ui``).

The string:

.. code-block:: none

    submitFunction@http://t.lamma/lib/js/apps/zotonic-1.0~176359536.js:1704:29\n
    http://t.lamma/lib/js/apps/zotonic-1.0~176359536.js:1796:67\n
    each@http://t.lamma/lib/js/apps/jquery-latest.min~157550645.js:2:2577

Gets formatted as:

.. code-block:: html

    <b>submitFunction</b> <small class='text-muted'>http://t.lamma/lib/js/apps/zotonic-1.0~176359536.js:1704:29</small><br>
    <small class='text-muted'>http://t.lamma/lib/js/apps/zotonic-1.0~176359536.js:1796:67</small><br>
    <b>each</b> <small class='text-muted'>http://t.lamma/lib/js/apps/jquery-latest.min~157550645.js:2:2577</small>

See :ref:`mod_logging`

