
.. include:: meta-admin.rst

This model exposes some meta-information for the use in ``mod_admin`` templates.

There are two properties available:

 1. Pivot queue count

    Retrieve the count of the pivot
    queue, the queue which decides which resources need re-indexing.

    To view the number of items in the pivot queue, do the following::

        {% print m.admin.pivot_queue_count %}

 2. Default ``published`` setting for the new-resource dialog

        {% if m.admin.rsc_dialog_is_published %} ... {% endif %}

