
.. include:: meta-admin_rsc.rst
             
This model exposes some meta-information of the :ref:`model-rsc` model.

Currently the only thing it does is retrieve the count of the pivot
queue, the queue which decides which resources need re-indexing.

To view the number of items in the pivot queue, do the following::

  {% print m.admin_rsc.pivot_queue_count %}

