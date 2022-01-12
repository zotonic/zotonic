.. highlight:: django
.. include:: meta-utc.rst
.. seealso:: :ref:`filter-date`

Translates a datetime from local time to UTC.

For example::

  {{ id.modified|utc|date:"Ymd:His\\Z" }}

Displays the modification date and time of a resource in Universal Time.
