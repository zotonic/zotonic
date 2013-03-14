
.. include:: meta-stats.rst

Access the statistics from templates using the `stats` model.

The `metrics` property holds a list of all available statistics.

To get the information about a metric there are keys for `system`, 
`name`, `type` and `value`. Example, listing all raw data::

   {% for metric in m.stats.metrics %}
      {{ metric.system }}.{{ metric.name }} ({{ metric.type }}): {{ metric.value|pprint }}<br />
   {% endfor %}

To get at a specific statistic, you need to provide system and name for it.

This example will retrieve the statistics for the db requests metric::

   {{ m.stats.db.requests.value }}


.. note:: This model is experimental and may change without notice.
