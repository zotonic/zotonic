
.. include:: meta-mod_export.rst

Provides a generic framework to export :term:`resources <resource>`.

Admin interface
---------------

.. image:: /img/admin_view.png
    :scale: 50%
    :align: right

When :ref:`enabled <activating-modules>`, this module adds two things to each
admin edit page:

- extra content types to the ‘View’ dropdown menu
- an ‘Export’ block.

Both single pages and :ref:`query resources <guide-query-resources>` can be
exported. For a query, all resources matching it will be included in the export.

Customizing exports
-------------------

To customize data selection and the properties that are exported, observe one or
several of the :ref:`export notifications <export-notifications>`.
