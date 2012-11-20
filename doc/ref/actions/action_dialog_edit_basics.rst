
.. include:: meta-dialog_edit_basics.rst

Open a dialog to edit the "basic" information of a :term:`resource`.

The basic information usually comprises of the title, the summary and
the category, but what exactly is displayed as "basic" info is
dependent on the :term:`category` of the resource and can be changed
per category by making a category specific template named
``_admin_edit_basics_form.tpl`` which is included using a :ref:`tag-catinclude`.

For instance, to create a special "basics" dialog for the category
`news`, you would create a template called
``_admin_edit_basics_form.news.tpl``

.. todo:: Extend documentation
