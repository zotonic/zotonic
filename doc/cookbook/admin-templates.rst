Admin template specific things
==============================

Common markup in admin templates.

Linking to edit pages
---------------------

Dispatches to edit pages is done by ``admin_edit_rsc``.

Linking:

.. code-block:: django

    {% url admin_edit_rsc id=my_id %}

Redirecting:

.. code-block:: django

    {% button text="edit" action={redirect dispatch="admin_edit_rsc" id=my_id} %}

