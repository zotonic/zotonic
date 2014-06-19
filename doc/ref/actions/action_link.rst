
.. include:: meta-link.rst

Add an :term:`edge` between two :term:`resources <resource>`. Used in the admin.

The edge is selected with either:

* the argument ``edge_id``
* the arguments ``subject_id``, ``predicate``, ``object_id``

For instance::

    {% button
        text="Add"
        class="btn"
        action={
            link
            subject_id=id
            predicate="contains"
            object_id=other_id
            action={
                reload
            }
        }
    %}

Other arguments:

* element_id
* edge_template
* action - actions executed after linking

.. seealso:: :ref:`action-unlink`

.. todo:: Extend documentation
