
.. include:: meta-unlink.rst

Remove an :term:`edge` between two :term:`resources <resource>`. Used in the admin.

The edge is either selected with the `edge_id` argument, or with the combination `subject_id` + `predicate` + `object_id`.

Other arguments:

* hide - selector to fade out after unlink
* action - actions executed after unlink

* undo_action - passed on to the undo action template
* edge_template - passed on to the undo action template
* undo_message_id - defaults to `unlink-undo-message`

After update, an undo message is rendered in the `undo_message_id`
target, with the template ``_action_unlink_undo.tpl``.

  
.. seealso:: :ref:`action-link`
             
.. todo:: Extend documentation
