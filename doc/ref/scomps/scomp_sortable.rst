
.. include:: meta-sortable.rst

Mark an element as sortable.

Sortables are part of a sorter.  Sortables in a sorter can be reordered by drag & drop.

Example::

   {% sorter id="sorter" tag="mysorter" %}
   {% sortable id="sort1" tag=1 %}
   {% sortable id="sort2" tag=2 %}
   <ul id="sorter">
     <li id="sort1">Sortable 1</li>
     <li id="sort2">Sortable 2</li>
   </ul>

This creates a list where the order of the list items can be changed by dragging and dropping them.

When the order of the sortables is changed, an event is sent to the sorter’s page :term:`controller` or :term:`delegate`.  The event contains the ordered list of sortable tags.

The event handler function is called like::

   event({sort, Sortables, Sorter}, Context).

Where “Sortables” is the list of sortables and “Sorter” is the sorter.  Both are “#dragdrop” records::

   -record(dragdrop, {tag, delegate, id}).

Where “tag” is the tag of the sortable or sorter, “delegate” is the module that handles the event and “id” is the HTML id of the sortable or sorter.

.. note:: that when the tag is a string then the ``#dragdrop`` tag will be an atom.

The sortable can have the following arguments:

+---------------+-------------------------------------------------------+---------------------+
|Argument       |Description                                            |Example              |
+===============+=======================================================+=====================+
|id             |The HTML id of the sortable element.                   |id="mysortable1"     |
+---------------+-------------------------------------------------------+---------------------+
|tag            |Tag that identifies the sortable to the event handler. |tag="my_atom_value"  |
|               |Can be any value.  A string will be converted to an    |                     |
|               |atom.                                                  |                     |
+---------------+-------------------------------------------------------+---------------------+
|delegate       |The delegate of the sortable, currently unused will be |delegate="mymodule"  |
|               |passed in the sortables’s #dragdrop record.            |                     |
+---------------+-------------------------------------------------------+---------------------+
|class          |A CSS class that will be added to the sortable.  The   |class="dragitem"     |
|               |class "sortable" will always be added.                 |                     |
+---------------+-------------------------------------------------------+---------------------+

.. seealso:: the :ref:`scomp-sorter` tag.

