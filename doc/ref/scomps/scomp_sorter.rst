
.. include:: meta-sorter.rst

A sorter is a container for sortables.

A sorter contains sortables and handles the events when the order of the sortables is changed.  Sortables in a sorter can be reordered by drag & drop.

Example::

   {% sorter id="sorter" tag="mysorter" %}
   {% sortable id="sort1" tag=1 %}
   {% sortable id="sort2" tag=2 %}
   <ul id="sorter">
     <li id="sort1">Sortable 1</li>
     <li id="sort2">Sortable 2</li>
   </ul>

This creates a list where the order of the list items can be changed by dragging and dropping them.

When the order of the sortables is changed, an event is send to the sorter’s page :term:`controller` or :term:`delegate`.  The event contains the ordered list of sortable tags.

The event handler function is called like::

   event({sort, Sortables, Sorter}, Context).

Where “Sortables” is the list of sortables and “Sorter” is the sorter.  Both are ``#dragdrop`` records::

   -record(dragdrop, {tag, delegate, id}).

Where “tag” is the tag of the sortable or sorter, “delegate” is the module that handles the event and “id” is the HTML id of the sortable or sorter.

.. note:: that when the tag is a string then the ``#dragdrop`` tag will be an atom.

The sorter can have the following arguments:

+---------------+----------------------------------------------------------------------+---------------------------+
|Argument       |Description                                                           |Example                    |
+===============+======================================================================+===========================+
|id             |The HTML id of the sortable element.                                  |id="mysorter"              |
+---------------+----------------------------------------------------------------------+---------------------------+
|tag            |Tag that identifies the sortable to the event handler.  Can be any    |tag="my_atom_value"        |
|               |value.  A string will be converted to an atom.                        |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|delegate       |The delegate of the sorter. The sort event will be send to the        |delegate="mymodule"        |
|               |delegate module. Defaults to the resource that handled the page       |                           |
|               |request.                                                              |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|class          |A CSS class that will be added to the sorter.  The class "sorter" will|class="bunny-sorter"       |
|               |always be added.                                                      |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|handle         |jQuery selector for the handles of the sortables. When not defined    |handle=".sortable-handle"  |
|               |then the whole sortable can be clicked on for dragging.               |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|connect_group  |Name of the group this sorter connects with. Sortables from this      |connect_group="bunnies"    |
|               |sorter can then be dragged to sorters with that group name. This      |                           |
|               |argument can be repeated to connect with multiple groups. Special     |                           |
|               |values are "all" and "none" to either connect to all other sorters or |                           |
|               |to no other sorter.                                                   |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|group          |The group of this sorter. Used in connection with the "connect_group" |group="cows"               |
|               |argument. Sortables can be dragged between sorters of the same group. |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|axis           |If defined the items can only be dragged horizontally or              |axis="y"                   |
|               |vertically. Possible values are "x" and "y".                          |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|containment    |Constrains dragging of the sortables to within the bounds of the      |containment="parent"       |
|               |specified element. Possible values are "parent", "document", "window",|                           |
|               |or a jQuery selector.                                                 |                           |
+---------------+----------------------------------------------------------------------+---------------------------+
|opacity        |Opacity a sortable is set to when being dragged. Defaults to "1.0".   |opacity="0.8"              |
+---------------+----------------------------------------------------------------------+---------------------------+
|placeholder    |Class that gets applied to the otherwise white space that will show   |class="drophere"           |
|               |between sortables as the new place of the sortable.                   |                           |
+---------------+----------------------------------------------------------------------+---------------------------+

.. seealso:: the :ref:`scomp-sortable` tag.

