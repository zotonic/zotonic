
.. include:: meta-draggable.rst

Mark a html element as draggable.

The draggable tag is used in conjunction with the :ref:`{% droppable %} <scomp-droppable>` tag to implement drag & drop.  Elements that are marked as draggable can be dropped on elements marked as droppable.  Drag & drop generates dragdrop events that are sent to the :term:`controller` or the :term:`delegate`.

For example::

   <div id="drag1">Drag me</div>
   {% draggable id="drag1" tag="drag-one" %}

Now the div with id “drag1” can be dragged.  When it is dropped then ``event/2`` of the controller or delegate Erlang module is called signaling the drag (and also the drop to the module receiving the droppable events)::

   event({drag, Drag, Drop}, Context).

Where both Drag and Drop are ``#dragdrop`` records::

   -record(dragdrop, {tag, delegate, id}).

The draggable tag accepts the following arguments:

+----------------+-----------------------------------------------------+---------------------------+
|Argument        |Description                                          |Example                    |
+================+=====================================================+===========================+
|id              |The id of the element that becomes draggable.        |id="drag1"                 |
+----------------+-----------------------------------------------------+---------------------------+
|tag             |The tag of the draggable that is sent as part of the |tag={subject_list id=42    |
|                |drag and drop events. This can be any value,         |changed=false}             |
|                |including a tuple.                                   |                           |
+----------------+-----------------------------------------------------+---------------------------+
|clone           |Clone the element when dragging or drag the element  |clone=true                 |
|                |itself.  Defaults to false.                          |                           |
+----------------+-----------------------------------------------------+---------------------------+
|revert          |When the element has to revert to its starting       |revert=false               |
|                |position. Defaults to "invalid", i.e. when the drop  |                           |
|                |was above an invalid position.  Other options are    |                           |
|                |true, false and "valid".                             |                           |
+----------------+-----------------------------------------------------+---------------------------+
|axis            |Constrain the drag movement to either the x or y     |                           |
|                |direction.  Normally the drag is not                 |                           |
|                |constrained. Acceptable values are "x" or "y"        |                           |
|                |axis="x"                                             |                           |
+----------------+-----------------------------------------------------+---------------------------+
|handle          |The css selector that is the handle to drag with.    |handle="handleclass"       |
|                |Defaults to the whole element.                       |                           |
+----------------+-----------------------------------------------------+---------------------------+
|group           |The name of this drag group, for use in the droppable|group="edges"              |
|                |element's "accept" argument. Multiple groups are     |                           |
|                |allowed.                                             |                           |
+----------------+-----------------------------------------------------+---------------------------+
|opacity         |Change the opacity while dragging. Defaults to "0.8".|opacity="0.5"              |
+----------------+-----------------------------------------------------+---------------------------+
|delegate        |The Erlang module that will receive the drag event   |                           |
|                |after a successful drop.                             |                           |
+----------------+-----------------------------------------------------+---------------------------+

.. seealso:: the :ref:`scomp-droppable` tag.

