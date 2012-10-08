
.. include:: meta-droppable.rst

Mark an element as valid drag destination.

The droppable tag is used in conjunction with the :ref:`{% draggable %} <scomp-draggable>` tag to implement drag & drop.  Elements that are marked as droppable can receive drops of draggable elements.  Drag & drop generates dragdrop events that are sent to the :term:`controller` or the :term:`delegate`.

For example::

   <div id="dropzone">Drop your stuff here</div>
   {% droppable id="dropzone" tag="drop-tag" %}

Now draggable elements can de dropped onto the div with id “dropzone”.  When a draggable is dropped then ``event/2`` of the controller or delegate Erlang module is called signaling the drop (and also the drag to the module receiving the draggable events)::

   event({drop, Drag, Drop}, Context).

Where both Drag and Drop are ``#dragdrop`` records::

   -record(dragdrop, {tag, delegate, id}).

The droppable tag accepts the following arguments:

+---------------+---------------------------------------------+------------------+
|Argument       |Description                                  |Example           |
+===============+=============================================+==================+
|id             |The id of the element that will accept drops |id="dropzone"     |
|               |of draggables.                               |                  |
+---------------+---------------------------------------------+------------------+
|tag            |The tag of the droppable that is sent as part|tag={subject      |
|               |of the drag and drop events. This can be any |id=123}           |
|               |value, including a tuple.                    |                  |
+---------------+---------------------------------------------+------------------+
|active         |The droppable will have this CSS class added |active="draghere" |
|               |when there is an acceptable draggable being  |                  |
|               |dragged.                                     |                  |
+---------------+---------------------------------------------+------------------+
|hover          |The droppable will have this CSS class added |active="dropnow"  |
|               |when there is an acceptable draggable        |                  |
|               |hovering over it.                            |                  |
+---------------+---------------------------------------------+------------------+
|accept         |The group the droppable accepts.  See the    |accept="edges"    |
|               |group argument of the draggable. A droppable |                  |
|               |can accept multiple groups, just repeat the  |                  |
|               |accept argument.                             |                  |
+---------------+---------------------------------------------+------------------+
|delegate       |The Erlang module that will receive the drop |                  |
|               |event after a successful drop.               |                  |
+---------------+---------------------------------------------+------------------+

.. seealso:: the :ref:`scomp-draggable` tag.

