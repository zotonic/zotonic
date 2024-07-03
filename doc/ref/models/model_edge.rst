.. include:: meta-edge.rst
.. seealso:: :ref:`model-rsc`, :ref:`model-media`

Access information about page connections.

Edges represent the connections between resources. They are
implemented as tuples ``{EdgeId, SubjectId, PredicateId, ObjectId,
OrderNr}``.  The edge id is a unique id representing the edge, it can
be used with edit actions. The OrderNr defines the order of the edges
`with respect to the subject`.

Most edge information is accessed using the :ref:`model-rsc`
model, but some information can only accessed with the m_edge model.

This model implements two template accessible options. They are mainly
used to obtain the edge’s id for edit pages.

The following m_edge model properties are available in templates:

+----------+---------------------------------------------------+----------------------------+
|Property  |Description                                        |Example value               |
+==========+===================================================+============================+
|o         |Returns a function that accepts a page             |``[{204,13},{510,14},       |
|          |                                                   |{508,15}]``                 |
|          |id and a predicate. The end result is a            |                            |
|          |list of tuples {PageId, EdgeId} which              |                            |
|          |are objects of the page.  Example                  |                            |
|          |usage: ``m.edge.o[id].author``                     |                            |
+----------+---------------------------------------------------+----------------------------+
|s         |Identical to the “o” property, except              |                            |
|          |that this function returns the subject             |                            |
|          |edges.                                             |                            |
+----------+---------------------------------------------------+----------------------------+
|o_props   |Similar to ``m.edge.o[id].author`` above, but      |.. code-block:: none        |
|          |returns a property list for the edges instead of   |                            |
|          |the 2-tuple.                                       |    [                       |
|          |                                                   |        {id, 86062},        |
|          |                                                   |        {subject_id, 10635},|
|          |                                                   |        {predicate_id, 304},|
|          |                                                   |        {object_id, 57577}, |
|          |                                                   |        {seq, 1},           |
|          |                                                   |        {creator_id, 1},    |
|          |                                                   |        {created, {         |
|          |                                                   |            {2015,11,17},   |
|          |                                                   |            {11,23,32}      |
|          |                                                   |        }}                  |
|          |                                                   |    ]                       |
|          |                                                   |    ]                       |
+----------+---------------------------------------------------+----------------------------+
|s_props   |Similar to ``m.edge.s[id].author`` above, but      |                            |
|          |returns a property list for the edges instead of   |                            |
|          |the 2-tuple.                                       |                            |
+----------+---------------------------------------------------+----------------------------+
|edges     |Returns a function that accepts a page             |See example below.          |
|          |id. The end result is a list of edges per predicate|                            |
|          |where the predicate is an atom and the edges are   |                            |
|          |property lists. Example usage: ``m.edge[10635]``   |                            |
+----------+---------------------------------------------------+----------------------------+
|id        |Look up an edge id by a                            |213                         |
|          |subject/predicate/object triple. Example usage::   |                            |
|          |                                                   |                            |
|          |  m.edge.id[subject_id].relation[object_id]        |                            |
|          |                                                   |                            |
|          |or::                                               |                            |
|          |                                                   |                            |
|          |  m.edge.id[subject_id][predicate_name][object_id] |                            |
|          |                                                   |                            |
|          |Returns ``undefined`` if the edge does not         |                            |
|          |exist; otherwise returns an integer.               |                            |
+----------+---------------------------------------------------+----------------------------+


.. highlight:: erlang

Example return value for ``{% print m.edge[10635] %}``::

    [{about,[[{id,86060},
              {subject_id,10635},
              {predicate_id,300},
              {name,<<"about">>},
              {object_id,17433},
              {seq,1},
              {created,{{2015,11,17},{11,22,11}}},
              {creator_id,1}]]},
     {author,[[{id,6},
               {subject_id,10635},
               {predicate_id,301},
               {name,<<"author">>},
               {object_id,10634},
               {seq,1000000},
               {created,{{2015,2,3},{16,23,20}}},
               {creator_id,1}]]}]


Other Topics
------------

``model/edge/post/o/+subject/+predicate/+object`` or
``model/edge/post/s/+object/+predicate/+object`` inserts a new edge between resources.

The posted message can optionally include the name or id of the object, predicate and subject.

.. code-block:: javascript

   cotonic.broker.publish("bridge/origin/edge/post/o/4312",
                          {
                            predicate: "author",
                            subject: 7575
                          });

It is also possible to insert edges via cotonics onclick topics.

.. code-block:: django

   <div data-onclick-topic="bridge/origin/model/edge/post/o/{{ id }}/?/{{ m.acl.user }}">
      <button data-edge-predicate="is_going">Is Going</button>
      <button data-edge-predicate="is_interested">Might Go</button>
   </div>

When a user clicks on a button, the model retrieves the predicate name (or id) from the
``data-edge-predicate`` attribute. This is also possible by for the object and subject attributes
of the edge. When there is a ``?`` in the topic path, the value can be retrieved from a
data attribute. The attribute value for object is: ``data-edge-object``. For subject it is:
``data-edge-subject``.


``model/edge/post/delete/o/+subject/+predicate/+object``,
``model/edge/post/delete/s/+object/+predicate/+subject`` or
``model/edge/post/delete/edge/+edge_id`` deletes the specified edge.

.. code-block:: javascript

   cotonic.broker.publish("bridge/origin/edge/delete/edge/6776");

Or via a onclick topic.

.. code-block:: django

   <button data-onclick-topic="bridge/origin/edge/delete/{{ edge_id }}">Delete</button


