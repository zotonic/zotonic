
.. include:: meta-edge.rst

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



.. seealso:: :ref:`model-rsc`, :ref:`model-media`

