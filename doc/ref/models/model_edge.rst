
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

+----------+---------------------------------------+----------------------------+
|Property  |Description                            |Example                     |
|          |                                       |value                       |
+==========+=======================================+============================+
|o         |Returns a function that accepts a page |[{204,13},{510,14},{508,15}]|
|          |id and a predicate. The end result is a|                            |
|          |list of tuples {PageId, EdgeId} which  |                            |
|          |are objects of the page.  Example      |                            |
|          |usage: ``m.edge.o[id].author``         |                            |
+----------+---------------------------------------+----------------------------+
|s         |Identical to the “o” property, except  |                            |
|          |that this function returns the subject |                            |
|          |edges.                                 |                            |
+----------+---------------------------------------+----------------------------+

.. seealso:: :ref:`model-rsc`, :ref:`model-media`

