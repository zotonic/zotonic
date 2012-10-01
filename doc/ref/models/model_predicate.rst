
.. include:: meta-predicate.rst

Retrieve information about predicates. Predicates are the `labels` on
edges (connections between resources) that give meaning to an edge. An
example is the predicate “author” which refers to the authors of an
article. Predicates form together with the referring and the referred
page a triple ``{subject, predicate, object}``.

Each predicate has a list of valid subject categories and valid object
categories. This is used to filter the list of predicates in the admin
edit page, and also to filter the list of found potential objects when
making a connection.

A full predicate definition can be fetched by name or id with::

  {{ m.predicate.author }}
  {{ m.predicate[104] }}

Which both return a property list with information about the
predicate. The property list contains all page properties and the
properties: “pred” which is the atomic predicate name, “subject” which
is a list of valid subject categories and “object” with is a list of
valid object categories.

The following m_predicate model properties are available in templates:

+----------------+----------------------------------------+-------------------------------+
|Property        |Description                             |Example value                  |
+----------------+----------------------------------------+-------------------------------+
|all             |Return a property list of all           |``[{about,                     |
|                |predicates. Keys are the atomic         |[{pred,,about},{subject,[104]},|
|                |predicate name, values are property     |{object,[]}, {id,300}, … ]``   |
|                |lists with information about the        |                               |
|                |predicate. The property list contains   |                               |
|                |all page properties and the properties: |                               |
|                |“pred” which is the atomic predicate    |                               |
|                |name, “subject” which is a list of valid|                               |
|                |subject categories and “object” with is |                               |
|                |a list of valid object categories.      |                               |
+----------------+----------------------------------------+-------------------------------+
|object_category |Used to derive the list of valid object |``[{104}, … ]``                |
|                |categories for a predicate. Example     |                               |
|                |usage:                                  |                               |
|                |``m.predicate.object_category.author``  |                               |
|                |Note: Each id is a 1-tuple.             |                               |
+----------------+----------------------------------------+-------------------------------+
|subject_category|Used to derive the list of valid subject|``[{674}, … ]``                |
|                |categories for a predicate. Example     |                               |
|                |usage:                                  |                               |
|                |``m.predicate.subject_category.author`` |                               |
|                |Note: Each id is a 1-tuple.             |                               |
+----------------+----------------------------------------+-------------------------------+

