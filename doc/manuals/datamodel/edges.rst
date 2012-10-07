.. _manual-datamodel-edges:
Edges
-----

Like stated, an :term:`edge` is a labelled connection between two
resources.

The ``edge`` table defines these relations between resources. It does
this by adding a directed edge from one rsc record (:index:`subject`)
to another (:index:`object`). It also adds a reference to the
:term:`predicate`: the label of the edge.

In the admin, edges are represented in the "Page connections" sidebar
panel, of the edit page of the `subject`: the resource where the edges
originate. By convention, edges are said to `belong` to their subject,
this to simplify the access control. So if you are allowed to edit the
resource, you're also allowed to edit its `outgoing` edges ("Page
connections" in the admin), creating connections to other resources.

.. seealso:: :ref:`model-edge`

Predicate
.........

Edges have a label: like in the example above, `author` is a
:term:`predicate` of an edge which denotes that a certain `article`
was written by a certain `person`

Like categories, these predicates themselves are also again resources:
allowing you to specify metadata, give them a meaningful title, et
cetera.

Each predicate has a list of valid subject categories and valid object
categories (stored in the ``predicate_category`` table). This is used
to filter the list of predicates in the admin edit page, and also to
filter the list of found potential objects when making a
connection. On their edit page in the admin interface, you can edit
the list of valid subject- and object categories for a predicate.

.. seealso:: :ref:`model-predicate`



