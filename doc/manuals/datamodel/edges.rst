.. _manual-datamodel-edges:

Edges
=====

An :term:`edge` is a labeled connection between two resources.

The ``edge`` table defines these relations between resources. It does
this by adding a directed edge from one rsc record (:index:`subject`)
to another (:index:`object`). It also adds a reference to the
:term:`predicate`: the label of the edge.

In the admin, edges are represented in the "Page connections" sidebar
panel, of the edit page of the `subject`: the resource where the edges
originate. By convention, edges are said to `belong` to their subject.
This is to simplify the access control: if you are allowed to edit the
resource, you're also allowed to edit its `outgoing` edges ("Page
connections" in the admin), creating connections to other resources.

.. seealso:: :ref:`model-edge`

.. _manual-datamodel-edge-predicates:
             
Predicate
---------

Edges have a label: like in :ref:`manual-datamodel-example`, `author`
is a :term:`predicate` of an edge which denotes that a certain
`article` was written by a certain `person`

Just like categories, these predicates are themselves also resources:
allowing you to specify metadata, give them a meaningful title, et
cetera.

Each predicate has a list of valid subject categories and valid object
categories (stored in the ``predicate_category`` table). This is used
to filter the list of predicates in the admin edit page, and also to
filter the list of found potential objects when making a
connection. On their edit page in the admin interface, you can edit
the list of valid subject and object categories for a predicate.

.. seealso:: :ref:`model-predicate`
