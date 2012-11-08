
.. include:: meta-rsc_gone.rst

This model tracks deleted resources (see :ref:`model-rsc`). Its 
primary goal is to be able to determine if a resource never existed, 
has been deleted or has been replaced by another resource.

Information kept
----------------

Only very basic information of the deleted resource is kept in the ``rsc_gone``
table. It is enough for referring to a new location, giving correct errors or
to determine who deleted a resource.

It is not enough to undelete a resource. The module :ref:`mod_backup` retains enough
information about past versions to be able to undelete a resource. Currently there
is no support for an undelete.

Properties
----------

Whenever a :ref:`model-rsc` record is deleted some information from 
that resource is copied to the ``rsc_gone`` table.

The following properties are saved:

+-------------------+-----------------------------------------------------+--------------------------------+
|Property           |Description                                          |Example value                   |
+===================+=====================================================+================================+
|id                 |Id of the resource, an integer.                      |42                              |
+-------------------+-----------------------------------------------------+--------------------------------+
|new_id             |If the resource is replaced by another resource then |341                             |
|                   |this is the id of that other resource.               |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|new_uri            |If the resource is moved to another place on the     |<<"http://example.com/hello">>  |
|                   |web then this is the uri of the new location.        |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|name               |The name (if any) of the deleted resource.           |<<"page_hello">>                |
+-------------------+-----------------------------------------------------+--------------------------------+
|uri                |The uri of the authoritative source for the resource.|<<"http://foo.bar/hello">>      |
+-------------------+-----------------------------------------------------+--------------------------------+
|page_path          |The page path (if any) of the deleted resource.      |<<"/hello">>                    |
+-------------------+-----------------------------------------------------+--------------------------------+
|is_authoritative   |Whether the resource originated on this site or was  |true                            |
|                   |imported and maintained on another site. Return a    |                                |
|                   |boolean.                                             |                                |
+-------------------+-----------------------------------------------------+--------------------------------+
|creator_id         |The id of the creator of the deleted resource.       |1                               |
+-------------------+-----------------------------------------------------+--------------------------------+
|created            |The date the deleted resource was created.           |{{2008,12,10},{15,30,00}}       |
+-------------------+-----------------------------------------------------+--------------------------------+
|modifier_id        |The id of the user that deleted the resource.        |2718                            |
+-------------------+-----------------------------------------------------+--------------------------------+
|modified           |The date the resource was deleted.                   |{{2012,12,5},{23,59,59}}        |
+-------------------+-----------------------------------------------------+--------------------------------+

.. seealso:: :ref:`manual-datamodel`, :ref:`model-rsc`
