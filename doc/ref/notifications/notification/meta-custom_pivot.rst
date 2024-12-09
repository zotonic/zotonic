.. _custom_pivot:

custom_pivot
^^^^^^^^^^^^

Add custom pivot fields to a resource's search index (map) 
Result is a single tuple or list of tuples ``{pivotname, props}``, where "pivotname" 
is the pivot defined in a call to ``z_pivot_rsc:define_custom_pivot/3`` or a table 
with created using a SQL command during (eg.) in a module ``manage_schema/2`` call. 
The name of the table is ``pivot_<pivotname>``.  The ``props`` is either a property 
list or a map with column/value pairs. 
 
The table MUST have an ``id`` column, with a foreign key constraint to the ``rsc`` 
table. If you define the pivot table using ``z_pivot_rsc:define_custom_pivot/3`` then 
this column and foreign key constraint are automatically added. 


Type: 
    :ref:`notification-map`

Return: 
    

``#custom_pivot{}`` properties:
    - id: ``m_rsc:resource_id()``
