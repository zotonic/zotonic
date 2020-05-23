.. _hierarchy_updated:

hierarchy_updated
^^^^^^^^^^^^^^^^^

Signal that the hierarchy underneath a resource has been changed by mod_menu 


Type: 
    :ref:`notification-notify`

Return: 
    

``#hierarchy_updated{}`` properties:
    - root_id: ``binary|integer``
    - predicate: ``atom``
    - inserted_ids: ``list``
    - deleted_ids: ``list``
