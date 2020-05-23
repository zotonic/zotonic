.. _edge_update:

edge_update
^^^^^^^^^^^

An edge has been updated 
Note that the Context for this notification does not have the user who 
updated the edge. 


Type: 
    :ref:`notification-notify`

Return: 
    return value is ignored

``#edge_update{}`` properties:
    - subject_id: ``m_rsc:resource()``
    - predicate: ``atom``
    - object_id: ``m_rsc:resource()``
    - edge_id: ``pos_integer``
