.. _edge_delete:

edge_delete
^^^^^^^^^^^

An edge has been deleted 
Note that the Context for this notification does not have the user who 
deleted the edge. 


Type: 
    :ref:`notification-notify`

Return: 
    return value is ignored

``#edge_delete{}`` properties:
    - subject_id: ``m_rsc:resource()``
    - predicate: ``atom``
    - object_id: ``m_rsc:resource()``
    - edge_id: ``pos_integer``
