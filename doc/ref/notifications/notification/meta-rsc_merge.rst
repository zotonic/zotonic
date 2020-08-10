.. _rsc_merge:

rsc_merge
^^^^^^^^^

Map to signal merging two resources. Move any information from the loser to the 
winner. The loser will be deleted. 


Type: 
    :ref:`notification-map`

Return: 
    

``#rsc_merge{}`` properties:
    - winner_id: ``m_rsc:resource_id()``
    - loser_id: ``m_rsc:resource_id()``
    - is_merge_trans: ``boolean``
