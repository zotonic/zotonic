.. _rsc_merge:

rsc_merge
^^^^^^^^^

Map to signal merging two resources. Move any information from the looser to the 
winner. The looser will be deleted. 


Type: 
    :ref:`notification-first`

Return: 
    

``#rsc_merge{}`` properties:
    - winner_id: ``integer``
    - looser_id: ``integer``
