.. _media_stillimage:

media_stillimage
^^^^^^^^^^^^^^^^

See if there is a 'still' image preview of a media item. (eg posterframe of a movie) 
Return:: ``{ok, ResourceId}`` or ``undefined`` 


Type: 
    :ref:`notification-first`

Return: 
    

``#media_stillimage{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
    - props: ``z_media_identify:media_info()``
