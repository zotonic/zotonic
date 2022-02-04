.. _media_upload_rsc_props:

media_upload_rsc_props
^^^^^^^^^^^^^^^^^^^^^^

Notification that a medium file has been uploaded. 
This is the moment to change resource properties, modify the file etc. 
The folded accumulator is the map with updated resource properties. 


Type: 
    :ref:`notification-foldl`

Return: 
    modified resource properties map

``#media_upload_rsc_props{}`` properties:
    - id: ``m_rsc:resource_id()|insert_rsc``
    - mime: ``binary``
    - archive_file: ``unknown``
    - options: ``list``
    - medium: ``z_media_identify:media_info()``
