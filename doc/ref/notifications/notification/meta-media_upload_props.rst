.. _media_upload_props:

media_upload_props
^^^^^^^^^^^^^^^^^^

Notification that a medium file has been uploaded. 
This is the moment to change properties, modify the file etc. 
The folded accumulator is the map with updated medium properties. 


Type: 
    :ref:`notification-foldl`

Return: 
    modified medium properties map

``#media_upload_props{}`` properties:
    - id: ``m_rsc:resource_id()|insert_rsc``
    - mime: ``binary``
    - archive_file: ``file:filename_all()|undefined``
    - options: ``list``
