.. _media_upload_rsc_props:

media_upload_rsc_props
^^^^^^^^^^^^^^^^^^^^^^

Notification that a medium file has been uploaded. 
This is the moment to change resource properties, modify the file etc. 


Type: 
    :ref:`notification-foldl`

Return: 
    modified ``#media_upload_rsc_props{}``

``#media_upload_rsc_props{}`` properties:
    - id: ``integer|insert_rsc``
    - mime: ``binary``
    - archive_file: ``unknown``
    - options: ``list``
    - medium: ``list``
