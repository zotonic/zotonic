.. _media_viewer:

media_viewer
^^^^^^^^^^^^

Request to generate a HTML media viewer for a resource. The HTML data can not contain any 
Javascript, as it might be serialized. This could happen if the correct cookies are not yet 
set or if the media viewer is part of a direct DOM update. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, Html}`` or ``undefined``

``#media_viewer{}`` properties:
    - id: ``unknown``
    - props: ``z_media_identify:media_info()``
    - filename: ``union``
    - options: ``list``
