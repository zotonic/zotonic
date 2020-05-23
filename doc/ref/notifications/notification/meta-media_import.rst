.. _media_import:

media_import
^^^^^^^^^^^^

Notification for fetching #media_import_props{} from different modules. 
This is used by z_media_import.erl for fetching properties and medium information (map) 
about resources. 


Type: 
    :ref:`notification-map`

Return: 
    

``#media_import{}`` properties:
    - url: ``binary``
    - host_rev: ``list``
    - mime: ``binary``
    - metadata: ``tuple``
