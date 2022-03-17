.. _media_import_medium:

media_import_medium
^^^^^^^^^^^^^^^^^^^

Notification to import a medium record from external source. This is called for non-file 
medium records, for example embedded video.  If the medium record is not recognized then it 
will not be imported. The handling module is responsible for sanitizing and inserting the medium 
record. 


Type: 
    :ref:`notification-first`

Return: 
    ``ok | {error, term()}``.

``#media_import_medium{}`` properties:
    - id: ``m_rsc:resource_id()``
    - medium: ``map``
