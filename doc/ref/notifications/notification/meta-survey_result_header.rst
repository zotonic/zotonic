.. _survey_result_header:

survey_result_header
^^^^^^^^^^^^^^^^^^^^

Modify header column for export. The values are the names of the answers and 
the text displayed above the column. The ``export`` view is for a complete export, the 
``summary`` view is for the limited result overview. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``list( {binary(), binary() | #trans{} )``

``#survey_result_header{}`` properties:
    - id: ``m_rsc:resource_id()``
    - handler: ``binary|undefined``
    - view: ``export|summary``
