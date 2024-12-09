.. _survey_result_columns:

survey_result_columns
^^^^^^^^^^^^^^^^^^^^^

Add header columns for export. The values are the names of the answers and 
the text displayed above the column. The ``text`` format is for a complete export, the 
``html`` format is for the limited result overview of the Survey Results Editor. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``list( {binary(), binary() | #trans{}} )``

``#survey_result_columns{}`` properties:
    - id: ``m_rsc:resource_id()``
    - handler: ``binary|undefined``
    - format: ``html|text``
