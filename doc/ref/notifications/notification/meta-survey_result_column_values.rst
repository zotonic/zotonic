.. _survey_result_column_values:

survey_result_column_values
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Modify row with answers for export. The header columns are given and the 
values that are known are set in the folded value. The user_id is the user who 
filled in the answers for this row. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``#{ binary() => iodata() }``

``#survey_result_column_values{}`` properties:
    - id: ``m_rsc:resource_id()``
    - handler: ``binary|undefined``
    - format: ``html|text``
    - user_id: ``m_rsc:resource_id()``
    - answer: ``proplists:proplist()``
    - columns: ``list``
