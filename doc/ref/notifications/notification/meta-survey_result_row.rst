.. _survey_result_row:

survey_result_row
^^^^^^^^^^^^^^^^^

Modify row with answers for export. The header columns are given and the 
values that are known are set in the folded value. The user_id is the user who 
filled in the answers for this row. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``#{ binary() => term() }``

``#survey_result_row{}`` properties:
    - id: ``m_rsc:resource_id()``
    - handler: ``binary|undefined``
    - view: ``export|result_view``
    - user_id: ``m_rsc:resource_id()``
    - columns: ``list``
