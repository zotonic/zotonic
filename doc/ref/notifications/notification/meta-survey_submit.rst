.. _survey_submit:

survey_submit
^^^^^^^^^^^^^

A survey has been filled in and submitted. 


Type: 
    :ref:`notification-first`

Return: 
    ``undefined``, ``ok``, ``{ok, Context | #render{}}``, ``{save, Context | #render{}`` or ``{error, term()}``

``#survey_submit{}`` properties:
    - id: ``m_rsc:resource_id()``
    - handler: ``binary|undefined``
    - answers: ``list``
    - missing: ``list``
    - answers_raw: ``list``
    - submit_args: ``proplists:proplist()``
