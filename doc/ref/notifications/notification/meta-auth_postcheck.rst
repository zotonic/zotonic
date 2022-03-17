.. _auth_postcheck:

auth_postcheck
^^^^^^^^^^^^^^

First for logon of user with username, called after successful password check. 


Type: 
    :ref:`notification-first`

Return: 
    'undefined' | ok | {error, Reason}

``#auth_postcheck{}`` properties:
    - id: ``m_rsc:resource_id()``
    - query_args: ``map``
