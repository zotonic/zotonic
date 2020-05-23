.. _auth_postcheck:

auth_postcheck
^^^^^^^^^^^^^^

First for logon of user with username, called after successful password check. 
     Returns: 'undefined' | ok | {error, Reason} 


Type: 
    :ref:`notification-first`

Return: 
    

``#auth_postcheck{}`` properties:
    - id: ``m_rsc:resource_id()``
    - query_args: ``list``
