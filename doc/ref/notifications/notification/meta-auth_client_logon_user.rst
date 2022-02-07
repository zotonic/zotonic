.. _auth_client_logon_user:

auth_client_logon_user
^^^^^^^^^^^^^^^^^^^^^^

Send a request to the client to login an user. The zotonic.auth.worker.js will 
     send a request to controller_authentication to exchange the one time token with 
     a z.auth cookie for the given user. The client will redirect to the Url. 


Type: 
    :ref:`notification-first`

Return: 
    ``ok | {error, term()}``

``#auth_client_logon_user{}`` properties:
    - user_id: ``m_rsc:resource_id()``
    - url: ``union``
