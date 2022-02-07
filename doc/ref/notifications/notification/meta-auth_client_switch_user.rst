.. _auth_client_switch_user:

auth_client_switch_user
^^^^^^^^^^^^^^^^^^^^^^^

Send a request to the client to switch users. The zotonic.auth.worker.js will 
     send a request to controller_authentication to perform the switch. 


Type: 
    :ref:`notification-first`

Return: 
    ``ok | {error, term()}``

``#auth_client_switch_user{}`` properties:
    - user_id: ``m_rsc:resource_id()``
