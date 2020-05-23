.. _auth_checked:

auth_checked
^^^^^^^^^^^^

Fold over the context after logon of user with username, communicates valid or invalid password 


Type: 
    :ref:`notification-first`

Return: 
    

``#auth_checked{}`` properties:
    - id: ``undefined|m_rsc:resource_id()``
    - username: ``binary``
    - is_accepted: ``boolean``
