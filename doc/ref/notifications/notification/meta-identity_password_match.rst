.. _identity_password_match:

identity_password_match
^^^^^^^^^^^^^^^^^^^^^^^

Check if passwords are matching. Uses the password hashing algorithms. 


Type: 
    :ref:`notification-first`

Return: 
    

``#identity_password_match{}`` properties:
    - rsc_id: ``m_rsc:resource_id()|undefined``
    - password: ``binary``
    - hash: ``m_identity:hash()|tuple``
