.. _identity_verification:

identity_verification
^^^^^^^^^^^^^^^^^^^^^

Request to send a verification to the user. Return ok or an error. 
Handled by mod_signup to send out verification emails. 
Identity may be undefined, or is an identity used for the verification. 


Type: 
    :ref:`notification-first`

Return: 
    

``#identity_verification{}`` properties:
    - user_id: ``m_rsc:resource_id()``
    - identity: ``undefined|m_identity:identity()``
