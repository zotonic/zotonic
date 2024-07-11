.. _identity_verified:

identity_verified
^^^^^^^^^^^^^^^^^

Notify that a user's identity has been verified. Signals to modules 
handling identities to mark this identity as verified. Handled by mod_admin_identity 
to call the m_identity model for this type/key. 


Type: 
    :ref:`notification-notify`

Return: 
    

``#identity_verified{}`` properties:
    - user_id: ``m_rsc:resource_id()``
    - type: ``m_identity:type()``
    - key: ``m_identity:key()``
