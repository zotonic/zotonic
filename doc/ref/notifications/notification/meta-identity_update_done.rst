.. _identity_update_done:

identity_update_done
^^^^^^^^^^^^^^^^^^^^

Notify that a user's identity has been updated by the identity model. 


Type: 
    :ref:`notification-notify`

Return: 
    

``#identity_update_done{}`` properties:
    - action: ``insert|update|delete|verify``
    - rsc_id: ``m_rsc:resource_id()``
    - type: ``binary``
    - key: ``m_identity:key()|undefined``
    - is_verified: ``boolean|undefined``
