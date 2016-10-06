.. _acl_is_allowed_prop:

acl_is_allowed_prop
^^^^^^^^^^^^^^^^^^^

Check if an action on a property is allowed. 


Type: 
    :ref:`notification-first`

Return: 
    ``true`` to grant access, ``false`` to deny it, ``undefined`` to let the next observer decide

``#acl_is_allowed_prop{}`` properties:
    - action: ``view|update|delete|insert|atom``
    - object: ``term``
    - prop: ``atom``
