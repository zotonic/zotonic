.. _auth_identity_types:

auth_identity_types
^^^^^^^^^^^^^^^^^^^

Return the list of identity types that allow somebody to logon and become an 
active user of the system. Defaults to [ username_pw ].  In the future more types 
can be requested, think of 'contact' - to be able to contact someone. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``[ atom() ]``

``#auth_identity_types{}`` properties:
    - unknown: ``unknown``
