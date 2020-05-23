.. _acl_is_owner:

acl_is_owner
^^^^^^^^^^^^

Check if a user is the owner of a resource. 
``id`` is the resource id. 


Type: 
    :ref:`notification-first`

Return: 
    ``true``, ``false`` or ``undefined`` to let the next observer decide

``#acl_is_owner{}`` properties:
    - id: ``integer``
    - creator_id: ``integer``
    - user_id: ``integer``
