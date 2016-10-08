.. _acl_is_allowed:

acl_is_allowed
^^^^^^^^^^^^^^

Check if a user is authorized to perform an operation on a an object 
(some resource or module). Observe this notification to do complex or more 
fine-grained authorization checks than you can do through the ACL rules admin 
interface. Defaults to ``false``. 


Type: 
    :ref:`notification-first`

Return: 
    ``true`` to allow the operation, ``false`` to deny it or ``undefined`` to let the next observer decide

``#acl_is_allowed{}`` properties:
    - action: ``view|update|delete|insert|use|atom``
    - object: ``term``
