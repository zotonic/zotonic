.. _acl_user_groups_modify:

acl_user_groups_modify
^^^^^^^^^^^^^^^^^^^^^^

Modify the list of user groups of an user. Called internally 
by the ACL modules when fetching the list of user groups an user 
is member of. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``[ m_rsc:resource_id() ]``

``#acl_user_groups_modify{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
    - groups: ``list``
