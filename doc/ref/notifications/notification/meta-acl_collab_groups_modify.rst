.. _acl_collab_groups_modify:

acl_collab_groups_modify
^^^^^^^^^^^^^^^^^^^^^^^^

Modify the list of collaboration groups of an user. Called internally 
by the ACL modules when fetching the list of collaboration groups an user 
is member of. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``[ m_rsc:resource_id() ]``

``#acl_collab_groups_modify{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
    - groups: ``list``
