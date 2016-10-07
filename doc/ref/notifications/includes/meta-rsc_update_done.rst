.. _rsc_update_done:

rsc_update_done
^^^^^^^^^^^^^^^

An updated resource has just been persisted. Observe this notification to 
execute follow-up actions for a resource update. 


Type: 
    :ref:`notification-notify`

Return: 
    return value is ignored

``#rsc_update_done{}`` properties:
    - action: ``insert|update|delete``
    - id: ``m_rsc:resource()``
    - pre_is_a: ``list``
    - post_is_a: ``list``
    - pre_props: ``list``
    - post_props: ``list``
