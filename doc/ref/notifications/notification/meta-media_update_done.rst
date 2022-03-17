.. _media_update_done:

media_update_done
^^^^^^^^^^^^^^^^^

Media update done notification. action is 'insert', 'update' or 'delete' 


Type: 
    :ref:`notification-notify`

Return: 
    

``#media_update_done{}`` properties:
    - action: ``insert|update|delete``
    - id: ``m_rsc:resource_id()``
    - pre_is_a: ``list``
    - post_is_a: ``list``
    - pre_props: ``map|undefined``
    - post_props: ``map|undefined``
