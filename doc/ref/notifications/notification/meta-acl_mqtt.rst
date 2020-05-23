.. _acl_mqtt:

acl_mqtt
^^^^^^^^

MQTT acl check, called via the normal acl notifications. 
Actions for these checks: subscribe, publish 


Type: 
    :ref:`notification-first`

Return: 
    

``#acl_mqtt{}`` properties:
    - type: ``wildcard|direct``
    - topic: ``binary``
    - words: ``list``
    - site: ``binary``
    - page_id: ``undefined|binary``
