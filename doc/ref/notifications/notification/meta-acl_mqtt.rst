.. _acl_mqtt:

acl_mqtt
^^^^^^^^

MQTT acl check, called via the normal acl notifications. 
Actions for these checks: subscribe, publish 


Type: 
    :ref:`notification-first`

Return: 
    

``#acl_mqtt{}`` properties:
    - topic: ``list``
    - is_wildcard: ``boolean``
    - packet: ``mqtt_packet_map:mqtt_packet()``
