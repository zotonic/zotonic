.. _mqtt_subscribe:

mqtt_subscribe
^^^^^^^^^^^^^^

Subscribe a function to an MQTT topic. 
The function will be called from a temporary process, and must be of the form: 
m:f(#emqtt_msg{}, A, Context) 


Type: 
    :ref:`notification-first`

Return: 
    

``#mqtt_subscribe{}`` properties:
    - topic: ``unknown``
    - qos: ``union``
    - mfa: ``unknown``
