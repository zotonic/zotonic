.. _request_context:

request_context
^^^^^^^^^^^^^^^

Refresh the context or request process for the given request or action 
     Called for every request that is not anoymous and before every MQTT relay from 
     the client.  Example: mod_development uses this to set flags in the process 
     dictionary. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``#context{}``

``#request_context{}`` properties:
    - phase: ``union``
    - document: ``map``
