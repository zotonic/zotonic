.. _rsc_update:

rsc_update
^^^^^^^^^^

An updated resource is about to be persisted. 
Observe this notification to change the resource properties before they are 
persisted. 
 
The props are the resource's props _before_ the update, but _after_ filtering 
and sanitization. The folded value is ``{ok, UpdateProps}`` for the update itself. 


Type: 
    :ref:`notification-foldr`

Return: 
    ``{ok, UpdateProps}`` or ``{error, term()}``

``#rsc_update{}`` properties:
    - action: ``insert|update``
    - id: ``m_rsc:resource_id()``
    - props: ``m_rsc:props()``
