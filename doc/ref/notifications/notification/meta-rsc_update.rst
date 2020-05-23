.. _rsc_update:

rsc_update
^^^^^^^^^^

An updated resource is about to be persisted. 
Observe this notification to change the resource properties before they are 
persisted. 
The props are the resource's props _before_ the update. 
The folded value is {IsChanged, UpdateProps} for the update itself. 
Set IsChanged to true if you modify the UpdateProps. 


Type: 
    :ref:`notification-foldr`

Return: 
    ``{true, ChangedProps}`` or ``{false, Props}``

``#rsc_update{}`` properties:
    - action: ``insert|update``
    - id: ``m_rsc:resource()``
    - props: ``list``
