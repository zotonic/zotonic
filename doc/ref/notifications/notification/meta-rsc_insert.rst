.. _rsc_insert:

rsc_insert
^^^^^^^^^^

Foldr for an resource insert, these are the initial properties and will overrule 
the properties in the insert request. Use with care.  The props are the properties of 
the later insert, after escaping/filtering but before the #rsc_update{} notification below. 


Type: 
    :ref:`notification-foldr`

Return: 
    proplist accumulator

``#rsc_insert{}`` properties:
    - props: ``m_rsc:props()``
