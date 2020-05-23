.. _activity:

activity
^^^^^^^^

An activity in Zotonic. When this is handled as a notification then return a list 
of patterns matching this activity.  These patterns are then used to find interested 
subscribers. 


Type: 
    :ref:`notification-map`

Return: 
    

``#activity{}`` properties:
    - version: ``pos_integer``
    - posted_time: ``unknown``
    - actor: ``unknown``
    - verb: ``atom``
    - object: ``unknown``
    - target: ``unknown``
