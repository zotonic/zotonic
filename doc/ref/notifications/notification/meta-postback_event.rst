.. _postback_event:

postback_event
^^^^^^^^^^^^^^

Message sent by a user-agent on a postback event. Encapsulates the encoded postback and any 
additional data. This is handled by z_transport.erl, which will call the correct event/2 functions. 


Type: 
    :ref:`notification-first`

Return: 
    

``#postback_event{}`` properties:
    - postback: ``unknown``
    - trigger: ``unknown``
    - target: ``unknown``
    - triggervalue: ``unknown``
    - data: ``unknown``
