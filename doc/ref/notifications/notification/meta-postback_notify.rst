.. _postback_notify:

postback_notify
^^^^^^^^^^^^^^^

Handle a javascript notification from the postback handler. The ``message`` is the the request, 
``trigger`` the id of the element which triggered the postback, and ``target`` the 
id of the element which should receive possible updates. ``#postback_notify`` is also used as an event. 


Type: 
    :ref:`notification-first`

Return: 
    ``undefined`` or ``#context{}`` with the result of the postback

``#postback_notify{}`` properties:
    - message: ``unknown``
    - trigger: ``unknown``
    - target: ``unknown``
    - data: ``unknown``
