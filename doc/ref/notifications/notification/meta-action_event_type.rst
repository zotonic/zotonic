.. _action_event_type:

action_event_type
^^^^^^^^^^^^^^^^^

Render the javascript for a custom action event type. 
The custom event type must be a tuple, for example: 
<code>{% wire type={live id=myid} action={...} %}</code> 
Must return {ok, Javascript, Context} 


Type: 
    :ref:`notification-first`

Return: 
    

``#action_event_type{}`` properties:
    - event: ``tuple``
    - trigger_id: ``string``
    - trigger: ``string``
    - postback_js: ``iolist``
    - postback_pickled: ``string|binary``
    - action_js: ``iolist``
