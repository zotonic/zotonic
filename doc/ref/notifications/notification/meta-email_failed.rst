.. _email_failed:

email_failed
^^^^^^^^^^^^

Notify that we could NOT send an e-mail (there might be a bounce later...) 
The Context is the depickled z_email:send/2 context. 


Type: 
    :ref:`notification-notify`

Return: 
    

``#email_failed{}`` properties:
    - message_nr: ``binary``
    - recipient: ``binary``
    - is_final: ``boolean``
    - reason: ``retry|illegal_address|smtphost|error``
    - status: ``binary``
