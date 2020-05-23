.. _email_bounced:

email_bounced
^^^^^^^^^^^^^

Bounced e-mail notification.  The recipient is the e-mail that is bouncing. When the 
the message_nr is unknown the it is set to 'undefined'. This can happen if it is a "late bounce". 
If the recipient is defined then the Context is the depickled z_email:send/2 context. 
(notify) 


Type: 
    :ref:`notification-first`

Return: 
    

``#email_bounced{}`` properties:
    - message_nr: ``binary``
    - recipient: ``undefined|binary``
