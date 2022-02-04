.. _email_send_encoded:

email_send_encoded
^^^^^^^^^^^^^^^^^^

Add a handler for receiving e-mail notifications 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, LocalFrom}``, the unique localpart of an e-mail address on this server.

``#email_send_encoded{}`` properties:
    - message_nr: ``binary``
    - from: ``binary``
    - to: ``binary``
    - encoded: ``binary``
    - options: ``gen_smtp_client:options()``
