.. _auth_precheck:

auth_precheck
^^^^^^^^^^^^^

First for logon of user with username, check for ratelimit, blocks etc. 


Type: 
    :ref:`notification-first`

Return: 
    'undefined' | ok | {error, Reason}

``#auth_precheck{}`` properties:
    - username: ``binary``
