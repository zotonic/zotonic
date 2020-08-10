.. _auth_precheck:

auth_precheck
^^^^^^^^^^^^^

First for logon of user with username, check for ratelimit, blocks etc. 
     Returns: 'undefined' | ok | {error, Reason} 


Type: 
    :ref:`notification-first`

Return: 
    

``#auth_precheck{}`` properties:
    - username: ``binary``
