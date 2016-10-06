.. _signup_failed_url:

signup_failed_url
^^^^^^^^^^^^^^^^^

Signup failed, give the error page URL. Return {ok, Url} or undefined. 
Reason is returned by the signup handler for the particular signup method (username, facebook etc) 


Type: 
    :ref:`notification-first`

Return: 
    

``#signup_failed_url{}`` properties:
    - reason: ``unknown``
