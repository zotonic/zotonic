.. _signup:

signup
^^^^^^

Request a signup of a new or existing user. Arguments are similar to #signup_url{} 
Returns {ok, UserId} or {error, Reason} 


Type: 
    :ref:`notification-first`

Return: 
    

``#signup{}`` properties:
    - id: ``integer``
    - props: ``list``
    - signup_props: ``list``
    - request_confirm: ``boolean``
