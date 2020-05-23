.. _dispatch_redirect:

dispatch_redirect
^^^^^^^^^^^^^^^^^

Final try for dispatch, try to match the request. 
Called when the site is known, but no match is found for the path 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, RscId::integer()}``, ``{ok, #dispatch_match{}}``, ``{ok, #dispatch_redirect{}}`` or ``undefined``

``#dispatch_redirect{}`` properties:
    - location: ``binary``
    - is_permanent: ``boolean``
