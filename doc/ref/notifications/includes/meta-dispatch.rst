.. _dispatch:

dispatch
^^^^^^^^

Final try for dispatch, try to match the request. 
Called when the site is known, but no match is found for the path 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, RscId::integer()}``, ``{ok, #dispatch_match{}}``, ``{ok, #dispatch_redirect{}}`` or ``undefined``

``#dispatch{}`` properties:
    - host: ``binary``
    - path: ``binary``
    - method: ``binary``
    - protocol: ``union``
    - tracer_pid: ``union``
