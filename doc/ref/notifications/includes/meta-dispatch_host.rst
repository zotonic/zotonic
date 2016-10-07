.. _dispatch_host:

dispatch_host
^^^^^^^^^^^^^

Try to find the site for the request 
Called when the request Host doesn't match any active site. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, #dispatch_redirect{}}`` or ``undefined``

``#dispatch_host{}`` properties:
    - host: ``binary``
    - path: ``binary``
    - method: ``binary``
    - protocol: ``union``
