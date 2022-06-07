.. _middleware:

middleware
^^^^^^^^^^

Delegates processing before the request, or when the request is welformed or handled.
Handy, e.g., to include personal headers to the request.


Type:
    :ref:`notification-foldl`

Return:
    ``z:context()``

``#middleware{}`` properties:
    - on: ``request|welformed|handled``
