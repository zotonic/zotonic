.. _resource_headers:

resource_headers
^^^^^^^^^^^^^^^^

Let all modules add resource specific response headers to the request. 
The accumulator is the list of headers to be set. 


Type: 
    :ref:`notification-foldl`

Return: 
    ``list( {binary(), binary()} )``

``#resource_headers{}`` properties:
    - id: ``m_rsc:resource_id()|undefined``
