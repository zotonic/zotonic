.. _multiupload:

multiupload
^^^^^^^^^^^

Handle an uploaded file which is part of a multiple file upload from a user-agent. 
The upload is a #upload record or a filename on the server. 


Type: 
    :ref:`notification-first`

Return: 
    ``#context{}`` with the result or ``undefined``

``#multiupload{}`` properties:
    - upload: ``term|string``
    - query_args: ``list``
