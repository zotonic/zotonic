.. _rsc_upload:

rsc_upload
^^^^^^^^^^

Upload and replace the the resource with the given data. The data is in the given format. 
Return {ok, Id} or {error, Reason}, return {error, badarg} when the data is corrupt. 


Type: 
    :ref:`notification-first`

Return: 
    

``#rsc_upload{}`` properties:
    - id: ``unknown``
    - format: ``json|bert``
    - data: ``unknown``
