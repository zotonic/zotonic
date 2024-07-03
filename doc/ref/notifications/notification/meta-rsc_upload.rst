.. _rsc_upload:

rsc_upload
^^^^^^^^^^

Upload and replace the resource with the given data. The data is in the given format. 


Type: 
    :ref:`notification-first`

Return: 
    {ok, Id} or {error, Reason}, return {error, badarg} when the data is corrupt.

``#rsc_upload{}`` properties:
    - id: ``m_rsc:resource()|undefined``
    - format: ``json|bert``
    - data: ``binary|map``
