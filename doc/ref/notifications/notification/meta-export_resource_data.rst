.. _export_resource_data:

export_resource_data
^^^^^^^^^^^^^^^^^^^^

mod_export - fetch a row for the export, can return a list of rows, a binary, and optionally a continuation state. 
Where Values is [ term() ], i.e. a list of opaque values, to be formatted with #export_resource_format. 
Return the empty list of values to signify the end of the data stream. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, Values|binary()}``, ``{ok, Values|binary(), ContinuationState}`` or ``{error, Reason}``

``#export_resource_data{}`` properties:
    - dispatch: ``atom``
    - id: ``integer``
    - content_type: ``string``
    - state: ``term``
