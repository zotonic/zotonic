.. _export_resource_encode:

export_resource_encode
^^^^^^^^^^^^^^^^^^^^^^

mod_export - Encode a single data element. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, binary()}``, ``{ok, binary(), ContinuationState}`` or ``{error, Reason}``

``#export_resource_encode{}`` properties:
    - dispatch: ``atom``
    - id: ``integer``
    - content_type: ``string``
    - data: ``term``
    - state: ``term``
