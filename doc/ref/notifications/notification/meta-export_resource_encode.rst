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
    - id: ``m_rsc:resource_id()|undefined``
    - content_type: ``binary``
    - data: ``term``
    - state: ``term``
