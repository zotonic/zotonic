.. _export_resource_header:

export_resource_header
^^^^^^^^^^^^^^^^^^^^^^

mod_export - Fetch the header for the export. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, list()|binary()}``, ``{ok, list()|binary(), ContinuationState}`` or ``{error, Reason}``

``#export_resource_header{}`` properties:
    - dispatch: ``atom``
    - id: ``m_rsc:resource_id()|undefined``
    - content_type: ``binary``
