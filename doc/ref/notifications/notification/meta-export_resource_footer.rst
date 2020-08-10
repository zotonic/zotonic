.. _export_resource_footer:

export_resource_footer
^^^^^^^^^^^^^^^^^^^^^^

mod_export - Fetch the footer for the export. Should cleanup the continuation state, if needed. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, binary()}`` or ``{error, Reason}``

``#export_resource_footer{}`` properties:
    - dispatch: ``atom``
    - id: ``m_rsc:resource_id()|undefined``
    - content_type: ``binary``
    - state: ``term``
