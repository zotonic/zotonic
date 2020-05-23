.. _export_resource_content_disposition:

export_resource_content_disposition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

mod_export - return the {ok, Disposition} for the content disposition. 


Type: 
    :ref:`notification-first`

Return: 
    {ok, <<"inline">>} or {ok, <<"attachment">>}

``#export_resource_content_disposition{}`` properties:
    - dispatch: ``atom``
    - id: ``m_rsc:resource_id()|undefined``
    - content_type: ``binary``
