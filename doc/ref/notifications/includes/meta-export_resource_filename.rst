.. _export_resource_filename:

export_resource_filename
^^^^^^^^^^^^^^^^^^^^^^^^

mod_export - return the {ok, Filename} for the content disposition. 


Type: 
    :ref:`notification-first`

Return: 
    ``{ok, Filename}}`` or ``undefined``

``#export_resource_filename{}`` properties:
    - dispatch: ``atom``
    - id: ``integer``
    - content_type: ``string``
